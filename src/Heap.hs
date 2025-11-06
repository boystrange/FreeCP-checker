module Graph (Type(..), HeapT, fold, unfold, node, set, defined, undefined) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

newtype Ptr = Ptr Int

data Type
    = Ref Bool Ptr
    | Bot
    | One
    | Skip
    | Seq Type Type
    | Mul Type Type
    | Par Type Type
    | With [(Label, (Measure, Type))]
    | Plus [(Label, (Measure, Type))]

dual :: Type -> Type
dual (Ref d p) = Ref (not d) p
dual Bot       = One
dual One       = Bot
dual Skip      = Skip
dual (Seq t s) = Seq (dual t) (dual s)
dual (Mul t s) = Par (dual t) (dual s)
dual (Par t s) = Mul (dual t) (dual s)
dual (With bs) = Plus (map (\(l, (m, t)) -> (l, (m, dual t))) bs)
dual (Plus bs) = With (map (\(l, (m, t)) -> (l, (m, dual t))) bs)

type Store = Map Ptr Type
type HeapT = StateT (Ptr, Store)

new :: HeapT Ptr
new = do
    (next, store) <- State.get
    State.put (succ next, store)
    return next

ref :: Type -> HeapT Ptr
ref t = do
    p <- new
    set p t
    return p

set :: Ptr -> Type -> HeapT ()
set p t = getHeap >>= setHeap . Map.insert p t

get :: Ptr -> HeapT (Maybe Type)
get p = Map.lookup p <$> getStore

deref :: Ptr -> HeapT Type
deref p = do
    mt <- get p
    case mt of
        Nothing -> error "dereferencing unset pointer"
        Just t  -> return t

find :: Type -> HeapT Type
find t@(Ref d p) = do
    mt <- deref p
    case mt of
        Nothing -> return t
        Just s -> find (if d then dual s else s)
find t = t

getStore :: HeapT Store
getStore = do
    (_, heap) <- State.get
    return heap

setStore :: Store -> HeapT ()
setStore heap = do
    (p, _) <- State.get
    State.set (p, heap)

fold :: ((Ptr -> HeapT a) -> Type -> HeapT a) -> a -> Type -> HeapT a
fold f a = f (ptr Set.empty)
    where
        ptr pset p | Set.member p pset = return a
        ptr pset p = do
            t <- deref p
            f (ptr (Set.insert p pset)) t

complete :: Type -> HeapT Bool
complete = fold aux True
    where
        aux :: (Ptr -> HeapT Bool) -> Type -> HeapT Bool
        aux go (Ref _ ptr) = go ptr
        aux _  Bot         = return True
        aux _  One         = return True
        aux _  Skip        = return False
        aux _  (Poly _ _)  = return True
        aux go (Seq t s)   = do
            tc <- aux go t
            sc <- aux go s
            return (tc || sc)
        aux go (Mul _ s)   = aux go s
        aux go (Par _ s)   = aux go s
        aux go (Plus bs)   = and <$> mapM (auxB go) vs
        aux go (With bs)   = and <$> mapM (auxB go) vs

        auxB :: (Ptr -> HeapT Bool) -> (Label, (Measure, Type)) -> HeapT Bool
        auxB go (_, (_, t)) = aux go t

partial :: Type -> HeapT Bool
partial t = not <$> complete t

type TypeDef = (TypeName, ([TypeName], Ptr))

make :: [SourceTypeDef] -> HeapT [TypeDef]
make = auxL
    where
        auxL :: [SourceTypeDef] -> HeapT [TypeDef]
        auxL tdefs = do
            ps <- mapM (const new) tdefs
            let ds = [ ((tname, targs), p) | ((tname, (targs, _)), p) <- zip tdefs ps ]
            let tmap = Map.fromList ds
            mapM (auxD tmap) ds

        auxD :: TypeMap -> ((TypeName, ([TypeName], SourceType)), Ptr) -> HeapT TypeDef
        auxD tmap ((tname, (targs, s)), p) = do
            t <- auxT tmap targs s
            set p t
            return (tname, (targs, t))

        auxT :: TypeMap -> [TypeName] -> SourceType -> HeapT Type
        auxT tmap targs S.Bot       = return Bot
        auxT tmap targs S.One       = return One
        auxT tmap targs S.Skip      = return Skip
        auxT tmap targs (S.Ref tname targs)
            | Just p <- Map.lookup (tname, targs) tmap = return $ Ref False p
            | otherwise = error "unknown type"
        auxT tmap targs (S.Poly d tname) =
            | tname `elem` targs = return $ Poly d tname
            | otherwise = error "unknown type"
        auxT (S.Seq t s) = do
            t' <- auxT tmap targs t
            s' <- auxT tmap targs s
            return $ Seq t' s'
        auxT tmap targs (S.Mul t s) = do
            t' <- auxT tmap targs t
            s' <- auxT tmap targs s
            return $ Mul t' s'
        auxT tmap targs (S.Par t s) = do
            t' <- auxT tmap targs t
            s' <- auxT tmap targs s
            return $ Par t' s'
        auxT tmap targs (S.With bs) = do
            bs' <- mapM (auxB tmap targs) bs
            return $ With bs'
        auxT tmap targs (S.Plus bs) = do
            bs' <- mapM (auxB tmap targs) bs
            return $ Plus bs'
        auxT tmap targs (S.Dual t) = dual <$> auxT tmap targs t

        auxB tmap targs (l, s) = do
            m <- newMeasure
            t <- auxT tmap targs s
            return (l, (m, t))

typeMap :: (Measure -> Measure) -> (Node -> Node) -> Type -> Type
typeMap f g Bot       = Bot
typeMap f g One       = One
typeMap f g Skip      = Skip
typeMap f g (Seq t s) = Seq (g t) (g s)
typeMap f g (Mul t s) = Mul (g t) (g s)
typeMap f g (Par t s) = Par (g t) (g s)
typeMap f g (With bs) = With (map (\(l, (m, n) -> (l, (f m, g n)))) bs)
typeMap f g (Plus bs) = Plus (map (\(l, (m, n) -> (l, (f m, g n)))) bs)

measures :: Type m n -> [m]
measures (With bs) = map (fst . snd) bs
measures (Plus bs) = map (fst . snd) bs
measures _         = []

unify :: ChannelName -> Type -> Type -> Checker ()
unify name = go Set.empty
    where
        go :: Set (Type, Type) -> Type -> Type -> Checker ()
        go vset t s | Set.member (t, s) vset = return True
        go vset t s = do
            t' <- find t
            s' <- find s
            aux (Set.insert (t', s') vset) t' s'

        aux :: Set (Type, Type) -> Type -> Type -> Checker ()
        -- POLYMORPHIC VARIABLES
        aux vset   (Ref False p)   (Ref False q) | p == q = return ()
        aux vset t@(Ref _     p) s@(Ref _     q) | p < q = auxT s t
        aux vset   (Ref False p) s               = set p s
        aux vset   (Ref True  p) s               = set p (dual s)
        aux vset t               s@(Ref False q) = set q t
        aux vset t               s@(Ref True  q) = set q (dual t)

        -- SEQUENTIAL COMPOSITION
        aux vset Skip            Skip            = return ()
        aux vset (Seq t1 t2)     (Seq s1 s2)     = do
            go vset t1 s1
            go vset t2 s2

        -- CONSTANTS
        aux vset Bot             Bot             = return ()
        aux vset One             One             = return ()

        -- CONNECTIVES
        aux vset (Mul t1 t2) (Mul s1 s2) = do
            go vset t1 s1
            go vset t2 s2
        aux vset (Par t1 t2) (Par s1 s2) = do
            go vset t1 s1
            go vset t2 s2
        aux vset (Plus bs1) (Plus bs2) = do
            let map1 = Map.fromList bs1
            let map2 = Map.fromList bs2
            sameTags (Map.keysSet map1) (Map.keysSet map2)
            forM_ (Map.elems (zipMap map1 map2)) $ \((m1, t1), (m2, t2)) -> do
                addMeasureConstraintEq m1 m2
                go vset t1 t2
        aux vset (With bs1) (With bs2) = do
            let map1 = Map.fromList bs1
            let map2 = Map.fromList bs2
            sameTags (Map.keysSet map1) (Map.keysSet map2)
            forM_ (Map.elems (zipMap map1 map2)) $ \((m1, t1), (m2, t2)) -> do
                addMeasureConstraintEq m1 m2
                go vset t1 t2

        -- TYPE MISMATCH
        aux vset t s = throw $ ErrorTypeMismatch name (show t) s

        sameTags tags1 tags2 =
            unless (tags1 == tags2) $
                throw $ ErrorLabelMismatch name (Set.elems tags1) (Set.elems tags2)

normalize :: Node -> HeapT Node
normalize = undefined

subst :: TypeName -> Type -> Type -> HeapT Type
subst tname t s = undefined

reachable :: Node -> HeapT (Set Node)
reachable = aux []
    where
        aux ns n | n `elem` ns = return Set.empty
        aux ns n = do
            t <- unfold n
            nset <- Set.unions <$> mapM (aux (n : ns)) (children t)
            return (Set.insert n nset)