-- MIT License
-- 
-- Copyright (c) 2025 Luca Padovani
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Type where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as State

import Atoms
import Measure
import qualified SourceType as S

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

newtype Ptr = Ptr Int
type Store = Map Ptr Type
type HeapT = StateT (Ptr, Store)

new :: Monad m => HeapT m Ptr
new = do
    (next, store) <- State.get
    State.put (succ next, store)
    return next

set :: Monad m => Ptr -> Type -> HeapT m ()
set p t = getHeap >>= setHeap . Map.insert p t

get :: Monad m => Ptr -> HeapT m (Maybe Type)
get p = Map.lookup p <$> getStore

ref :: Monad m => Type -> HeapT m Ptr
ref t = do
    p <- new
    set p t
    return p

deref :: Monad m => Ptr -> HeapT m Type
deref p = do
    mt <- get p
    case mt of
        Nothing -> error "dereferencing unset pointer"
        Just t  -> return t

-- find :: Type -> HeapT Type
-- find t@(Ref d p) = do
--     mt <- deref p
--     case mt of
--         Nothing -> return t
--         Just s -> find (if d then dual s else s)
-- find t = t

getStore :: Monad m => HeapT m Store
getStore = do
    (_, heap) <- State.get
    return heap

setStore :: Monad m => Store -> HeapT m ()
setStore heap = do
    (p, _) <- State.get
    State.put (p, heap)

fold :: Monad m => ((Ptr -> HeapT m a) -> Type -> HeapT m a) -> a -> Type -> HeapT m a
fold f a = f (ptr Set.empty)
    where
        ptr pset p | Set.member p pset = return a
        ptr pset p = do
            t <- deref p
            f (ptr (Set.insert p pset)) t

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

complete :: Type -> HeapT m Bool
complete = fold aux True
    where
        aux :: (Ptr -> HeapT m Bool) -> Type -> HeapT Bool
        aux go (Ref _ ptr) = go ptr
        aux _  Bot         = return True
        aux _  One         = return True
        aux _  Skip        = return False
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

partial :: Monad m => Type -> HeapT m Bool
partial t = not <$> complete t

type TypeDef = (TypeName, ([TypeName], Ptr))
type TypeMap = Map (TypeName, [TypeName]) Ptr
type TypeArgMap = Map TypeName Ptr

make :: Monad m => [S.TypeDef] -> HeapT m [TypeDef]
make = auxL
    where
        auxL :: [S.TypeDef] -> HeapT m [TypeDef]
        auxL tdefs = do
            ps <- mapM (const new) tdefs
            let ds = [ ((tname, targs), p) | ((tname, (targs, _)), p) <- zip tdefs ps ]
            let tmap = Map.fromList ds
            mapM (auxD tmap) ds

        auxD :: TypeMap -> ((TypeName, ([TypeName], S.Type)), Ptr) -> HeapT m TypeDef
        auxD tmap ((tname, (targs, s)), p) = do
            amap <- Map.fromList <$> zip targs <$> mapM (const new) targs
            t <- auxT tmap amap s
            set p t
            return (tname, (targs, t))

        auxT :: TypeMap -> TypeArgMap -> S.Type -> HeapT m Type
        auxT tmap amap S.Bot       = return Bot
        auxT tmap amap S.One       = return One
        auxT tmap amap S.Skip      = return Skip
        auxT tmap amap (S.Ref tname targs)
            | Just p <- Map.lookup (tname, targs) tmap = return $ Ref False p
            | otherwise = error "unknown type"
        auxT tmap amap (S.Poly d tname)
            | Just p <- Map.lookup tname amap = return $ Ref d p
            | otherwise = error "unknown type argument"
        auxT tmap amap (S.Seq t s) = do
            t' <- auxT tmap amap t
            s' <- auxT tmap amap s
            return $ Seq t' s'
        auxT tmap amap (S.Mul t s) = do
            t' <- auxT tmap amap t
            s' <- auxT tmap amap s
            return $ Mul t' s'
        auxT tmap amap (S.Par t s) = do
            t' <- auxT tmap amap t
            s' <- auxT tmap amap s
            return $ Par t' s'
        auxT tmap amap (S.With bs) = do
            bs' <- mapM (auxB tmap amap) bs
            return $ With bs'
        auxT tmap amap (S.Plus bs) = do
            bs' <- mapM (auxB tmap amap) bs
            return $ Plus bs'
        auxT tmap amap (S.Dual t) = dual <$> auxT tmap amap t

        auxB tmap amap (l, s) = do
            m <- newMeasure
            t <- auxT tmap amap s
            return (l, (m, t))

-- typeMap :: (Measure -> Measure) -> (Node -> Node) -> Type -> Type
-- typeMap f g Bot       = Bot
-- typeMap f g One       = One
-- typeMap f g Skip      = Skip
-- typeMap f g (Seq t s) = Seq (g t) (g s)
-- typeMap f g (Mul t s) = Mul (g t) (g s)
-- typeMap f g (Par t s) = Par (g t) (g s)
-- typeMap f g (With bs) = With (map (\(l, (m, n) -> (l, (f m, g n)))) bs)
-- typeMap f g (Plus bs) = Plus (map (\(l, (m, n) -> (l, (f m, g n)))) bs)

-- measures :: Type m n -> [m]
-- measures (With bs) = map (fst . snd) bs
-- measures (Plus bs) = map (fst . snd) bs
-- measures _         = []

-- unify :: ChannelName -> Type -> Type -> Checker ()
-- unify name = go Set.empty
--     where
--         go :: Set (Type, Type) -> Type -> Type -> Checker ()
--         go vset t s | Set.member (t, s) vset = return True
--         go vset t s = do
--             t' <- find t
--             s' <- find s
--             aux (Set.insert (t', s') vset) t' s'

--         aux :: Set (Type, Type) -> Type -> Type -> Checker ()
--         -- POLYMORPHIC VARIABLES
--         aux vset   (Ref False p)   (Ref False q) | p == q = return ()
--         aux vset t@(Ref _     p) s@(Ref _     q) | p < q = auxT s t
--         aux vset   (Ref False p) s               = set p s
--         aux vset   (Ref True  p) s               = set p (dual s)
--         aux vset t               s@(Ref False q) = set q t
--         aux vset t               s@(Ref True  q) = set q (dual t)

--         -- SEQUENTIAL COMPOSITION
--         aux vset Skip            Skip            = return ()
--         aux vset (Seq t1 t2)     (Seq s1 s2)     = do
--             go vset t1 s1
--             go vset t2 s2

--         -- CONSTANTS
--         aux vset Bot             Bot             = return ()
--         aux vset One             One             = return ()

--         -- CONNECTIVES
--         aux vset (Mul t1 t2) (Mul s1 s2) = do
--             go vset t1 s1
--             go vset t2 s2
--         aux vset (Par t1 t2) (Par s1 s2) = do
--             go vset t1 s1
--             go vset t2 s2
--         aux vset (Plus bs1) (Plus bs2) = do
--             let map1 = Map.fromList bs1
--             let map2 = Map.fromList bs2
--             sameTags (Map.keysSet map1) (Map.keysSet map2)
--             forM_ (Map.elems (zipMap map1 map2)) $ \((m1, t1), (m2, t2)) -> do
--                 addMeasureConstraintEq m1 m2
--                 go vset t1 t2
--         aux vset (With bs1) (With bs2) = do
--             let map1 = Map.fromList bs1
--             let map2 = Map.fromList bs2
--             sameTags (Map.keysSet map1) (Map.keysSet map2)
--             forM_ (Map.elems (zipMap map1 map2)) $ \((m1, t1), (m2, t2)) -> do
--                 addMeasureConstraintEq m1 m2
--                 go vset t1 t2

--         -- TYPE MISMATCH
--         aux vset t s = throw $ ErrorTypeMismatch name (show t) s

--         sameTags tags1 tags2 =
--             unless (tags1 == tags2) $
--                 throw $ ErrorLabelMismatch name (Set.elems tags1) (Set.elems tags2)

-- normalize :: Node -> HeapT Node
-- normalize = undefined

-- subst :: TypeName -> Type -> Type -> HeapT Type
-- subst tname t s = undefined

-- reachable :: Node -> HeapT (Set Node)
-- reachable = aux []
--     where
--         aux ns n | n `elem` ns = return Set.empty
--         aux ns n = do
--             t <- unfold n
--             nset <- Set.unions <$> mapM (aux (n : ns)) (children t)
--             return (Set.insert n nset)















-- -- |This module defines the internal representation of __types__ (Section 3).
-- module Type where

-- import Common
-- import Atoms
-- import Measure
-- import Data.Set (Set)
-- import qualified Data.Set as Set
-- import Data.Map (Map)
-- import qualified Data.Map as Map
-- import Data.List (sort)
-- import Control.Monad (forM_)

-- data TypeVariable = PolyVar TypeName | RecVar TypeName
--   deriving (Eq, Ord)

-- type TMap m = Map TypeVariable (Type m)

-- class TypeVariables a where
--   tvars :: a -> Set TypeVariable
--   rvars :: a -> Set TypeName
--   rvars x = Set.fromList [ tname | RecVar tname <- Set.toList (tvars x) ]

-- data TVar = TVar Int
--   deriving (Eq, Ord)

-- instance Enum TVar where
--   toEnum = TVar
--   fromEnum (TVar n) = n

-- -- |Session type representation. In addition to the forms described in the
-- -- paper, we also provide a 'Rec' constructor to represent recursive session
-- -- types explicitly in a closed form that is easier to convert into regular
-- -- trees.
-- data Type m
--   = One
--   | Bot
--   | Skip
--   | Seq (Type m) (Type m)
--   | Poly Bool TypeName
--   | Var TypeName
--   | Rec TypeName (Type m)
--   | Par (Type m) (Type m)
--   | Mul (Type m) (Type m)
--   | With [(Label, (m, Type m))]
--   | Plus [(Label, (m, Type m))]
--   | Dual (Type m)  -- internal use only
--   deriving (Eq, Ord)

-- type TypeS = Type ()
-- type TypeM = Type Measure

-- -- DEFINITIONS

-- -- |A type definition is a pair consisting of a type name and a session type.
-- type TypeDef = (TypeName, TypeS)

-- instance TypeVariables (Type m) where
--   tvars (Seq t s)      = Set.union (tvars t) (tvars s)
--   tvars (Poly _ tname) = Set.singleton (PolyVar tname)
--   tvars (Var tname)    = Set.singleton (RecVar tname)
--   tvars (Rec tname t)  = Set.delete (RecVar tname) (tvars t)
--   tvars (Par t s)      = Set.union (tvars t) (tvars s)
--   tvars (Mul t s)      = Set.union (tvars t) (tvars s)
--   tvars (With bs)      = Set.unions (map (tvars . snd . snd) bs)
--   tvars (Plus bs)      = Set.unions (map (tvars . snd . snd) bs)
--   tvars _              = Set.empty

-- tsubst :: TypeVariable -> Type m -> Type m -> Type m
-- tsubst tname t = tsubsts (Map.singleton tname t)

-- tsubsts :: TMap m -> Type m -> Type m
-- tsubsts tmap (Seq t s)      = Seq (tsubsts tmap t) (tsubsts tmap s)
-- tsubsts tmap (Par t s)      = Par (tsubsts tmap t) (tsubsts tmap s)
-- tsubsts tmap (Mul t s)      = Mul (tsubsts tmap t) (tsubsts tmap s)
-- tsubsts tmap (Plus bs)      = Plus (mapSnd (fmap (tsubsts tmap)) bs)
-- tsubsts tmap (With bs)      = With (mapSnd (fmap (tsubsts tmap)) bs)
-- tsubsts tmap (Poly d sname) | Just t <- Map.lookup (PolyVar sname) tmap = if d then dual t else t
-- tsubsts tmap (Var sname)    | Just t <- Map.lookup (RecVar sname) tmap = t
-- tsubsts tmap (Rec sname t)  = Rec sname (tsubsts (Map.delete (RecVar sname) tmap) t)
-- tsubsts tmap s              = s

-- instance TypeVariables a => TypeVariables [a] where
--   tvars = Set.unions . map tvars

-- -- unfold :: Type m -> Type m
-- -- unfold (Seq t s) = unfold (unfold t |> s)
-- -- unfold t@(Rec tname s) = unfold (tsubst (RecVar tname) t s)
-- -- unfold t = t

-- dual :: Type m -> Type m
-- dual One            = Bot
-- dual Bot            = One
-- dual Skip           = Skip
-- dual (Seq t s)      = Seq (dual t) (dual s)
-- dual (Poly d tname) = Poly (not d) tname
-- dual (Var tname)    = Var tname
-- dual (Rec tname t)  = Rec tname (dual t)
-- dual (Par t s)      = Mul (dual t) (dual s)
-- dual (Mul t s)      = Par (dual t) (dual s)
-- dual (With bs)      = Plus (mapSnd (fmap dual) bs)
-- dual (Plus bs)      = With (mapSnd (fmap dual) bs)

-- normalize :: Type m -> Type m
-- normalize = go True []
--   where
--     go :: Bool -> [Type m] -> Type m -> Type m
--     go _    _        One             = One
--     go _    _        Bot             = Bot
--     go _    []       Skip            = Skip
--     go unf  (t : ts) Skip            = go unf ts t
--     go unf  ts       (Seq t s)       = go unf (s : ts) t
--     go _    _        (Poly d tname)  = Poly d tname
--     go _    ts       (Mul t s)       = Mul t (go False ts s)
--     go _    ts       (Par t s)       = Par t (go False ts s)
--     go _    ts       (Plus bs)       = Plus (mapSnd (fmap $ go False ts) bs)
--     go _    ts       (With bs)       = With (mapSnd (fmap $ go False ts) bs)
--     go True ts       t@(Rec tname s) = go True ts (tsubst (RecVar tname) t s)
--     go _    ts       t@(Rec _ _)     = Seq t (go False ts Skip)

-- instance MeasureVariables t => MeasureVariables (Type t) where
--   mvars One            = Set.empty
--   mvars Bot            = Set.empty
--   mvars Skip           = Set.empty
--   mvars (Seq t s)      = Set.union (mvars t) (mvars s)
--   mvars (Poly _ tname) = Set.empty
--   mvars (Var tname)    = Set.empty
--   mvars (Rec tname t)  = mvars t
--   mvars (Par t s)      = Set.union (mvars t) (mvars s)
--   mvars (Mul t s)      = Set.union (mvars t) (mvars s)
--   mvars (With bs)      = Set.unions [ Set.union (mvars m) (mvars t) | (_, (m, t)) <- bs ]
--   mvars (Plus bs)      = Set.unions [ Set.union (mvars m) (mvars t) | (_, (m, t)) <- bs ]
  