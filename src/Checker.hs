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

-- |This module provides an implementation of the type checker
-- according to the algorithmic version of the type system
module Checker where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.List (sort)
import qualified Data.Set as Set
import Control.Monad (forM_, forM, when, unless)
import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as State
import Control.Exception (throw)
import Data.Maybe ( fromJust )
import qualified Render

import Debug.Trace

import Common
import Atoms
import Measure
import Type
import Process
import Exceptions

type TypeContext = Map TypeName TypeM
type ProcessContext = Map ProcessName (Measure, [TypeM])
type Checker = StateT (ProcessContext, TVar, MVar, TypeContext, [MeasureConstraint]) IO

find :: TypeM -> Checker TypeM
find t@(Var d tname) = do
  (_, _, _, tmap, _) <- State.get
  case Map.lookup tname tmap of
    Nothing -> return t
    Just t  -> find (if d then dual t else t) 
find t = return t

instantiate :: [TypeM] -> Checker [TypeM]
instantiate ts = do
  let vs = Set.toList (tvars ts)
  ss <- mapM (const newTypeVar) vs
  let tmap = Map.fromList (zip vs ss)
  return (map (tsubsts tmap) ts)

addTypeConstraint :: TypeName -> TypeM -> Checker ()
addTypeConstraint tname t = do
  -- State.lift $ putStr $ "ADDING TYPE CONSTRAINT FOR " ++ show tname ++ " "
  -- State.lift $ Render.printType t
  -- State.lift $ putStrLn ""
  (pctxt, tv, mv, tmap, cs) <- State.get
  State.put (pctxt, tv, mv, Map.insert tname t tmap, cs)

unify :: [(ChannelName, TypeM, TypeM)] -> Checker ()
unify = mapM_ aux
  where
    aux :: (ChannelName, TypeM, TypeM) -> Checker ()
    aux (name, t, s) = auxT t s
      where
        auxT :: TypeM -> TypeM -> Checker ()
        auxT t s = do
          -- State.lift $ putStr "UNIFY "
          -- State.lift $ Render.printType t
          -- State.lift $ putStr "\nAND   "
          -- State.lift $ Render.printType s
          -- State.lift $ putStrLn "\n"
          tf <- find t
          sf <- find s
          auxV tf sf

        -- unification for polymorphic variables
        auxV (Var False tname) (Var False sname) | tname == sname = return ()
        auxV t@(Var False tname) s | Set.member (PolyVar tname) (tvars s) = throw $ ErrorTypeMismatch name (show t) s
        auxV t@(Var _ tname) s@(Var _ sname) | tname > sname = auxT s t
        auxV (Var False tname) s | Set.null (rvars s) = addTypeConstraint tname s
                                  | otherwise = throw $ ErrorNotImplemented "unification with free recursion variables"
        auxV (Var True tname) s = auxT (Type.dual s) (Var False tname)
        auxV t s@(Var _ _) = auxT s t

        -- TODO: to implement the occur check correctly we should look at the
        -- type expanded with all the unifications

        -- unification for equi-recursive types
        auxV (Inv tname) (Inv sname) | tname == sname = return ()
        auxV (Rec tname t) (Rec sname s) | tname == sname = auxT t s

        -- unification for sequential composition
        auxV Skip        Skip        = return ()
        auxV (Seq t1 s1) (Seq t2 s2) = do
          auxT t1 t2
          auxT s1 s2

        -- unification for regular constructors
        auxV One         One         = return ()
        auxV Bot         Bot         = return ()
        auxV (Par t1 s1) (Par t2 s2) = do
          auxT t1 t2
          auxT s1 s2
        auxV (Mul t1 s1) (Mul t2 s2) = do
          auxT t1 t2
          auxT s1 s2
        auxV (With bs1) (With bs2) = do
          let m1 = Map.fromList bs1
          let m2 = Map.fromList bs2
          tle (Map.keysSet m2) (Map.keysSet m1)
          forM_ (Map.elems (zipMap m1 m2)) (uncurry auxT)
        auxV (Plus bs1) (Plus bs2) = do
          let m1 = Map.fromList bs1
          let m2 = Map.fromList bs2
          tle (Map.keysSet m1) (Map.keysSet m2)
          forM_ (Map.elems (zipMap m1 m2)) (uncurry auxT)
        auxV (Put m) (Put n) = addMeasureConstraintEq n m
        auxV (Get m) (Get n) = addMeasureConstraintEq m n
        -- type mismatch
        auxV t s = throw $ ErrorTypeMismatch name (show t) s

        tle tags1 tags2 =
          unless (tags1 == tags2) $
            throw $ ErrorLabelMismatch name (Set.elems tags1) (Set.elems tags2)

checkTypeEq :: ChannelName -> TypeM -> TypeM -> Checker ()
checkTypeEq x t s = unify [(x, t, s)]

checkContextEq :: Context -> Context -> Checker ()
checkContextEq ctx1 ctx2 = do
  let uset = Map.keysSet ctx1
  let vset = Map.keysSet ctx2
  unless (uset == vset) $ throw $ ErrorLinearity $ Set.elems $ Set.union (Set.difference uset vset) (Set.difference vset uset)
  -- Make sure that the expected and actual types of the argument match.
  forM_ (Map.toList (zipMap ctx1 ctx2)) $ \(x, (t1, t2)) -> do
    checkTypeEq x t1 t2

-- |A __context__ is a finite map from channel names to session types
-- represented as regular trees.
type Context = Map ChannelName TypeM

newTypeVar :: Checker TypeM
newTypeVar = do
  (penv, n, m, tmap, cs) <- State.get
  State.put (penv, succ n, m, tmap, cs)
  return (Var False (Identifier Somewhere (show n)))

newMeasureVar :: Checker MVar
newMeasureVar = do
  (penv, n, μ, tmap, cs) <- State.get
  State.put (penv, n, succ μ, tmap, cs)
  return μ

annotateType :: TypeS -> Checker TypeM
annotateType = aux
  where
    aux (Var tname t) = return $ Var tname t
    aux (Inv tname) = return $ Inv tname
    aux One = return One
    aux Bot = return Bot
    aux Skip = return Skip
    aux (Seq t s) = do
      t' <- aux t
      s' <- aux s
      return $ Seq t' s'
    aux (Rec tname t) = do
      t' <- aux t
      return $ Rec tname t'
    aux (Par t s) = do
      t' <- aux t
      s' <- aux s
      return $ Par t' s'
    aux (Mul t s) = do
      t' <- aux t
      s' <- aux s
      return $ Mul t' s'
    aux (Plus bs) = do
      bs' <- mapM auxB bs
      return $ Plus bs'
    aux (With bs) = do
      bs' <- mapM auxB bs
      return $ With bs'
    aux (Get m) = Get <$> auxM m
    aux (Put m) = Put <$> auxM m

    auxB (tag, t) = do
      t' <- aux t
      return (tag, t')

    auxM () = MRef <$> newMeasureVar

annotateExpr :: TypeE -> Checker TypeM
annotateExpr (Copy t) = annotateType t
annotateExpr (Dual t) = dual <$> annotateType t

annotateProcess :: ProcessS -> Checker ProcessM
annotateProcess = go
  where
    go :: ProcessS -> Checker ProcessM
    go (Call pname xs) = return $ Call pname xs
    go (Link x y) = return $ Link x y
    go (Wait x p) = do
      p <- go p
      return $ Wait x p
    go (Close x) = return $ Close x
    go (Fork x y p q) = do
      p <- go p
      q <- go q
      return $ Fork x y p q
    go (Join x y p) = do
      p <- go p
      return $ Join x y p
    go (Select x tag p) = do
      p <- go p
      return $ Select x tag p
    go (Case x bs) = do
      bs <- mapM goB bs
      return $ Case x bs
    go (PutGas x p) = do
      p <- go p
      return $ PutGas x p
    go (GetGas x p) = do
      p <- go p
      return $ GetGas x p
    go (Cut x t p q) = do
      t <- annotateExpr t
      p <- go p
      q <- go q
      return $ Cut x t p q

    goB :: (a, ProcessS) -> Checker (a, ProcessM)
    goB (x, p) = do
      p <- go p
      return (x, p)

getProcess :: ProcessName -> Checker (Measure, [TypeM])
getProcess pname = do
  (penv, _, _, _, _) <- State.get
  case Map.lookup pname penv of
    Nothing -> throw $ ErrorUnknownIdentifier "process" (showWithPos pname)
    Just (m, ts) -> do
      ss <- instantiate ts
      return (m, ss)

setProcess :: ProcessName -> Measure -> [TypeM] -> Checker ()
setProcess pname m ts = do
  (penv, n, μ', tmap, cs') <- State.get
  let penv' = Map.insert pname (m, ts) penv
  State.put (penv', n, μ', tmap, cs')

addProcess :: ProcessName -> [(ChannelName, TypeE)] -> Checker Context
addProcess pname xts = do
  (penv, _, _, _, _) <- State.get
  unless (not (Map.member pname penv)) $ throw $ ErrorMultipleProcessDefinitions pname
  μ <- MRef <$> newMeasureVar
  ts <- mapM (annotateExpr . snd) xts
  setProcess pname μ ts
  return $ Map.fromList (zip (map fst xts) ts)

getMeasureConstraints :: Checker [MeasureConstraint]
getMeasureConstraints = do
  (penv, m, μ, tmap, cs) <- State.get
  State.put (penv, m, toEnum 0, tmap, [])
  return cs

addMeasureConstraint :: MeasureConstraint -> Checker ()
addMeasureConstraint c = do
  (penv, m, n, tmap, cs) <- State.get
  State.put (penv, m, n, tmap, c : cs)

addMeasureConstraints :: [MeasureConstraint] -> Checker ()
addMeasureConstraints = mapM_ addMeasureConstraint

addMeasureConstraintEq :: Measure -> Measure -> Checker ()
addMeasureConstraintEq m n | m == n = return ()
                           | otherwise = addMeasureConstraint (CEq m n)

addMeasureConstraintLe :: Measure -> Measure -> Checker ()
addMeasureConstraintLe m n | m == n = return ()
                           | otherwise = addMeasureConstraint (CLe m n)

peek :: Context -> ChannelName -> Checker TypeM
peek ctx x =
  case Map.lookup x ctx of
    Nothing -> throw $ ErrorUnknownIdentifier "channel" (showWithPos x)
    Just t -> return t

-- |Remove a channel from a context, returning the remaining context
-- and the session type associated with the channel.
remove :: Context -> ChannelName -> Checker (Context, TypeM)
remove ctx x = do
  t <- peek ctx x
  return (Map.delete x ctx, t)

insert :: Context -> ChannelName -> TypeM -> Checker Context
insert ctx x t =
  case Map.lookup x ctx of
    Just _ -> throw $ ErrorLinearity [x]
    Nothing -> return (Map.insert x t ctx)

-- | Check that all process definitions are well typed. The first
-- argument is the subtyping relation being used, so that it is
-- possible to choose among fair and unfair subtyping.
checkTypes :: [ProcessDefS] -> IO (TypeContext, [MeasureConstraint], [ProcessDef])
checkTypes pdefs = State.evalStateT (checkProgram pdefs) (Map.empty, toEnum 0, toEnum 0, Map.empty, [])
  where
    checkProgram :: [ProcessDefS] -> Checker (TypeContext, [MeasureConstraint], [ProcessDef])
    checkProgram pdefs = do
      pdefs <- mapM (\(pname, xts, p) -> do
                        ctx <- addProcess pname xts
                        return (pname, xts, ctx, p)
                    ) pdefs
      pdefs <- mapM (\(pname, xts, ctx, p) -> do
                        (p', ν) <- annotateProcess p >>= auxP ctx
                        (μ, ts) <- getProcess pname
                        addMeasureConstraintLe ν μ
                        return (pname, μ, zip (map fst xts) ts, p')
                    ) pdefs
      (_, _, _, tmap, cs) <- State.get
      return (tmap, cs, pdefs)

    -- Check that the context is empty. If not, there are some
    -- channels left unused.
    checkEmpty :: Context -> Checker ()
    checkEmpty ctx = unless (Map.null ctx) $ throw $ ErrorLinearity (Map.keys ctx)

    -- Return the list of session types associated with the free
    -- names of a process name.
    -- checkProcess :: ProcessName -> Checker (Measure, [Type])
    -- checkProcess pname = do
    --   case Map.lookup pname penv of
    --     Nothing -> throw $ ErrorUnknownIdentifier "process" (showWithPos pname)
    --     Just (m, gs) -> return (m, gs)

    partitionContext :: Context -> ProcessM -> ProcessM -> (Context, Context)
    partitionContext ctx p q = (pctx, qctx)
      where
        pctx = Map.withoutKeys ctx qnameset
        qctx = Map.restrictKeys ctx qnameset
        qnameset = fn q

    -- Check that a process is well typed in a given context.
    auxP :: Context -> ProcessM -> Checker (ProcessM, Measure)
    auxP ctx (Call pname xs) = do
      (μ, ts) <- getProcess pname
      unless (length ts == length xs) $ throw $ ErrorArityMismatch pname (length ts) (length xs)
      let ctx' = Map.fromList (zip xs ts)
      checkContextEq ctx' ctx
      return (Call pname xs, μ)
    -- Link
    auxP ctx (Link x y) = do
      (ctx, t) <- remove ctx x
      (ctx, s) <- remove ctx y
      checkEmpty ctx
      checkTypeEq x t (Type.dual s)
      μ <- MRef <$> newMeasureVar
      return (Link x y, mzero)
    -- Rule [⊥]
    auxP ctx (Wait x p) = do
      -- Remove the association for x from the context.
      (ctx, t) <- remove ctx x
      -- Make sure that the type of x is ?end
      checkTypeEq x Type.Bot (expose t)
      -- Type check the continuation.
      (p', μ) <- auxP ctx p
      return (Wait x p', μ)
    -- Rule [1]
    auxP ctx (Close x) = do
      -- Remove the association for x from the context.
      (ctx, t) <- remove ctx x
      -- Make sure that the remaining context is empty.
      checkEmpty ctx
      -- Make sure that the type of x is !end
      checkTypeEq x Type.One (expose t)
      μ <- MRef <$> newMeasureVar
      return (Close x, msucc μ)
    -- Rule [#]
    auxP ctx (Join x y p) = do
      -- If y already occurs in the context it shadows a linear name
      when (y `Map.member` ctx) $ throw $ ErrorLinearity [y]
      -- Remove the association for x from the context.
      (ctx, t) <- remove ctx x
      -- Check the shape of the type of x.
      case expose t of
        -- If it is the input of a channel, insert the association
        -- for y in the context along with the updated type of x and
        -- type check the continuation.
        Type.Par s t' -> do
          ctx <- insert ctx x t'
          ctx <- insert ctx y s
          (p', μ) <- auxP ctx p
          return (Join x y p', μ)
        -- If it is any other type, signal the error.
        _ -> throw $ ErrorTypeMismatch x "|" t
    -- Rule [⊗]
    auxP ctx (Fork x y p q) = do
      -- Remove the association for x from the context.
      (ctx, t) <- remove ctx x
      let (ctxp, ctxq) = partitionContext ctx p q
      -- Check the shape of the type associated with x.
      case expose t of
        -- If it is the output of a channel...
        Type.Mul s t' -> do
          ctxp <- insert ctxp y s
          ctxq <- insert ctxq x t'
          (p', μ) <- auxP ctxp p
          -- Update the type of x and type check the continuation.
          (q', ν) <- auxP ctxq q
          return (Fork x y p' q', madd μ ν)
        -- If it is any other type...
        _ -> throw $ ErrorTypeMismatch x "*" t
    -- Rule [⊕]
    auxP ctx (Select x tag p) = do
      (ctx, t) <- remove ctx x
      case expose t of
        s@(Type.Plus bs) -> do
          case lookup tag bs of
            Just sk -> do
              ctx <- insert ctx x sk
              (p', μ) <- auxP ctx p
              return (Select x tag p', msucc μ)
            Nothing -> throw $ ErrorLabelMismatch x (map fst bs) [tag]
        _ -> throw $ ErrorTypeMismatch x "⊕" t
    -- Rule [&]
    auxP ctx (Case x cs) = do
      μ <- MRef <$> newMeasureVar
      -- Remove the association for x from the context.
      (ctx, t) <- remove ctx x
      -- Check the shape of the type associated with x.
      case expose t of
        -- If it is a "with"...
        Type.With bs -> do
          let tmap = Map.fromList bs
          let pmap = Map.fromList cs
          -- Retrieve the set of labels from the type
          let tlabels = Map.keys tmap
          -- Retrieve the set of labels from the process
          let plabels = Map.keys pmap
          -- If the two sets of labels differ, there is mismatch
          -- between type and process.
          unless (tlabels == plabels) $ throw $ ErrorLabelMismatch x tlabels plabels
          -- Type check each branch after updating the context.
          tps <- forM (Map.toList (zipMap tmap pmap)) $
            \(tag, (si, pi)) -> do
              ctx <- insert ctx x si
              (pi', μi) <- auxP ctx pi
              addMeasureConstraintLe μi μ
              return (tag, pi')
          return (Case x tps, μ)
        -- In all the other cases the type is just the wrong one
        _ -> throw $ ErrorTypeMismatch x "&" t
    -- Rule [put]
    auxP ctx (PutGas x p) = do
      (ctx, t) <- remove ctx x
      case expose t of
        Type.Seq (Type.Put ν) s -> do
          ctx <- insert ctx x s
          (p', μ) <- auxP ctx p
          return (PutGas x p', msucc (madd μ ν))
        _ -> throw $ ErrorTypeMismatch x "put" t
    -- Rule [get]
    auxP ctx (GetGas x p) = do
      (ctx, t) <- remove ctx x
      case expose t of
        Type.Seq (Type.Get ν) s -> do
          ctx <- insert ctx x s
          (p', μ) <- auxP ctx p
          addMeasureConstraintLe ν μ
          return (GetGas x p', MSub μ ν)
        u -> throw $ ErrorTypeMismatch x "get" u
    -- Rule [cut]
    auxP ctx (Cut x t p q) = do
      -- If x already occurs in the context we throw an exception,
      -- because it would shadow a linear resource.
      when (x `Map.member` ctx) $ throw $ ErrorLinearity [x]
      let (ctxp, ctxq) = partitionContext ctx p q
      ctxp <- insert ctxp x t
      ctxq <- insert ctxq x (Type.dual t)
      (p', μ) <- auxP ctxp p
      (q', ν) <- auxP ctxq q
      return (Cut x t p' q', madd μ ν)
