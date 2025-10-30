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
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import Control.Exception (throw)
import Data.Maybe ( fromJust )

import Debug.Trace

import Common
import Atoms
import Measure
import Strategy
import Type
import Process
import Exceptions

type TypeContext = Map TypeName TypeM
type Unifier = State (TypeContext, [MeasureConstraint])
type ProcessContext = Map ProcessName (Measure, [TypeM])
type Checker = State (ProcessContext, MVar, [MeasureConstraint])

find :: TypeM -> Unifier TypeM
find t@(Poly d tname) = do
  (tmap, cs) <- State.get
  case Map.lookup tname tmap of
    Nothing -> return t
    Just t  -> find (if d then dual t else t) 
find t = return t

addTypeConstraint :: TypeName -> TypeM -> Unifier ()
addTypeConstraint tname t = undefined

unify :: [(ChannelName, TypeM, TypeM)] -> Checker ()
unify xs = addMeasureConstraints (snd (State.execState (forM_ xs aux) ([], [])))
  where
    aux :: (ChannelName, TypeM, TypeM) -> Unifier ()
    aux (name, t, s) = auxT [] t s
      where
        auxT :: [(TypeM, TypeM)] -> TypeM -> TypeM -> Unifier ()
        auxT vs t s | (t, s) `elem` vs = return ()
        auxT vs t s = do
          tf <- find (Type.unfold t)
          sf <- find (Type.unfold s)
          auxV ((tf, sf) : vs) tf sf

        -- unification for polymorphic variables
        auxV vs (Poly False tname1) (Poly False tname2) | tname1 == tname2 = return ()
        auxV vs t@(Poly False tname) s | Set.member tname (Type.fv s) = throw $ ErrorTypeMismatch name t s
        auxV vs t@(Poly _ tname1) s@(Poly _ tname2) | tname1 > tname2 = auxT vs s t
        auxV vs (Poly False tname) t = addTypeConstraint tname t
        auxV vs (Poly True tname) t = auxT vs (Type.dual t) (Poly False tname)
        auxV vs t s@(Poly _ _) = auxT vs s t

        -- unification for sequential composition
        auxV vs Skip        Skip        = return ()
        auxV vs (Seq t1 s1) (Seq t2 s2) = do
          aux vs t1 t2
          aux vs s1 s2

        -- unification for regular constructors
        auxV vs One         One         = return ()
        auxV vs Bot         Bot         = return ()
        auxV vs (Par t1 s1) (Par t2 s2) = do
          aux vs t1 t2
          aux vs s1 s2
        auxV vs (Mul t1 s1) (Mul t2 s2) = do
          aux vs t1 t2
          aux vs s1 s2
        auxV vs (With bs1) (With bs2) = do
          let m1 = Map.fromList bs1
          let m2 = Map.fromList bs2
          tle (Map.keysSet m2) (Map.keysSet m1)
          forM_ (Map.elems (zipMap m1 m2)) (uncurry (aux vs))
        auxV vs (Plus bs1) (Plus bs2) = do
          let m1 = Map.fromList bs1
          let m2 = Map.fromList bs2
          tle (Map.keysSet m1) (Map.keysSet m2)
          forM_ (Map.elems (zipMap m1 m2)) (uncurry (aux vs))
        auxV vs (Put m t) (Put n s) = do
          aux vs t s
          addMeasureConstraintEq n m
        auxV vs (Get m t) (Get n s) = do
          aux vs t s
          addMeasureConstraintEq m n
        -- type mismatch
        auxV _ t s = throw $ ErrorTypeMismatch name (show t) s

        tle tags1 tags2 =
          unless (tags1 == tags2) $
            throw $ ErrorLabelMismatch name (Set.elems tags1) (Set.elems tags2)

checkTypeEq :: ChannelName -> TypeM -> TypeM -> Checker ()
checkTypeEq x t s = unify [(x, t, s)]

checkContextSub :: Context -> Context -> Checker ()
checkContextSub ctx1 ctx2 = do
  let uset = Map.keysSet ctx1
  let vset = Map.keysSet ctx2
  unless (uset == vset) $ throw $ ErrorLinearity $ Set.elems $ Set.union (Set.difference uset vset) (Set.difference vset uset)
  -- Make sure that the expected and actual types of the argument match.
  forM_ (Map.toList (zipMap ctx1 ctx2)) $ \(x, (t1, t2)) -> do
    checkTypeEq x t1 t2

-- |A __context__ is a finite map from channel names to session types
-- represented as regular trees.
type Context = Map ChannelName TypeM

newMeasureVar :: Checker MVar
newMeasureVar = do
  (penv, μ, cs) <- State.get
  State.put (penv, succ μ, cs)
  return μ

annotateType :: TypeS -> Checker TypeM
annotateType = aux
  where
    aux (Var tname) = return $ Var tname
    aux One = return One
    aux Bot = return Bot
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
    aux (Get m t) = do
      m' <- auxM m
      t' <- aux t
      return $ Get m' t'
    aux (Put m t) = do
      m' <- auxM m
      t' <- aux t
      return $ Put m' t'

    auxB (tag, t) = do
      t' <- aux t
      return (tag, t')

    auxM Nothing = MRef <$> newMeasureVar
    auxM (Just n) = return $ MCon n

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
      t <- annotateType t
      p <- go p
      q <- go q
      return $ Cut x t p q

    goB :: (a, ProcessS) -> Checker (a, ProcessM)
    goB (x, p) = do
      p <- go p
      return (x, p)

getProcess :: ProcessName -> Checker (Measure, [TypeM])
getProcess pname = do
  (penv, _, _) <- State.get
  case Map.lookup pname penv of
    Nothing -> throw $ ErrorUnknownIdentifier "process" (showWithPos pname)
    Just pd -> return pd

setProcess :: ProcessName -> Measure -> [TypeM] -> Checker ()
setProcess pname m ts = do
  (penv, μ', cs') <- State.get
  let penv' = Map.insert pname (m, ts) penv
  State.put (penv', μ', cs')

addProcess :: ProcessName -> [(ChannelName, TypeS)] -> Checker Context
addProcess pname xts = do
  (penv, _, _) <- State.get
  unless (not (Map.member pname penv)) $ throw $ ErrorMultipleProcessDefinitions pname
  μ <- MRef <$> newMeasureVar
  ts <- mapM (annotateType . snd) xts
  setProcess pname μ ts
  return $ Map.fromList (zip (map fst xts) ts)

getMeasureConstraints :: Checker [MeasureConstraint]
getMeasureConstraints = do
  (penv, μ, cs) <- State.get
  State.put (penv, toEnum 0, [])
  return cs

addMeasureConstraint :: MeasureConstraint -> Checker ()
addMeasureConstraint c = do
  (penv, n, cs) <- State.get
  State.put (penv, n, c : cs)

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
checkTypes :: Strategy -> [ProcessDefS] -> ([MeasureConstraint], [ProcessDef])
checkTypes strat pdefs = State.evalState (checkProgram pdefs) (Map.empty, toEnum 0, [])
  where
    checkProgram :: [ProcessDefS] -> Checker ([MeasureConstraint], [ProcessDef])
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
      (_, _, cs) <- State.get
      return (cs, pdefs)

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
      checkContextSub ctx' ctx
      return (Call pname xs, mcall strat μ)
    -- Link
    auxP ctx (Link x y) = do
      (ctx, t) <- remove ctx x
      (ctx, s) <- remove ctx y
      checkEmpty ctx
      checkTypeEq x t (Type.dual s)
      μ <- MRef <$> newMeasureVar
      return (Link x y, mlink strat μ)
    -- Rule [⊥]
    auxP ctx (Wait x p) = do
      -- Remove the association for x from the context.
      (ctx, t) <- remove ctx x
      -- Make sure that the type of x is ?end
      checkTypeEq x Type.Bot t
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
      checkTypeEq x Type.One t
      μ <- MRef <$> newMeasureVar
      return (Close x, mclose strat μ)
    -- Rule [#]
    auxP ctx (Join x y p) = do
      -- If y already occurs in the context it shadows a linear name
      when (y `Map.member` ctx) $ throw $ ErrorLinearity [y]
      -- Remove the association for x from the context.
      (ctx, t) <- remove ctx x
      -- Check the shape of the type of x.
      case Type.unfold t of
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
      case Type.unfold t of
        -- If it is the output of a channel...
        Type.Mul s t' -> do
          ctxp <- insert ctxp y s
          ctxq <- insert ctxq x t'
          (p', μ) <- auxP ctxp p
          -- Update the type of x and type check the continuation.
          (q', ν) <- auxP ctxq q
          return (Fork x y p' q', mfork strat (madd μ ν))
        -- If it is any other type...
        _ -> throw $ ErrorTypeMismatch x "*" t
    -- Rule [⊕]
    auxP ctx (Select x tag p) = do
      (ctx, t) <- remove ctx x
      case Type.unfold t of
        Type.Plus bs -> do
          case lookup tag bs of
            Just sk -> do
              ctx <- insert ctx x sk
              (p', μ) <- auxP ctx p
              return (Select x tag p', mtag strat μ)
            Nothing -> throw $ ErrorLabelMismatch x (map fst bs) [tag]
        _ -> throw $ ErrorTypeMismatch x "⊕" t
    -- Rule [&]
    auxP ctx (Case x cs) = do
      μ <- MRef <$> newMeasureVar
      -- Remove the association for x from the context.
      (ctx, t) <- remove ctx x
      -- Check the shape of the type associated with x.
      case Type.unfold t of
        -- If it is a "with"...
        Type.With bs -> do
          let tmap = Map.fromList bs
          let pmap = Map.fromList cs
          -- Retrieve the set of labels from the type
          let tlabels = Map.keys tmap
          -- Retrieve the set of labels from the process
          let plabels = Map.keys pmap
          -- If the two sets of labels differ, there is mismatch between type
          -- and process.
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
      case Type.unfold t of
        Type.Put ν s -> do
          ctx <- insert ctx x s
          (p', μ) <- auxP ctx p
          return (PutGas x p', mput strat (madd μ ν))
        _ -> throw $ ErrorTypeMismatch x "++" t
    -- Rule [get]
    auxP ctx (GetGas x p) = do
      (ctx, t) <- remove ctx x
      case Type.unfold t of
        Type.Get ν s -> do
          ctx <- insert ctx x s
          (p', μ) <- auxP ctx p
          addMeasureConstraintLe ν μ
          return (GetGas x p', MSub μ ν)
        _ -> throw $ ErrorTypeMismatch x "--" t
    -- Rule [cut]
    auxP ctx (Cut x t p q) = do
      -- If x already occurs in the context we throw an exception, because it
      -- would shadow a linear resource.
      when (x `Map.member` ctx) $ throw $ ErrorLinearity [x]
      let (ctxp, ctxq) = partitionContext ctx p q
      ctxp <- insert ctxp x t
      ctxq <- insert ctxq x (Type.dual t)
      (p', μ) <- auxP ctxp p
      (q', ν) <- auxP ctxq q
      return (Cut x t p' q', madd μ ν)
