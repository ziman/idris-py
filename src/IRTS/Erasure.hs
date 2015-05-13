{-# LANGUAGE PatternGuards #-}
module IRTS.Erasure (erase) where

import IRTS.Defunctionalise
import Idris.Core.TT (Name(..))

import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace

type FunDecl = DDecl
type CtorDecl = DDecl

data Arg = Arg Int | Ret deriving (Eq, Ord, Show)
data Node = N Name Arg deriving (Eq, Ord, Show)

type Guards = S.Set Node
type Uses = S.Set Node

infix 3 :<-:
data Impl = Uses :<-: Guards deriving (Eq, Ord, Show)
type Impls = S.Set Impl

type Vars = M.Map Name Impls

single :: Node -> Impls
single n = S.singleton $ S.singleton n :<-: S.empty

argVars :: Name -> [Name] -> Vars
argVars fn args = M.fromList [(n, single $ N fn (Arg i)) | (i,n) <- zip [0..] args]

cond :: Node -> Impls -> Impls
cond n = S.map $ \(uses :<-: guards) -> uses :<-: S.insert n guards

analyse :: FunDecl -> Impls
analyse (DFun fn args body) = anExp (argVars fn args) body

anExp :: Vars -> DExp -> Impls
anExp vs (DV (Loc i)) = error "de bruijns not implemented"
anExp vs (DV (Glob n))
    | Just impls <- M.lookup n vs = impls
    | otherwise = error $ show n ++ " not found in environment"
anExp vs (DApp _ fn args) = S.unions [anArg i e | (i, e) <- zip [0..] args]
  where
    anArg i e = cond (N fn $ Arg i) $ anExp vs e
anExp vs (DLet n v e) = anExp (M.insert n (anExp vs v) vs) e
anExp vs (DUpdate n e) = error "update not implemented"
anExp vs (DProj e i) = error "DProj not implemented"
anExp vs (DC _ t cn args) = anExp vs $ DApp False cn args
anExp vs (DCase ct e alts) = anCase vs e alts
anExp vs (DChkCase e alts) = anCase vs e alts
anExp vs (DConst _) = S.empty
anExp vs (DForeign _ _ args) = S.unions [anExp vs e | (_, e) <- args]
anExp vs (DOp _ args) = S.unions $ map (anExp vs) args
anExp vs  DNothing = S.empty
anExp vs (DError msg) = S.empty

anCase :: Vars -> DExp -> [DAlt] -> Impls
anCase vs s [DDefaultCase e] = anExp vs e  -- ignore the scrutinee
anCase vs s [DConstCase c e] = anExp vs s `S.union` anExp vs e
anCase vs s [DConCase t cn ns e] = anExp (vs' `M.union` vs) e
  where
    simpls = anExp vs s
    vs' = M.map (S.union simpls) (argVars cn ns)
anCase vs s alts = S.unions $ anExp vs s : map (anAlt vs) alts

anAlt :: Vars -> DAlt -> Impls
anAlt vs (DDefaultCase e) = anExp vs e
anAlt vs (DConstCase c e) = anExp vs e
anAlt vs (DConCase t cn ns e) = anExp (argVars cn ns `M.union` vs) e

-- todo: detagging

erase :: [(Name, DDecl)] -> [(Name, DDecl)]
erase decls = decls
  where
    ctors = [c | (_, c@(DConstructor _ _ _)) <- decls]
    funs  = [f | (_, f@(DFun _ _ _)) <- decls]
