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
data Node i = N Name i deriving (Eq, Ord, Show)

type Guards i = S.Set (Node i)
type Uses i = S.Set (Node i)

infix 3 :<-:
data Impl i = Uses i :<-: Guards i deriving (Eq, Ord, Show)
type Impls i = S.Set (Impl i)

data VarInfo i = VI
    { viImpls :: Impls i
    }
    deriving (Eq, Ord, Show)

type Vars i = M.Map Name (VarInfo i)

single :: Node i -> Impls i
single n = S.singleton $ S.singleton n :<-: S.empty

cond :: Ord i => Node i -> Impls i -> Impls i
cond n = S.map $ \(uses :<-: guards) -> uses :<-: S.insert n guards

analyse :: FunDecl -> Impls Int
analyse (DFun fn args body) = anExp argVars body
  where
    argVars = M.fromList [
        (n, VI
            { viImpls = single $ N fn i
            }
        ) | (i,n) <- zip [0..] args]

anExp :: Vars Int -> DExp -> Impls Int
anExp vs (DV (Loc i)) = error "de bruijns not implemented"
anExp vs (DV (Glob n))
    | Just varInfo <- M.lookup n vs = viImpls varInfo
    | otherwise = error $ show n ++ " not found in environment"
anExp vs (DApp _ fn args) = S.unions [anArg i e | (i, e) <- zip [0..] args]
  where
    anArg i e = cond (N fn i) $ anExp vs e
anExp vs (DLet n v e) = anExp (M.insert n (VI $ anExp vs v) vs) e
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
anExp vs e = error $ show e ++ " not implemented"

anCase :: Vars Int -> DExp -> [DAlt] -> Impls Int
anCase vs e alts = error "anCase not implemented"

erase :: [(Name, DDecl)] -> [(Name, DDecl)]
erase decls = decls
  where
    ctors = [c | (_, c@(DConstructor _ _ _)) <- decls]
    funs  = [f | (_, f@(DFun _ _ _)) <- decls]
