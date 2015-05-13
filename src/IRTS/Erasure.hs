{-# LANGUAGE PatternGuards #-}
module IRTS.Erasure (erase) where

import IRTS.Defunctionalise
import Idris.Core.TT (Name(..), sMN, sNS, sUN)
import Idris.Core.CaseTree (CaseType)

import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace

type FunDecl = DDecl
type CtorDecl = DDecl

data Arg = Arg Int | Ret | Tag deriving (Eq, Ord, Show)
data Node = N Name Arg deriving (Eq, Ord)

instance Show Node where
    show (N n (Arg i)) = "(" ++ show n ++ ", " ++ show i ++ ")"
    show (N n Ret) = "(" ++ show n ++ ", *R)"
    show (N n Tag) = "(" ++ show n ++ ", *T)"

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
analyse (DFun fn args body) = cond (N fn Ret) $ anExp (argVars fn args) body

anExp :: Vars -> DExp -> Impls
anExp vs (DV (Loc i)) = error "de bruijns not implemented"
anExp vs (DV (Glob n))
    | Just impls <- M.lookup n vs = impls
    | otherwise = error $ show n ++ " not found in environment"
anExp vs (DApp _ fn args) = single (N fn Ret) `S.union` S.unions [anArg i e | (i, e) <- zip [0..] args]
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
anCase vs s [DConstCase c e] = anExp vs e  -- ignore the scrutinee
anCase vs s [DConCase t cn ns e] = anExp (vs' `M.union` vs) e
  where
    simpls = anExp vs s
    vs' = M.map (S.union simpls) (argVars cn ns)
anCase vs s alts = S.unions $ anExp vs s : map (anAlt vs) alts

anAlt :: Vars -> DAlt -> Impls
anAlt vs (DDefaultCase e) = anExp vs e
anAlt vs (DConstCase c e) = anExp vs e
anAlt vs (DConCase t cn ns e) = single (N cn Tag) `S.union` anExp (argVars cn ns `M.union` vs) e

solve :: Uses -> Impls -> Uses
solve axioms impls = step impls' axioms
  where
    impls' = M.unionsWith S.union [M.singleton gs us | (us :<-: gs) <- S.toList impls]

step :: M.Map Guards Uses -> Uses -> Uses
step clauses answerSet
    | S.null newAtoms = answerSet
    | otherwise = step reducedClauses (S.union newAtoms answerSet)
  where
    newAtoms = M.findWithDefault S.empty S.empty reducedClauses S.\\ answerSet
    reducedClauses = reduce clauses
    reduce = M.mapKeysWith S.union reduceSet . M.map reduceSet
    reduceSet = (`S.difference` answerSet)

prune :: Uses -> (Name, DDecl) -> [(Name, DDecl)]
prune uses (n, DConstructor cn tag arity)
--    | not tagUsed && null argsUsed = []  -- problematic, breaks getTag & stuff
    | not tagUsed = [(n, DConstructor cn (-1) $ length argsUsed)]
    | otherwise = [(n, DConstructor cn tag $ length argsUsed)]
  where
    argsUsed = [i | i <- [0..arity-1], N cn (Arg i) `S.member` uses]
    tagUsed = N cn Tag `S.member` uses

prune uses (n, DFun fn args body)
    | not retUsed = []
    | otherwise = [(n, DFun fn (map snd argsUsed) $ pruneExp uses body)]
  where
    argsUsed = [(i,n) | (i,n) <- zip [0..] args, N fn (Arg i) `S.member` uses]
    retUsed = N fn Ret `S.member` uses

pruneExp :: Uses -> DExp -> DExp
pruneExp uses e@(DV _) = e
pruneExp uses (DApp tc fn es) = DApp tc fn $ map (pruneExp uses) es'
  where
    es' = [e | (i,e) <- zip [0..] es, N fn (Arg i) `S.member` uses]
pruneExp uses (DLet n v e) = DLet n (pruneExp uses v) (pruneExp uses e)
pruneExp uses (DUpdate n e) = DUpdate n (pruneExp uses e)
pruneExp uses (DProj e i) = DProj (pruneExp uses e) i
pruneExp uses (DC v t cn es) = DC v t cn $ map (pruneExp uses) es'
  where
    es' = [e | (i,e) <- zip [0..] es, N cn (Arg i) `S.member` uses]
pruneExp uses (DCase ct s alts) = pruneCase uses (Just ct) s alts
pruneExp uses (DChkCase s alts) = pruneCase uses  Nothing  s alts
pruneExp uses e@(DConst c) = e
pruneExp uses (DForeign d d' args) = DForeign d d' [(d'', pruneExp uses e) | (d'', e) <- args]
pruneExp uses (DOp fn es) = DOp fn $ map (pruneExp uses) es
pruneExp uses e@DNothing = e
pruneExp uses e@(DError msg) = e

mkCase :: Maybe CaseType -> DExp -> [DAlt] -> DExp
mkCase  Nothing  s alts = DChkCase s alts
mkCase (Just ct) s alts = DCase ct s alts

pruneCase :: Uses -> Maybe CaseType -> DExp -> [DAlt] -> DExp
pruneCase uses ct s [DDefaultCase e] = pruneExp uses e
pruneCase uses ct s [DConstCase c e] = pruneExp uses e
pruneCase uses ct s [DConCase tag cn ns e]
    | any (`occursIn` e) ns = mkCase ct s [DConCase tag cn ns $ pruneExp uses e]
    | otherwise = pruneExp uses e
pruneCase uses ct s alts = mkCase ct s $ map (pruneAlt uses) alts

pruneAlt :: Uses -> DAlt -> DAlt
pruneAlt uses (DDefaultCase e) = DDefaultCase $ pruneExp uses e
pruneAlt uses (DConstCase c e) = DConstCase c $ pruneExp uses e
pruneAlt uses (DConCase tag cn ns e) = DConCase tag cn ns' $ pruneExp uses e
  where
    ns' = [n | (i,n) <- zip [0..] ns, N cn (Arg i) `S.member` uses]

occursIn :: Name -> DExp -> Bool
occursIn n (DV (Glob n')) = n == n'
occursIn n (DV _) = False
occursIn n (DApp _ fn args) = (n `occursIn`) `any` args
occursIn n (DLet n' v e) = (n `occursIn` v) || ((n /= n') && (n `occursIn` e))
occursIn n (DUpdate n' e) = n `occursIn` e
occursIn n (DProj e i) = n `occursIn` e
occursIn n (DC _ t cn args) = (n `occursIn`) `any` args
occursIn n (DCase _ s alts) = (n `occursIn` s) || any (n `occursInAlt`) alts
occursIn n (DChkCase s alts) = (n `occursIn` s) || any (n `occursInAlt`) alts
occursIn n (DConst _) = False
occursIn n (DForeign _ _ args) = or [n `occursIn` e | (_, e) <- args]
occursIn n (DOp _ args) = (n `occursIn`) `any` args
occursIn n  DNothing = False
occursIn n (DError _) = False

occursInAlt :: Name -> DAlt -> Bool
occursInAlt n (DDefaultCase e) = n `occursIn` e
occursInAlt n (DConstCase c e) = n `occursIn` e
occursInAlt n (DConCase tag cn ns e) = all (n /=) ns && (n `occursIn` e)

-- todo: constructor tag = -1 -> detagged
-- todo: only-tag constructors should compile to int
-- todo: newtype optimisation

erase :: [(Name, DDecl)] -> [(Name, DDecl)]
erase decls = {- dbg `trace` -} concatMap (prune uses) decls
  where
    impls = S.unions $ map analyse funs
    uses = solve axioms impls

    ctors = [c | (_, c@(DConstructor _ _ _)) <- decls]
    funs  = [f | (_, f@(DFun _ _ _)) <- decls]

    axioms = S.fromList
        [ N (sMN 0 "runMain") Ret
        
        -- compiler special-casing to python lists rely on not erasing the args
        , N (sNS (sUN "::") ["List", "Prelude"]) Tag
        , N (sNS (sUN "::") ["List", "Prelude"]) (Arg 0)
        , N (sNS (sUN "::") ["List", "Prelude"]) (Arg 1)
        , N (sNS (sUN "Nil") ["List", "Prelude"]) Tag

        , N (sNS (sUN "TCons") ["Telescope", "Python"]) Tag
        , N (sNS (sUN "TCons") ["Telescope", "Python"]) (Arg 0)
        , N (sNS (sUN "TCons") ["Telescope", "Python"]) (Arg 1)
        , N (sNS (sUN "TSkip") ["Telescope", "Python"]) Tag
        , N (sNS (sUN "TSkip") ["Telescope", "Python"]) (Arg 0)
        , N (sNS (sUN "TNil")  ["Telescope", "Python"]) Tag

        , N (sNS (sUN "Just")    ["Maybe", "Prelude"]) Tag
        , N (sNS (sUN "Just")    ["Maybe", "Prelude"]) (Arg 0)
        , N (sNS (sUN "Nothing") ["Maybe", "Prelude"]) Tag
        ]

    dbg = unlines
        [ "USAGE:"
        , fmt impls
        , "USES:"
        , unlines . map (("  " ++) . show) . S.toList $ uses
        ]

    fmt = unlines . map fmtImpl . S.toList
    fmtImpl (us :<-: gs) = show (S.toList us) ++ " <-- " ++ show (S.toList gs)
