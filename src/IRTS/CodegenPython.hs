{-# LANGUAGE PatternGuards #-}
module IRTS.CodegenPython (codegenPython) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import IRTS.Defunctionalise
import Idris.Core.TT

import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Monad
import Control.Applicative hiding (empty, Const)
import Control.Monad.Trans.State.Lazy

import Text.PrettyPrint hiding (Str)

data CGState = CGState
    { varCounter :: Int
    , ctors :: M.Map Name Int
    }
    deriving (Show)

type Stmts = Doc  -- statements
type Expr  = Doc  -- expressions

-- A code generator for "a" generates:
-- 1. statements that prepare context for the expression "a"
-- 2. the actual expression "a"
newtype CG a = CG { runCG :: State CGState (Stmts, a) }

instance Functor CG where
    fmap f (CG x) = CG $ do
        (stmts, expr) <- x
        return (stmts, f expr)

instance Applicative CG where
    pure x = CG (return (empty, x))
    CG f <*> CG x = CG $ do
        (stf, f') <- f
        (stx, x') <- x
        return (stf $+$ stx, f' x')

instance Monad CG where
    return = pure
    CG x >>= f = CG $ do
        (stx, x') <- x
        (sty, y') <- runCG $ f x'
        return (stx $+$ sty, y')

err :: String -> CG Expr
err = return . cgError

smap :: (Stmts -> Stmts) -> CG a -> CG a
smap f (CG x) = CG $ do
    (stmts, expr) <- x
    return (f stmts, expr)

emit :: Stmts -> CG ()
emit stmts = CG $ return (stmts, ())

sindent :: CG a -> CG a
sindent = smap indent

fresh :: CG LVar
fresh = CG $ do
    CGState vc ctors <- get
    put $ CGState (vc + 1) ctors
    return (empty, Loc (-vc))

ctorTag :: Name -> CG (Maybe Int)
ctorTag n = CG $ do
    CGState vc ctors <- get
    return (empty, M.lookup n ctors)

indent :: Doc -> Doc
indent = nest 2

pythonPreamble :: Doc
pythonPreamble = vcat . map text $
    [ "#!/usr/bin/env python"
    , ""
    , "import sys"
    , ""
    , "class IdrisError(Exception):"
    , "  pass"
    , ""
    , "def idris_error(msg):"
    , "  raise IdrisError(msg)"
    , ""
    ]

pythonLauncher :: Doc
pythonLauncher = vcat . map text $
    [ "if __name__ == '__main__':"
    , "  " ++ mangle (sMN 0 "runMain") ++ "()"
    , ""
    ]

-- idris's showCG is inexhaustive
showCG'' :: Name -> String
showCG'' (UN n) = T.unpack n
showCG'' (NS n s) = showSep "." (map T.unpack (reverse s)) ++ "." ++ showCG n
showCG'' (MN _ u) | u == txt "underscore" = "_"
showCG'' (MN i s) = "{" ++ T.unpack s ++ show i ++ "}"
showCG'' (SN s) = showCG' s
  where showCG' (WhereN i p c) = showCG p ++ ":" ++ showCG c ++ ":" ++ show i
        showCG' (WithN i n) = "_" ++ showCG n ++ "_with_" ++ show i
        showCG' (InstanceN cl inst) = '@':showCG cl ++ '$':showSep ":" (map T.unpack inst)
        showCG' (MethodN m) = '!':showCG m
        showCG' (ParentN p c) = showCG p ++ "#" ++ show c
        showCG' (CaseN c) = showCG c ++ "_case"
        showCG' (ElimN sn) = showCG sn ++ "_elim"
        showCG' (InstanceCtorN n) = showCG n ++ "_ictor"
showCG'' NErased = "_"

mangle :: Name -> String
mangle (MN i n)
    | all (\x -> isAlpha x || x `elem` "_") (T.unpack n)
    = T.unpack n ++ show i
mangle n = "idris_" ++ concatMap mangleChar (showCG'' n)
  where
    mangleChar x
        | isAlpha x || isDigit x = [x]
        | otherwise = "_" ++ show (ord x) ++ "_"

-- simpleDecls / defunDecls / liftDecls
codegenPython :: CodeGenerator
codegenPython ci = writeFile (outputFile ci) (render source)
  where
    source = pythonPreamble $+$ definitions $+$ pythonLauncher
    ctors = M.fromList [(n, tag) | (n, DConstructor n' tag arity) <- defunDecls ci]
    definitions = vcat $ map (cgDef ctors) [d | d@(_, DFun _ _ _) <- defunDecls ci]

cgName :: Name -> Doc
cgName = text . mangle

cgTuple :: [Doc] -> Doc
cgTuple xs = parens . hsep $ punctuate comma xs

cgApp :: Expr -> [Expr] -> Expr
cgApp f args
    | length (render simple) <= 60 = f <> parens simple
    | otherwise = 
        (f <> lparen)
        $+$ indent (vcat $ punctuate comma args)
        $+$ rparen
  where
    simple = hsep $ punctuate comma args

cgDef :: M.Map Name Int -> (Name, DDecl) -> Doc
cgDef ctors (n, DFun name' args body) =
    comment
    $+$ header
    $+$ indent (
            -- trace $+$  -- comment this line out to disable debug
            statements
            $+$ text "return" <+> retVal
        )
    $+$ text ""  -- empty line separating definitions
  where
    comment = text $ "# " ++ show name'
    header = text "def" <+> cgName n <> cgTuple (map cgName args) <> colon
    (statements, retVal) = evalState (runCG $ cgExp body) (CGState 1 ctors)

    trace = text "print" <+> text (show $ mangle n ++ "(" ++ argfmt ++ ")")
                <+> text "%" <+> cgTuple [text "repr" <> parens (cgName a) | a <- args]
    argfmt = intercalate ", " ["%s" | _ <- args]

cgVar :: LVar -> Doc
cgVar (Loc  i)
    | i >= 0    = text "loc" <> int i
    | otherwise = text "aux" <> int (-i)
cgVar (Glob n) = cgName n

cgError :: String -> Doc
cgError msg = text "idris_error" <> parens (text $ show msg)

cgExtern :: String -> [Doc] -> Doc
cgExtern "prim__null" args = text "None"
cgExtern n args = cgError $ "unimplemented external: " ++ n

(!) :: Doc -> String -> Doc
x ! i = x <> brackets (text i)

cgPrim :: PrimFn -> [Doc] -> Doc
cgPrim (LPlus  _) [x, y] = x <+> text "+" <+> y
cgPrim (LMinus _) [x, y] = x <+> text "-" <+> y
cgPrim (LTimes _) [x, y] = x <+> text "*" <+> y
cgPrim (LUDiv  _) [x, y] = x <+> text "/" <+> y
cgPrim (LSDiv  _) [x, y] = x <+> text "/" <+> y
cgPrim (LURem  _) [x, y] = x <+> text "%" <+> y
cgPrim (LSRem  _) [x, y] = x <+> text "%" <+> y

cgPrim (LEq    _) [x, y] = x <+> text "==" <+> y
cgPrim (LSLt   _) [x, y] = x <+> text "<" <+> y
cgPrim (LSExt _ _)[x]    = x

cgPrim (LIntStr _) [x] = text "str" <> parens x  
cgPrim (LStrInt _) [x] = text "int" <> parens x
cgPrim  LStrRev    [x] = x ! "::-1"
cgPrim  LStrConcat [x, y] = x <+> text "+" <+> y
cgPrim  LStrCons   [x, y] = x <+> text "+" <+> y
cgPrim  LStrEq     [x, y] = x <+> text "==" <+> y
cgPrim  LStrHead   [x] = x ! "0"
cgPrim  LStrTail   [x] = x ! "1:"

cgPrim  LWriteStr [world, s] = text "sys.stdout.write" <> parens s
cgPrim  LReadStr  _ = text "sys.stdin.readline()"

cgPrim (LExternal n) args = cgExtern (show n) args
cgPrim f args = cgError $ "unimplemented prim: " ++ show f ++ ", args = " ++ show args

cgConst :: Const -> Doc
cgConst (I i) = text $ show i
cgConst (BI i) = text $ show i
cgConst (Fl f) = text $ show f
cgConst (Ch c) = text $ show c
cgConst (Str s) = text $ show s
cgConst c = cgError $ "unimplemented constant: " ++ show c

cgComment :: String -> Doc
cgComment msg = text "#" <+> text msg

cgCtor :: Int -> Name -> [Expr] -> Expr
cgCtor tag n [] = parens (int tag <> comma) -- <+> cgComment (show n)
cgCtor tag n args =
  lparen <> int tag <> comma <+> cgComment (show n)
  $+$ indent (vcat $ punctuate comma args)
  $+$ rparen

cgAssign :: LVar -> Expr -> Stmts
cgAssign v e = cgVar v <+> text "=" <+> e

cgAssignN :: Name -> Expr -> Stmts
cgAssignN n e = cgName n <+> text "=" <+> e

cgMatch :: [LVar] -> LVar -> Stmts
cgMatch lhs rhs =
  hsep [cgVar v <> comma | v <- lhs]
  <+> text "="
  <+> cgVar rhs <> text "[1:]"

cgExp :: DExp -> CG Expr
cgExp (DV var) = return $ cgVar var
cgExp (DApp isTail n args) = do
    tag <- ctorTag n
    case tag of
        Just t  -> cgExp (DC Nothing t n args)
        Nothing -> cgApp (cgName n) <$> mapM cgExp args
cgExp (DLet n v e) = do
    emit . cgAssignN n =<< cgExp v
    cgExp e
cgExp (DUpdate n e) = return . cgError $ "unimplemented SUpdate for " ++ show n ++ " and " ++ show e
cgExp (DC _ tag n args) = cgCtor tag n <$> mapM cgExp args

cgExp (DCase caseType (DV var) alts) = cgCase var alts
cgExp (DCase caseType e alts) = do
    scrutinee <- fresh
    emit . cgAssign scrutinee =<< cgExp e
    cgCase scrutinee alts

cgExp (DChkCase (DV var) alts) = cgCase var alts
cgExp (DChkCase e alts) = do
    scrutinee <- fresh
    emit . cgAssign scrutinee =<< cgExp e
    cgCase scrutinee alts

cgExp (DProj e i) = do
    e <- cgExp e
    return $ e ! show (i+1)
cgExp (DConst c) = return $ cgConst c

cgExp (DForeign fdesc rdesc args) = return $ cgError "foreign not implemented"
cgExp (DOp prim args) = cgPrim prim <$> mapM cgExp args
cgExp  DNothing = return $ text "None"
cgExp (DError msg) = return $ cgError msg

cgCase :: LVar -> [DAlt] -> CG Expr
cgCase var [DDefaultCase e] = cgExp e
cgCase var alts = do
    retVar <- fresh
    mapM_ (cgAlt var retVar) (zip ("if" : repeat "elif") alts)
    emitUnreachableCase
    return $ cgVar retVar
  where
    emitUnreachableCase
        | (DDefaultCase _ : _) <- reverse alts
        = return ()

        | otherwise
        = emit $ text "else" <> colon $+$ indent (cgError "unreachable case")

cgAlt :: LVar -> LVar -> (String, DAlt) -> CG ()
cgAlt v retVar (if_, DConCase tag' ctorName [] e) = do
    Just tag <- ctorTag ctorName  -- in DExp, tag' above is always (-1)
    emit (
        text if_ <+> cgVar v <> text "[0] ==" <+> int tag <> colon
        <+> cgComment (show ctorName)
     )
    sindent $ do
        emit . cgAssign retVar =<< cgExp e

cgAlt v retVar (if_, DConCase tag' ctorName args e) = do
    Just tag <- ctorTag ctorName  -- in DExp, tag' above is always (-1)
    emit (
        text if_ <+> cgVar v <> text "[0] ==" <+> int tag <> colon
        <+> cgComment (show ctorName)
      )
    sindent $ do
        emit $ cgMatch (map Glob args) v
        emit . cgAssign retVar =<< cgExp e

cgAlt v retVar (if_, DConstCase c e) = do
    emit $ text if_ <+> cgVar v <+> text "==" <+> cgConst c <> colon
    sindent $
        emit . cgAssign retVar =<< cgExp e

cgAlt v retVar (if_, DDefaultCase e) = do
    emit $ text "else" <> colon
    sindent $
        emit . cgAssign retVar =<< cgExp e
