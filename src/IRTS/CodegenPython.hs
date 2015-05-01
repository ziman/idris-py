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
import Data.Ord
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Monad
import Control.Applicative hiding (empty, Const)
import Control.Monad.Trans.State.Lazy

import Util.PrettyPrint

-- Codegen state within one Decl
data CGState = CGState
    { varCounter :: Int          -- for fresh variables
    , ctorTags :: M.Map Name Int -- constructor tags
    , curFun :: (Name, [Name])   -- name, args for tail calls
    }
    deriving (Show)

-- Make the distinction for the humans reading the code
type Stmts = Doc  -- statements
type Expr  = Doc  -- expressions

-- A code generator for "a" generates:
--
--   1. statements that prepare context for "a"
--   2. expression that stands for "a" afterwards
--
-- Note that (1.) is not part of the state;
-- it's a part of the what the given DExp translates to.
--
-- (2.) is always an expression in Python
-- and therefore composes into bigger expressions easily,
-- while (1.) are statements and they can only be sequenced.
--
-- The idea is that, for example, f(x, y) compiles to:
--   x_statements
--   y_statements
--   f(x_expr, y_expr)
--
newtype CG a = CG { runCG :: State CGState (Stmts, a) }

-- Now let's say how Functor/Applicative/Monad deal
-- with the division into (Stmt, Expr)
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
    CGState vc ctors cf <- get
    put $ CGState (vc + 1) ctors cf
    return (empty, Loc (-vc))

ctorTag :: Name -> CG (Maybe Int)
ctorTag n = CG $ do
    CGState vc ctors cf <- get
    return (empty, M.lookup n ctors)

currentFn :: CG (Name, [Name])
currentFn = CG $ do
    CGState vc ctors cf <- get
    return (empty, cf)

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
    , "MODULES = dict()"
    , ""
    , "def idris_pymodule(name):"
    , "  mod = MODULES.get(name)"
    , "  if mod is None:"
    , "    mod = __import__(name)"
    , "    MODULES[name] = mod"
    , "  return mod"
    , ""
    , "def idris_getfield(o, f):"
    , "  try:"
    , "    return o.__getattribute__(f)"
    , "  except AttributeError:"
    , "    # it's a module"
    , "    return o.__dict__[f]"
    , ""
    , "def idris_call(f, args):"
    , "  native_args = []"
    , "  while len(args) == 3:  # it's a cons"
    , "    native_args.append(args[1])"
    , "    args = args[2]"
    , "  return f(*native_args)"
    , ""
    , "def idris_foreach(it, st, f):"
    , "  for x in it:"
    , "    # Apply st, x, world"
    , "    st = APPLY0(APPLY0(APPLY0(f, st), x), None)"
    , "  return st"
    , ""
    , "def idris_is_none(x):"
    , "  return 1 if x is None else 0"
    , ""
    , "def idris_try(f, fail, succ):"
    , "  try:"
    , "    result = APPLY0(f, None)  # apply to world"
    , "    return APPLY0(succ, result)"
    , "  except Exception as e:"
    , "    return APPLY0(APPLY0(fail, e.__class__.__name__), e)"
    , ""
    , "def idris_raise(e):"
    , "  raise e"
    , ""
    ]

pythonLauncher :: Doc
pythonLauncher =
    text "if __name__ == '__main__':"
    $+$ indent (cgApp (cgName $ sMN 0 "runMain") [])

mangle :: Name -> String
mangle n = "idris_" ++ concatMap mangleChar (showCG n)
  where
    mangleChar x
        | isAlpha x || isDigit x = [x]
        | otherwise = "_" ++ show (ord x) ++ "_"

-- We could generate from:
-- simpleDecls / defunDecls / liftDecls
codegenPython :: CodeGenerator
codegenPython ci = writeFile (outputFile ci) (render "#" source)
  where
    source = pythonPreamble $+$ definitions $+$ pythonLauncher
    ctors = M.fromList [(n, tag) | (n, DConstructor n' tag arity) <- defunDecls ci]
    definitions = vcat $ map (cgDef ctors) [d | d@(_, DFun _ _ _) <- defunDecls ci]

-- Let's not mangle /that/ much. Especially function parameters
-- like e0 and e1 are nicer when readable.
cgName :: Name -> Expr
cgName (MN i n) | all (\x -> isAlpha x || x `elem` "_") (T.unpack n)
    = text $ T.unpack n ++ show i
cgName n = text (mangle n)  -- <?> show n  -- uncomment this to get a comment for *every* mangled name

bigParens :: Doc -> Doc
bigParens d = lparen $+$ indent d $+$ rparen

bigBraces :: Doc -> Doc
bigBraces d = lbrace $+$ indent d $+$ rbrace

cgTuple :: Int -> [Expr] -> Expr
cgTuple maxSize [] = parens empty  -- don't split empty tuples
cgTuple maxSize xs
    | size oneLiner <= maxSize = oneLiner
    | allSmall  = bigParens $ vsepLines lines  -- for arg lists where every item is just a token
    | otherwise = bigParens $ vsepLines xs
  where
    oneLiner = parens (hsep $ punctuate comma xs)
    vsepLines = vcat . punctuate comma
    allSmall = and [size x < 8 | x <- xs]
    lines = wrapLines 60 empty xs

    wrapLines :: Int -> Doc -> [Doc] -> [Doc]
    wrapLines w curLine []
        | size curLine == 0 = []
        | otherwise         = [curLine]
    wrapLines w curLine (x : xs)
        | curSize >= w = curLine : wrapLines w x xs
        | curSize == 0 = wrapLines w x xs
        | otherwise = wrapLines w (curLine <> comma <+> x) xs
      where
        curSize = size curLine

cgApp :: Expr -> [Expr] -> Expr
cgApp f args = f <> cgTuple maxWidth args
  where
    maxWidth = 80 - width f

-- Process one definition. The caller deals with constructor declarations,
-- we only deal with function definitions.
cgDef :: M.Map Name Int -> (Name, DDecl) -> Doc
cgDef ctors (n, DFun name' args body) =
    (empty <?> show name')
    $+$ (text "def" <+> cgName n <> cgTuple maxArgsWidth (map cgName args) <> colon)
    $+$ indent (
        text "while" <+> text "True" <> colon  -- for tail calls
        $+$ indent (
                -- trace $+$  -- uncomment this line to enable printing traces
                statements
                $+$ text "return" <+> retVal
            )
        )
    $+$ text ""  -- empty line separating definitions
  where
    maxArgsWidth = 80 - width (cgName n)
    (statements, retVal) = evalState body' initState
    body' = runCG . cgExp . tailify n $ body
    initState = CGState 1 ctors (n, args)

    -- used only for debugging
    trace = text "print" <+> text (show $ mangle n ++ "(" ++ argfmt ++ ")")
                <+> text "%" <+> cgTuple 80 [text "repr" <> parens (cgName a) | a <- args]
    argfmt = intercalate ", " ["%s" | _ <- args]

-- Mark tail-calls as such.
tailify :: Name -> DExp -> DExp
tailify n (DLet n' v e) = DLet n' v (tailify n e)
tailify n (DCase ct e alts) = DCase ct e (map (tailifyA n) alts)
tailify n (DChkCase e alts) = DChkCase e (map (tailifyA n) alts)
tailify n e@(DApp isTail n' args)
    | n' == n = DApp True n' args
tailify n e = e

tailifyA :: Name -> DAlt -> DAlt
tailifyA n (DConCase tag cn args e) = DConCase tag cn args (tailify n e)
tailifyA n (DConstCase c e) = DConstCase c (tailify n e)
tailifyA n (DDefaultCase e) = DDefaultCase (tailify n e)

cgVar :: LVar -> Expr
cgVar (Loc  i)
    | i >= 0    = text "loc" <> int i
    | otherwise = text "aux" <> int (-i)
cgVar (Glob n) = cgName n

cgError :: String -> Expr
cgError msg = text "idris_error" <> parens (text $ show msg)

cgExtern :: String -> [Expr] -> Expr
cgExtern "prim__null" args = text "None"
cgExtern n args = cgError $ "unimplemented external: " ++ n

-- Notation for python bracketed[indexing].
(!) :: Expr -> String -> Expr
x ! i = x <> brackets (text i)

cgPrim :: PrimFn -> [Expr] -> Expr
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

cgConst :: Const -> Expr
cgConst (I i) = text $ show i
cgConst (BI i) = text $ show i
cgConst (Fl f) = text $ show f
cgConst (Ch c) = text $ show c
cgConst (Str s) = text $ show s
cgConst c = cgError $ "unimplemented constant: " ++ show c

cgCtor :: Int -> Name -> [Expr] -> Expr
cgCtor tag n [] = parens (int tag <> comma) <?> show n
cgCtor tag n args = cgTuple 80 $ (int tag <?> show n) : args

cgAssign :: LVar -> Expr -> Stmts
cgAssign v e = cgVar v <+> text "=" <+> e

cgAssignN :: Name -> Expr -> Stmts
cgAssignN n e = cgName n <+> text "=" <+> e

cgAssignMany :: [Name] -> [Expr] -> Stmts
cgAssignMany ns es =
  hsep [cgName n <> comma | n <- ns]
  <+> text "="
  <+> hsep [e <> comma | e <- es]

-- pattern-matching / tuple decomposition
cgMatch :: [LVar] -> LVar -> Stmts
cgMatch lhs rhs =
  hsep [cgVar v <> comma | v <- lhs]
  <+> text "="
  <+> cgVar rhs <> text "[1:]"

cgTailCall :: [Name] -> [Expr] -> CG Expr
cgTailCall argNames args = do
    emit $ cgAssignMany argNames args
    emit $ text "continue"
    return $ cgError "unreachable due to tail call"

cgExp :: DExp -> CG Expr
cgExp (DV var) = return $ cgVar var
cgExp (DApp isTailCall n args) = do
    tag <- ctorTag n
    case tag of
        Just t  -> cgExp (DC Nothing t n args)  -- application of ctor
        Nothing -> do
            (curFn, argNames) <- currentFn
            if isTailCall && n == curFn
               then cgTailCall argNames =<< mapM cgExp args  -- tail call!
               else cgApp (cgName n) <$> mapM cgExp args     -- ordinary call

cgExp (DLet n v e) = do
    emit . cgAssignN n =<< cgExp v
    cgExp e

cgExp (DUpdate n e) = return . cgError $ "unimplemented SUpdate for " ++ show n ++ " and " ++ show e

cgExp (DC _ tag n args) = cgCtor tag n <$> mapM cgExp args

-- if the scrutinee is something big, save it into a variable
-- because we'll copy it into a possibly long chain of if-elif-...
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

cgExp (DForeign fdesc (FStr fn) args) = cgApp (text fn) <$> mapM (cgExp . snd) args
cgExp (DForeign fdesc rdesc args) = error $ "unrecognised foreign: " ++ show (fdesc, rdesc, args)
cgExp (DOp prim args) = cgPrim prim <$> mapM cgExp args
cgExp  DNothing = return $ text "None"
cgExp (DError msg) = return $ cgError msg

ifElif :: [String]
ifElif = "if" : repeat "elif"

-- We assume that all tags are different here
cgAltTree :: Int -> Int -> LVar -> LVar -> [(Int, DAlt)] -> CG ()
cgAltTree groupSize altCount retVar scrutinee alts
    | altCount > groupSize
    = do
        emit $ text "if" <+> cgVar scrutinee <> text "[0] <" <+> int firstHi <> colon
        sindent $ cgAltTree groupSize lo retVar scrutinee (take lo alts)
        emit $ text "else" <> colon
        sindent $ cgAltTree groupSize (altCount - lo) retVar scrutinee (drop lo alts)
  where
    lo = altCount `div` 2
    firstHi = fst (alts !! lo)

cgAltTree groupSize altCount retVar scrutinee alts
    = mapM_ (cgAlt scrutinee retVar) (zip ifElif $ map snd alts)

cgDictCase :: LVar -> [(Const, Expr)] -> [Expr] -> Expr
cgDictCase scrutinee items dflt =
    bigBraces (vcat $ punctuate comma items')
    <> case dflt of
        []  -> brackets $ cgVar scrutinee
        d:_ -> text ".get" <> parens (cgVar scrutinee <> comma <+> d)
  where
    items' = [ cgConst c <> colon <+> e | (c, e) <- items]

-- For case-expressions, we:
-- 1. generate a fresh var
-- 2. emit statements containing an if-elif-... chain that assigns to the var
-- 3. use the assigned var as the expression standing for the result
cgCase :: LVar -> [DAlt] -> CG Expr
cgCase var [DDefaultCase e] = cgExp e

-- compile big constant-cases into dict lookups
cgCase var alts
    | length alts > 8
    , all isConstant alts = do
        exprs <- mapM cgExp [e | DConstCase c e <- alts]        
        dflt  <- mapM cgExp [e | DDefaultCase e <- alts]
        return $ cgDictCase
            var
            (zip [c | DConstCase c e <- alts] exprs)
            dflt
  where
    isConstant :: DAlt -> Bool
    isConstant (DConstCase _ _) = True
    isConstant (DDefaultCase _) = True
    isConstant _ = False

-- compile big constructor-cases into binary search on tags
cgCase var alts
    | altCount >= 2 * groupSize  -- there would be at least 2 full groups
    , DDefaultCase def : alts' <- reverse alts
    , all isConCase alts' = do
        retVar <- fresh
        taggedAlts <- mapM getTag alts'
        cgAltTree groupSize altCount retVar var
            $ sortBy (comparing fst) taggedAlts
        return $ cgVar retVar
  where
    groupSize = 3  -- smallest group size: (groupSize+1) `div` 2
    altCount = length alts

    isConCase :: DAlt -> Bool
    isConCase (DConCase _ _ _ _) = True
    isConCase _ = False

    getTag :: DAlt -> CG (Int, DAlt)
    getTag alt@(DConCase _ n _ _) = do
        Just tag <- ctorTag n
        return (tag, alt)

-- otherwise just do the linear if-elif thing
cgCase var alts = do
    retVar <- fresh
    mapM_ (cgAlt var retVar) (zip ifElif alts)
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
    -- DConCase does not contain useful tags yet
    -- we need to find out by looking up by name
    Just tag <- ctorTag ctorName
    emit (
        text if_ <+> cgVar v <> text "[0] ==" <+> int tag <> colon
        <?> show ctorName
     )
    sindent $ do
        emit . cgAssign retVar =<< cgExp e

cgAlt v retVar (if_, DConCase tag' ctorName args e) = do
    -- DConCase does not contain useful tags yet
    -- we need to find out by looking up by name
    Just tag <- ctorTag ctorName
    emit (
        text if_ <+> cgVar v <> text "[0] ==" <+> int tag <> colon
        <?> show ctorName
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
