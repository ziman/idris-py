{-# LANGUAGE PatternGuards #-}
module IRTS.CodegenPython (codegenPython) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Data.Maybe
import Data.Char

import Text.PrettyPrint hiding (Str)

indent :: Doc -> Doc
indent = nest 2

pythonPreamble :: Doc
pythonPreamble = vcat . map text $
    [ "#!/usr/bin/env python"
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
    ]

mangle :: Name -> String
mangle = ("idris_" ++) . concatMap mangleChar . showCG
  where
    mangleChar x
        | isAlpha x || isDigit x = [x]
        | otherwise = "_" ++ show (ord x) ++ "_"

codegenPython :: CodeGenerator
codegenPython ci = writeFile (outputFile ci) (render source)
  where
    source = pythonPreamble $+$ definitions $+$ pythonLauncher
    definitions = vcat $ map cgDef (liftDecls ci)

cgName :: Name -> Doc
cgName = text . mangle

cgTuple :: [Doc] -> Doc
cgTuple xs = parens . hsep $ punctuate comma xs

cgBigTuple :: [Doc] -> Doc
cgBigTuple xs =
    lparen
    $$ (indent . vcat $ punctuate comma xs)
    $$ rparen

cgApp :: Doc -> [Doc] -> Doc
cgApp f args =
    (f <> lparen)
    $$ (indent . vcat $ punctuate comma args)
    $$ rparen

cgDef :: (Name, LDecl) -> Doc
cgDef (n, LConstructor name' tag arity) = empty
cgDef (n, LFun opts name' args body) = header $+$ indent (text "return" <+> cgExp body) $+$ text ""
  where
    header = text "def" <+> cgName n <> cgTuple (map cgName args) <> colon

cgVar :: LVar -> Doc
cgVar (Loc  i) = text "loc" <> int i
cgVar (Glob n) = cgName n <> text "()"

cgLam :: [Doc] -> LExp -> Doc
cgLam vars e =
  (lparen <> text "lambda" <+> hsep (punctuate comma vars) <> colon)
  $$ indent (cgExp e)
  $$ rparen

cgMatch :: [Name] -> Doc -> Doc -> Doc
cgMatch vars val body = 
  (lparen <> text "lambda" <+> hsep (punctuate comma $ map cgName vars) <> colon)
  $$ indent body
  $$ rparen <> lparen
  $$ indent val
  $$ rparen

cgLet :: [(Name, LExp)] -> LExp -> Doc
cgLet vars e =
  cgMatch
    (map fst vars)
    (vcat . punctuate comma $ map (cgExp . snd) vars)
    (cgExp e)

cgErr :: String -> Doc
cgErr msg = text "idris_raise" <> (parens . quotes) (text msg)

varScrutinee :: Name
varScrutinee = sUN "_case_scrutinee_"

varTag :: Name
varTag = sUN "_tag_"

cgError :: String -> Doc
cgError msg = text "idris_error" <> parens (text $ show msg)

cgExtern :: String -> [Doc] -> Doc
cgExtern "prim__null" args = text "None"
cgExtern n args = cgError $ "unimplemented external: " ++ n

cgPrim :: PrimFn -> [Doc] -> Doc
cgPrim (LPlus  _) [x, y] = x <+> text "+" <+> y
cgPrim (LMinus _) [x, y] = x <+> text "-" <+> y
cgPrim (LTimes _) [x, y] = x <+> text "*" <+> y
cgPrim (LUDiv  _) [x, y] = x <+> text "/" <+> y
cgPrim (LSDiv  _) [x, y] = x <+> text "/" <+> y
cgPrim (LURem  _) [x, y] = x <+> text "%" <+> y
cgPrim (LSRem  _) [x, y] = x <+> text "%" <+> y
cgPrim (LIntStr _) [x] = text "unicode" <> parens x  
cgPrim (LExternal n) args = cgExtern (show n) args
cgPrim f args = cgError $ "unimplemented prim: " ++ show f

cgConst :: Const -> Doc
cgConst (I i) = text $ show i
cgConst (BI i) = text $ show i
cgConst (Fl f) = text $ show f
cgConst (Ch c) = text $ show c
cgConst (Str s) = text $ show s
cgConst c = cgError $ "unimplemented constant: " ++ show c

cgExp :: LExp -> Doc
cgExp (LV var) = cgVar var
cgExp (LApp isTail (LV var) args) = cgApp (cgVar var) (map cgExp args)
cgExp (LApp isTail f args) = cgApp (parens $ cgExp f) (map cgExp args)
cgExp (LLazyApp n args) = cgApp (cgName n) (map cgExp args)
cgExp (LLazyExp e) = parens (text "lambda:" <+> cgExp e)
cgExp (LForce e) = cgExp e <> text "()"
cgExp (LLet n v e) = cgLet [(n, v)] e
cgExp (LLam ns e) = cgLam (map cgName ns) e
cgExp (LProj e i) = parens (cgExp e) <> brackets (int $ i + 1)
cgExp (LCon _ tag n args) = cgTuple (int tag : map cgExp args)
cgExp (LCase caseType (LV var) alts) = cgCase var alts
cgExp (LCase caseType e alts) = cgLet [(varScrutinee, e)] $ LCase caseType (LV $ Glob varScrutinee) alts
cgExp (LConst c) = cgConst c
cgExp (LForeign fdesc rdesc args) = cgError "foreign not implemented"
cgExp (LOp prim args) = cgPrim prim (map cgExp args)
cgExp  LNothing = text "None"
cgExp (LError msg) = cgError msg

cgCase :: LVar -> [LAlt] -> Doc
cgCase var [] = cgError "no case alternatives"
cgCase var alts = lparen
    $+$ vcat (addLastAlt $ map (cgAlt var) alts)
    $+$ rparen
  where
    addLastAlt
        | (LDefaultCase _ : _) <- reverse alts = id
        | otherwise = (++ [indent $ cgError "unreachable case"])

cgAlt :: LVar -> LAlt -> Doc
cgAlt v (LConCase tag ctorName args e) =
  indent (cgMatch (varTag : args) (cgVar v) (cgExp e))
  $+$ rparen <+> text "if" <+> cgVar v <> text "[0] ==" <+> int tag <+> text "else" <+> lparen
cgAlt v (LConstCase c e) =
  indent (cgExp e)
  $+$ rparen <+> text "if" <+> cgVar v <+> text "==" <+> cgConst c <+> text "else" <+> lparen
cgAlt v (LDefaultCase e) = indent $ cgExp e

php :: ()
php = ()
  where
    phpname :: Name -> String
    phpname _ = ""

    errCode = "function error($str) { echo \"$str\\n\"; exit(0); }"
    doEcho = "function idris_writeStr($str) { echo \"$str\"; }"
    doRead = "function idris_readStr() { return fgets(STDIN); }"
    doAppend = "function idris_append($l, $r) { return ($l . $r); }"
    mkStr = "function mkStr($l) { return array($l); }"

    var :: Name -> String
    var n = "$" ++ phpname n

    loc :: Int -> String
    loc i = "$loc" ++ show i

    doCodegen :: (Name, SDecl) -> String
    doCodegen (n, SFun _ args i def) = cgFun n args def

    cgFun :: Name -> [Name] -> SExp -> String
    cgFun n args def 
        = "function " ++ phpname n ++ "("
                      ++ showSep "," (map (loc . fst) (zip [0..] args)) ++ ") {\n"
                      ++ cgBody doRet def ++ "\n}\n\n"
      where doRet :: String -> String -- Return the calculated expression
            doRet str = "return " ++ str ++ ";"

    -- cgBody converts the SExp into a chunk of php which calculates the result
    -- of an expression, then runs the function on the resulting bit of code.
    --
    -- We do it this way because we might calculate an expression in a deeply nested
    -- case statement, or inside a let, etc, so the assignment/return of the calculated
    -- expression itself may happen quite deeply.

    cgBody :: (String -> String) -> SExp -> String
    cgBody ret (SV (Glob n)) = ret $ phpname n ++ "()"
    cgBody ret (SV (Loc i)) = ret $ loc i 
    cgBody ret (SApp _ f args) = ret $ phpname f ++ "(" ++ 
                                       showSep "," (map cgVar args) ++ ")"
    cgBody ret (SLet (Loc i) v sc)
       = cgBody (\x -> loc i ++ " = " ++ x ++ ";\n") v ++
         cgBody ret sc
    cgBody ret (SUpdate n e)
       = cgBody ret e
    cgBody ret (SProj e i)
       = ret $ cgVar e ++ "[" ++ show (i + 1) ++ "]"
    cgBody ret (SCon _ t n args)
       = ret $ "array(" ++ showSep "," 
                  (show t : (map cgVar args)) ++ ")"
    cgBody ret (SCase _ e alts)
       = let scrvar = cgVar e 
             scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
           "switch(" ++ scr ++ ") {\n"
             ++ showSep "\nbreak;\n" (map (cgAlt ret scrvar) alts) ++ "\n}"
      where conCase (SConCase _ _ _ _ _) = True
            conCase _ = False
    cgBody ret (SChkCase e alts)
       = let scrvar = cgVar e 
             scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
           "switch(" ++ scr ++ ") {\n"
             ++ showSep "\nbreak;\n" (map (cgAlt ret scrvar) alts) ++ "\n}"
      where conCase (SConCase _ _ _ _ _) = True
            conCase _ = False
    cgBody ret (SConst c) = ret $ cgConst c
    cgBody ret (SOp op args) = ret $ cgOp op (map cgVar args)
    cgBody ret SNothing = ret "0"
    cgBody ret (SError x) = ret $ "error( " ++ show x ++ ")"
    cgBody ret _ = ret $ "error(\"NOT IMPLEMENTED!!!!\")"

    cgAlt :: (String -> String) -> String -> SAlt -> String
    cgAlt ret scr (SConstCase t exp)
       = "case " ++ show t ++ ":\n" ++ cgBody ret exp
    cgAlt ret scr (SDefaultCase exp) = "default:\n" ++ cgBody ret exp
    cgAlt ret scr (SConCase lv t n args exp)
       = "case " ++ show t ++ ":\n"
                 ++ project 1 lv args ++ "\n" ++ cgBody ret exp
       where project i v [] = ""
             project i v (n : ns) = loc v ++ " = " ++ scr ++ "[" ++ show i ++ "]; "
                                      ++ project (i + 1) (v + 1) ns

    cgVar :: LVar -> String
    cgVar (Loc i) = loc i 
    cgVar (Glob n) = var n

    cgConst :: Const -> String
    cgConst (I i) = show i
    cgConst (Ch i) = show i
    cgConst (BI i) = show i
    cgConst (Str s) = show s
    cgConst TheWorld = "0"
    cgConst x | isTypeConst x = "0"
    cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"

    cgOp :: PrimFn -> [String] -> String
    cgOp (LPlus (ATInt _)) [l, r] 
         = "(" ++ l ++ " + " ++ r ++ ")"
    cgOp (LMinus (ATInt _)) [l, r] 
         = "(" ++ l ++ " - " ++ r ++ ")"
    cgOp (LTimes (ATInt _)) [l, r] 
         = "(" ++ l ++ " * " ++ r ++ ")"
    cgOp (LEq (ATInt _)) [l, r] 
         = "(" ++ l ++ " == " ++ r ++ ")"
    cgOp (LSLt (ATInt _)) [l, r] 
         = "(" ++ l ++ " < " ++ r ++ ")"
    cgOp (LSLe (ATInt _)) [l, r] 
         = "(" ++ l ++ " <= " ++ r ++ ")"
    cgOp (LSGt (ATInt _)) [l, r] 
         = "(" ++ l ++ " > " ++ r ++ ")"
    cgOp (LSGe (ATInt _)) [l, r] 
         = "(" ++ l ++ " >= " ++ r ++ ")"
    cgOp LStrEq [l,r] = "(" ++ l ++ " == " ++ r ++ ")"
    cgOp (LIntStr _) [x] = x
    cgOp (LChInt _) [x] = x
    cgOp (LIntCh _) [x] = x
    cgOp (LSExt _ _) [x] = x
    cgOp (LTrunc _ _) [x] = x
    cgOp LWriteStr [_,str] = "idris_writeStr(" ++ str ++ ")"
    cgOp LReadStr [_] = "idris_readStr()"
    cgOp LStrConcat [l,r] = "idris_append(" ++ l ++ ", " ++ r ++ ")"
    cgOp LStrCons [l,r] = "idris_append(" ++ l ++ ", " ++ r ++ ")"
    cgOp op exps = "error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"
       -- error("Operator " ++ show op ++ " not implemented")



