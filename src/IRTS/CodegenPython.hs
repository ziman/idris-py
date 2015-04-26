{-# LANGUAGE PatternGuards #-}
module IRTS.CodegenPython (codegenPython) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import IRTS.Defunctionalise
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
    ]

mangle :: Name -> String
mangle = ("idris_" ++) . concatMap mangleChar . showCG
  where
    mangleChar x
        | isAlpha x || isDigit x = [x]
        | otherwise = "_" ++ show (ord x) ++ "_"

-- simpleDecls / defunDecls / liftDecls
codegenPython :: CodeGenerator
codegenPython ci = writeFile (outputFile ci) (render source)
  where
    source = pythonPreamble $+$ definitions $+$ pythonLauncher
    definitions = vcat $ map cgDef (simpleDecls ci)

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

cgDef :: (Name, SDecl) -> Doc
cgDef (n, SFun name' args _ body) =
    comment $+$ header $+$ indent (cgExp (text "return" <+>) body) $+$ text ""
  where
    comment = text $ "# " ++ show name'
    header = text "def" <+> cgName n <> cgTuple args' <> colon
    args' = [cgVar (Loc i) | (i, n) <- zip [0..] args]

cgVar :: LVar -> Doc
cgVar (Loc  i)
    | i >= 0    = text "loc" <> int i
    | otherwise = text "aux" <> int (-i)
cgVar (Glob n) = cgName n <> text "()"

cgMatch :: [LVar] -> Doc -> Doc -> Doc
cgMatch vars val body = 
  (lparen <> text "lambda" <+> hsep (punctuate comma $ map cgVar vars) <> colon)
  $$ indent body
  $$ rparen <> lparen
  $$ indent val
  $$ rparen

cgErr :: String -> Doc
cgErr msg = text "idris_raise" <> (parens . quotes) (text msg)

varScrutinee :: Name
varScrutinee = sUN "_case_scrutinee_"

varTag :: LVar
varTag = Loc (-1)

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
cgPrim (LIntStr _) [x] = text "str" <> parens x  
cgPrim (LExternal n) args = cgExtern (show n) args
cgPrim  LWriteStr [world, s] = text "sys.stdout.write" <> parens s
cgPrim  LStrConcat [x, y] = x <+> text "+" <+> y
cgPrim f args = cgError $ "unimplemented prim: " ++ show f ++ ", args = " ++ show args

cgConst :: Const -> Doc
cgConst (I i) = text $ show i
cgConst (BI i) = text $ show i
cgConst (Fl f) = text $ show f
cgConst (Ch c) = text $ show c
cgConst (Str s) = text $ show s
cgConst c = cgError $ "unimplemented constant: " ++ show c

ret :: Doc -> Doc
ret x = text "return" <+> x

cgExp :: (Doc -> Doc) -> SExp -> Doc
cgExp ret (SV var) = ret $ cgVar var
cgExp ret (SApp isTail n args) = ret $ cgApp (cgName n) (map cgVar args)
cgExp ret (SLet n v e) =
  cgExp (\code -> cgVar n <+> text "=" <+> code) v
  $+$ cgExp ret e
cgExp ret (SUpdate n e) = ret . cgError $ "unimplemented SUpdate for " ++ show n ++ " and " ++ show e
cgExp ret (SCon _ tag n []) = ret $ parens (int tag <> comma)
cgExp ret (SCon _ tag n args) = ret $ cgTuple (int tag : map cgVar args)
cgExp ret (SCase caseType var alts) = cgCase ret var alts
cgExp ret (SChkCase var alts) = cgCase ret var alts
cgExp ret (SProj var i) = ret (cgVar var <> brackets (int i))
cgExp ret (SConst c) = ret $ cgConst c

cgExp ret (SForeign fdesc rdesc args) = ret $ cgError "foreign not implemented"
cgExp ret (SOp prim args) = ret $ cgPrim prim (map cgVar args)
cgExp ret  SNothing = ret $ text "None"
cgExp ret (SError msg) = ret $ cgError msg

cgCase :: (Doc -> Doc) -> LVar -> [SAlt] -> Doc
cgCase ret var alts =
    vcat (map (cgAlt ret var) alts)
    $+$ ret (cgError "unreachable case")

cgAlt :: (Doc -> Doc) -> LVar -> SAlt -> Doc
cgAlt ret v (SConCase lv tag ctorName args e) =
  text "if" <+> cgVar v <> text "[0] ==" <+> int tag <> colon
  $+$ indent (
    text "_tag" <> comma
    <+> (hsep [cgVar (Loc i) <> comma | (i, _) <- zip [lv..] args])
    <+> text "="
    <+> cgVar v
    $+$ cgExp ret e
  )

cgAlt ret v (SConstCase c e) =
  text "if" <+> cgVar v <+> text "==" <+> cgConst c <> colon
  $+$ indent (cgExp ret e)

cgAlt ret v (SDefaultCase e) = cgExp ret e
