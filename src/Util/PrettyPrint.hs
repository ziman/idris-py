module Util.PrettyPrint
    ( Doc
    , int, text
    , comma, colon, lparen, rparen, lbracket, rbracket
    , (<>), (<+>), ($+$), ($$)
    , nest
    , parens, brackets
    , empty
    , render
    , vcat, hsep
    , punctuate
    , size, width
    )
    where

-- This module is equivalent to Text.PrettyPrint.
-- The only difference is slightly different indentation behaviour.

newtype Doc = Doc [String]
instance Show Doc where
    show = render

infixr 6 <>, <+>
infixr 5 $$, $+$

int :: Int -> Doc
int i = text $ show i

text :: String -> Doc
text s = Doc [s]

comma, colon, lparen, rparen, lbracket, rbracket :: Doc
comma    = text ","
colon    = text ":"
lparen   = text "("
rparen   = text ")"
lbracket = text "["
rbracket = text "]"

(<>) :: Doc -> Doc -> Doc
Doc xs <> Doc ys = Doc $ meld "" xs ys

(<+>) :: Doc -> Doc -> Doc
Doc xs <+> Doc ys = Doc $ meld " " xs ys

($+$) :: Doc -> Doc -> Doc
Doc xs $+$ Doc ys = Doc $ xs ++ ys

($$) :: Doc -> Doc -> Doc
($$) = ($+$)

meld :: String -> [String] -> [String] -> [String]
meld sep [] ys = ys
meld sep xs [] = xs
meld sep [x] (y : ys) = (x ++ sep ++ y) : ys
meld sep (x : xs) ys = x : meld sep xs ys

nest :: Int -> Doc -> Doc
nest n (Doc xs) = Doc $ map (replicate n ' ' ++) xs

parens :: Doc -> Doc
parens d = lparen <> d <> rparen

brackets :: Doc -> Doc
brackets d = lbracket <> d <> rbracket

render :: Doc -> String
render (Doc xs) = unlines xs

empty :: Doc
empty = Doc []

vcat :: [Doc] -> Doc
vcat = foldr ($+$) empty

hsep :: [Doc] -> Doc
hsep = foldr (<+>) empty

punctuate :: Doc -> [Doc] -> [Doc]
punctuate sep [] = []
punctuate sep [x] = [x]
punctuate sep (x : xs) = (x <> sep) : punctuate sep xs

size :: Doc -> Int
size (Doc xs) = sum $ map length xs

width :: Doc -> Int
width (Doc xs) = maximum $ map length xs
