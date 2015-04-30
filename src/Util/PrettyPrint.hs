-- This module is equivalent to Text.PrettyPrint.
-- The only difference is slightly different indentation behaviour.
-- (Plus support of code comments).

module Util.PrettyPrint
    ( Doc
    , int, text
    , comma, colon, lparen, rparen, lbracket, rbracket
    , (<>), (<+>), ($+$), ($$)
    , (<?>)
    , nest
    , parens, brackets
    , empty
    , render
    , vcat, hsep
    , punctuate
    , size, width
    )
    where

import qualified Data.Text.Lazy as T

type Line = (T.Text, T.Text)  -- text, comment
newtype Doc = Doc [Line]
instance Show Doc where
    show = T.unpack . render "--"

infixr 6 <>, <+>
infixr 5 $$, $+$
infixl 1 <?>

int :: Int -> Doc
int i = text $ show i

text :: String -> Doc
text s = Doc [(T.pack s, T.empty)]

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

-- | Add a comment to the first line of the Doc.
(<?>) :: Doc -> String -> Doc
Doc [] <?> comment = Doc [(T.empty, T.pack $ comment)]
Doc ((t,c) : lines) <?> comment = Doc $ (t, merge (T.pack comment) c) : lines
  where
    merge x y
        | T.null x = y
        | T.null y = x
        | otherwise = x +++ T.pack " (" +++ y +++ T.pack ")"

infixr 3 +++
(+++) :: T.Text -> T.Text -> T.Text
(+++) = T.append

meld :: String -> [Line] -> [Line] -> [Line]
meld sep [] ys = ys
meld sep xs [] = xs
meld sep [(x,xc)] ((y,yc) : ys) = (x +++ T.pack sep +++ y, merge xc yc) : ys
  where
    merge x y
        | T.null x = y
        | T.null y = x
        | otherwise = x +++ T.pack ", " +++ y
meld sep (x : xs) ys = x : meld sep xs ys

nest :: Int -> Doc -> Doc
nest n (Doc xs) = Doc [(T.replicate (fromIntegral n) (T.pack " ") +++ t, c) | (t, c) <- xs]

parens :: Doc -> Doc
parens d = lparen <> d <> rparen

brackets :: Doc -> Doc
brackets d = lbracket <> d <> rbracket

render :: String -> Doc -> T.Text
render cmtStr (Doc xs) = T.unlines $ map (renderLine $ T.pack cmtStr) xs

renderLine :: T.Text -> Line -> T.Text
renderLine sep (content, comment)
    | T.null content && T.null comment = T.empty
    | T.null content = sep +++ space +++ comment
    | T.null comment = content
    | otherwise = content +++ space +++ space +++ sep +++ space +++ comment
  where
    space = T.pack " "

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
size (Doc xs) = fromIntegral $ sum [T.length t | (t, c) <- xs]

width :: Doc -> Int
width (Doc xs) = fromIntegral $ maximum [T.length t | (t, c) <- xs]
