module Prettify
  (
    Doc
  ) where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Eq, Show)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text ""  = Empty
text str = Text str

double :: Double -> Doc
double num = text (show num)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
x <> Empty = x
Empty <> y = y
x <> y     = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep xs = undefined

-- to build a fold exclusively for Doc, note that Empty is the
-- identity for concatenation.
fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

compact :: Doc -> String
compact x = undefined

pretty :: Int -> Doc -> String
pretty width x = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []       = []
punctuate p [d]      = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
