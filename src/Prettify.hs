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

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined

compact :: Doc -> String
compact x = undefined

pretty :: Int -> Doc -> String
pretty width x = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []       = []
punctuate p [d]      = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
