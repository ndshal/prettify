module Prettify
  (
    Doc,
    empty,
    char,
    text,
    double,
    line,
    (<>),
    (</>),
    hcat,
    fsep,
    punctuate,
    compact,
    pretty
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

-- softline should insert a Line [break] if the document is too wide,
-- and otherwise a space. The way to do this is to keep a Union of
-- two potential states.
softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

-- note that flatten always gets called on the left element of a Union:
-- the left subtree of a Union is always at least as long as the right
flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

(<>) :: Doc -> Doc -> Doc
x <> Empty = x
Empty <> y = y
x <> y     = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

fsep :: [Doc] -> Doc
fsep = fold (</>)

-- to build a fold exclusively for Doc, note that Empty is the
-- identity for concatenation.
fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

-- compact toString method. Treats all softlines as spaces.
compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                  Empty        -> transform ds
                  Char c       -> c : transform ds
                  Text s       -> s ++ transform ds
                  Line         -> '\n' : transform ds
                  a `Concat` b -> transform (a:b:ds)
                  _ `Union` b  -> transform (b:ds)

-- simple formatted toString method.
-- The Int parameter is the line / page width.
pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                  Empty        -> best col ds
                  Char c       -> c : best (col + 1) ds
                  Text s       -> s ++ best (col + length s) ds
                  Line         -> '\n' : best 0 ds
                  a `Concat` b -> best col (a:b:ds)
                  a `Union` b  -> nicest col (best col (a:ds))
                                             (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

-- helper method to determine whether a given number of columns
-- fits a string
fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []       = []
punctuate p [d]      = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
