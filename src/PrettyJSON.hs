module PrettyJSON
    (
      renderJValue
    ) where

import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Numeric (showHex)

import JSONClass (JValue(..))
import Prettify (Doc, (<>), char, text, double, hcat, fsep, punctuate,
                 fill, nest, compact, pretty)

renderJValue :: JValue -> Doc
renderJValue (JString str) = string str
renderJValue (JNumber num) = double num
renderJValue (JBool False) = text "false"
renderJValue (JBool True)  = text "true"
renderJValue JNull         = text "null"
renderJValue (JArray ary)  = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' renderField obj
    where renderField (name, val) = string name
                                 <> text ": "
                                 <> renderJValue val

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close renderItem = enclose open close
                             . fsep . punctuate (char ',') . map renderItem

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x = text "\\u"
          <> text (replicate (4 - length h) '0')
          <> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
    where d = ord c
