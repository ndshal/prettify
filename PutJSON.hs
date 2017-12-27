module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString)     = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (k, v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values ++ "]"
    where values [] = ""
          values vs = intercalate ", " (map renderJValue vs)
