module PrettyJSON where

import Prettify

renderJValue :: JValue -> Doc
renderJValue (JString str) = string str
renderJValue (JNumber num) = double num
renderJValue (JBool False) = text "false"
renderJValue (JBool True)  = text "true"
renderJValue JNull         = text "null"
