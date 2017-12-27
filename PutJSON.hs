module PutJSON where

import Data.List
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString)     = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null" 
