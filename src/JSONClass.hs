{-# LANGUAGE TypeSynonymInstances #-}

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue   = id
    fromJValue = Right

instance JSON Bool where
    toJValue             = JBool
    fromJValue (JBool b) = Right b
    fromJValue _         = Left "not a JSON boolean"
{-
 Without the TypeSynonymInstances pragma, the following code would break
 at compile time - String is a synonym for [Char], which is in turn [a]
 with Char substituted for a. The base Haskell 98 report does not allow
 parametric types with a specific param value to be instances - that is
 we could write an instance for [a] but not for [Char].
-}
instance JSON String where
    toJValue               = JString
    fromJValue (JString s) = Right s
    fromJValue _           = Left "not a JSON string"
