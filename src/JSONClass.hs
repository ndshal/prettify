{-# LANGUAGE TypeSynonymInstances #-}

module JSONClass
    (
      JAry(..)
    ) where

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

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _           = Left "not a JSON number"

instance JSON Int where
    toJValue   = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue   = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue   = JNumber
    fromJValue = doubleToJValue id

newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show)

newtype JObj a = JObj {
      fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)
