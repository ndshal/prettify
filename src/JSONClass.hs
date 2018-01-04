{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module JSONClass
    (
      JValue(..)
      , JAry(..)
      , JObj(..)
      , isNull
    ) where

import Control.Arrow (second)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JArray (JAry JValue)
            | JObject (JObj JValue)
              deriving (Eq, Ord, Show)

newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show)

newtype JObj a = JObj {
      fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)

isNull v = v == JNull

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

instance JSON Int where
    toJValue   = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue   = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue   = JNumber
    fromJValue = doubleToJValue id

instance (JSON a) => JSON (JAry a) where
    toJValue = JArray . JAry . map toJValue . fromJAry

    fromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
    fromJValue _ = Left "not a JSON array"

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObj

    fromJValue (JObject (JObj a)) = whenRight JObj (mapEithers unwrap o)
        where unwrap (k, v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "not a JSON object"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _           = Left "not a JSON number"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right b)  = Right (f b)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _      = Right []
