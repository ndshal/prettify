
-- constructor for JValue type.
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)

-- Helpers to extract values from JValue
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing
