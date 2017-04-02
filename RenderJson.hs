module PrintJson where

import Data.List (intercalate)

data Json = Jstring String
  | Jnumber Double
  | Jbool Bool
  | Jnull
  | Jobject [(String, Json)]
  | Jarray [Json]
  deriving (Show, Eq, Ord)

renderJson :: Json -> String

renderJson (Jstring s)		= show s 
renderJson (Jnumber n)		= show (truncate n)
renderJson (Jbool True)		= "true" 
renderJson (Jbool False) 	= "false" 
renderJson (Jnull)			= "null"

renderJson (Jobject o) = "{\n  " ++ pairs o ++ "\n}"
  where pairs [] = ""
  	pairs ps = intercalate ",\n  " (map renderPair ps)
  	renderPair (k,v) = show k ++ ": " ++ renderJson v

renderJson (Jarray a) = "[" ++ values a++ "]"
  where values [] = ""
  	values vs = intercalate ", " (map renderJson vs)

putJson :: Json -> IO ()
putJson v = putStrLn (renderJson v)

putJson (Jobject [("name", (Jstring "John")), ("age", (Jnumber 22)), ("flagged", (Jbool True)), ("array", Jarray [(Jstring "abc"), (Jnumber 123)])])