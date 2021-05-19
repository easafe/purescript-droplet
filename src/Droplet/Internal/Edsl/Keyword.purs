module Droplet.Internal.Edsl.Keyword where


--magic strings
selectKeyword :: String
selectKeyword = "SELECT "

fromKeyword :: String
fromKeyword = " FROM "

whereKeyword :: String
whereKeyword = " WHERE "

andKeyword :: String
andKeyword = " AND "

orKeyword :: String
orKeyword = " OR "

asKeyword :: String
asKeyword = " AS "

starToken :: String
starToken = "*"

comma :: String
comma = ", "

openBracket :: String
openBracket = "("

closeBracket :: String
closeBracket = ")"

equalsSymbol :: String
equalsSymbol = " = "

notEqualsSymbol :: String
notEqualsSymbol = " <> "

parameterToken :: String
parameterToken = "$"

insertKeyword :: String
insertKeyword = "INSERT INTO "

valuesKeyword :: String
valuesKeyword = " VALUES "

updateKeyword :: String
updateKeyword = "UPDATE "

setKeyword :: String
setKeyword = " SET "

deleteKeyword :: String
deleteKeyword = "DELETE"

atToken :: String
atToken = "@"
