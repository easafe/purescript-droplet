-- | SQL Keywords and tokens
module Droplet.Language.Internal.Token where

--magic strings
selectKeyword ∷ String
selectKeyword = "SELECT "

distinctKeyword ∷ String
distinctKeyword = "DISTINCT "

isNullKeyword ∷ String
isNullKeyword = " IS NULL"

isNotNullKeyword ∷ String
isNotNullKeyword = " IS NOT NULL"

unionKeyword ∷ String
unionKeyword = " UNION "

allKeyword ∷ String
allKeyword = "ALL "

fromKeyword ∷ String
fromKeyword = " FROM "

constraintKeyword ∷ String
constraintKeyword = "CONSTRAINT "

identityKeyword ∷ String
identityKeyword = " GENERATED ALWAYS AS IDENTITY"

uniqueKeyword ∷ String
uniqueKeyword = " UNIQUE"

whereKeyword ∷ String
whereKeyword = " WHERE "

referencesKeyword ∷ String
referencesKeyword = " REFERENCES "

andKeyword ∷ String
andKeyword = " AND "

orKeyword ∷ String
orKeyword = " OR "

asKeyword ∷ String
asKeyword = " AS "

innerKeyword ∷ String
innerKeyword = " INNER "

primaryKeyKeyword ∷ String
primaryKeyKeyword = " PRIMARY KEY"

foreignKeyKeyword ∷ String
foreignKeyKeyword = " FOREIGN KEY"

defaultKeyword ∷ String
defaultKeyword = "DEFAULT"

joinKeyword ∷ String
joinKeyword = "JOIN "

onKeyword ∷ String
onKeyword = " ON "

groupByKeyword ∷ String
groupByKeyword = " GROUP BY "

existsKeyword ∷ String
existsKeyword = "EXISTS "

inKeyword ∷ String
inKeyword = " IN "

notKeyword ∷ String
notKeyword = "NOT "

leftKeyword ∷ String
leftKeyword = " LEFT "

starSymbol ∷ String
starSymbol = "*"

comma ∷ String
comma = ", "

openBracket ∷ String
openBracket = "("

closeBracket ∷ String
closeBracket = ")"

equalsSymbol ∷ String
equalsSymbol = " = "

notEqualsSymbol ∷ String
notEqualsSymbol = " <> "

lesserThanSymbol ∷ String
lesserThanSymbol = " < "

greaterThanSymbol ∷ String
greaterThanSymbol = " > "

lesserEqualsThanSymbol ∷ String
lesserEqualsThanSymbol = " <= "

greaterEqualsThanSymbol ∷ String
greaterEqualsThanSymbol = " >= "

parameterSymbol ∷ String
parameterSymbol = "$"

insertKeyword ∷ String
insertKeyword = "INSERT INTO "

valuesKeyword ∷ String
valuesKeyword = " VALUES"

updateKeyword ∷ String
updateKeyword = "UPDATE "

setKeyword ∷ String
setKeyword = " SET "

deleteKeyword ∷ String
deleteKeyword = "DELETE"

atSymbol ∷ String
atSymbol = "@"

returningKeyword ∷ String
returningKeyword = " RETURNING "

descKeyword ∷ String
descKeyword = " DESC"

ascKeyword ∷ String
ascKeyword = " ASC"

orderKeyword ∷ String
orderKeyword = " ORDER "

byKeyword ∷ String
byKeyword = "BY "

countFunctionName ∷ String
countFunctionName = "count"

limitKeyword ∷ String
limitKeyword = " LIMIT "

offsetKeyword ∷ String
offsetKeyword = " OFFSET "

semicolon ∷ String
semicolon = ";"

dotSymbol ∷ String
dotSymbol = "."

string_aggFunctionName ∷ String
string_aggFunctionName = "string_agg"

array_aggFunctionName ∷ String
array_aggFunctionName = "array_agg"

simpleQuoteSymbol ∷ String
simpleQuoteSymbol = "'"

integerType ∷ String
integerType = "INTEGER"

bigIntegerType ∷ String
bigIntegerType = "BIGINT"

dateType ∷ String
dateType = "DATE"

tableKeyword ∷ String
tableKeyword = "TABLE "

notNullKeyword ∷ String
notNullKeyword = " NOT NULL"

dateTimeType ∷ String
dateTimeType = "TIMESTAMPTZ"

stringType ∷ String
stringType = "TEXT"

booleanType ∷ String
booleanType = "BOOL"

addKeyword ∷ String
addKeyword = "ADD "

alterKeyword ∷ String
alterKeyword = "ALTER "

dropKeyword ∷ String
dropKeyword = "DROP "

numberType ∷ String
numberType = "DOUBLE PRECISION"

createKeyword ∷ String
createKeyword = "CREATE "

newline ∷ String
newline = "\n"

space ∷ String
space = " "

quoteSymbol ∷ String
quoteSymbol = """""""
