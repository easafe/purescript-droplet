-- | Plain (untyped) representation of SQL statements
-- |
-- | Statements are type checked by `Droplet.Language.Internal.Syntax`, lowered to this representation by `Droplet.Language.Internal.Translate` and then printed by `render`
-- |
-- | Do not import this module directly, it will break your code and make it not type safe. Use the sanitized `Droplet.Language` instead
module Droplet.Language.Internal.Ast
      ( Statement
      , Clause(..)
      , Source(..)
      , JoinType(..)
      , Expression(..)
      , SortDirection(..)
      , print
      ) where

import Prelude

import Data.Array ((:))
import Data.Array as DA
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.String as DST
import Data.Tuple (Tuple(..))
import Droplet.Language.Internal.Condition (BinaryOperator(..))
import Droplet.Language.Internal.Token (addKeyword, allKeyword, alterKeyword, createKeyword, dropKeyword, tableKeyword, andKeyword, asKeyword, ascKeyword, byKeyword, closeBracket, comma, defaultKeyword, deleteKeyword, descKeyword, distinctKeyword, dotSymbol, equalsSymbol, existsKeyword, fromKeyword, greaterEqualsThanSymbol, greaterThanSymbol, groupByKeyword, inKeyword, innerKeyword, insertKeyword, isNotNullKeyword, isNullKeyword, joinKeyword, leftKeyword, lesserEqualsThanSymbol, lesserThanSymbol, limitKeyword, notEqualsSymbol, notKeyword, offsetKeyword, onKeyword, openBracket, orKeyword, orderKeyword, parameterSymbol, quoteSymbol, returningKeyword, selectKeyword, setKeyword, space, starSymbol, unionKeyword, updateKeyword, valuesKeyword, whereKeyword)
import Foreign (Foreign)

-- | Clauses in the order they were written
type Statement = Array Clause

data Clause
      = Select (Array Expression)
      | From Source
      | Where Expression
      | GroupBy (Array Expression)
      | OrderBy (Array Expression)
      | Limit Int
      | Offset Int
      -- | (statement UNION [ALL] statement); `true` for UNION ALL
      | Union Boolean Statement Statement
      | Update String
      | Set (Array (Tuple String Expression))
      | Delete
      -- | INSERT INTO table (columns)
      | InsertInto String (Array Expression)
      -- | INSERT INTO table DEFAULT VALUES
      | InsertDefaultValues String
      -- | VALUES (row), (row), ...
      | Values (Array (Array Expression))
      | Returning (Array Expression)
      -- | CREATE TABLE table (definitions)
      | CreateTable String (Array String)
      -- | ALTER TABLE table ADD definition
      | AlterTableAdd String String
      | DropTable String

data Source
      = Table String
      | AliasedTable String String
      -- | (statement) AS alias
      | SubQuery Statement String
      | Join JoinType Source Source Expression

data JoinType = InnerJoin | LeftJoin

data SortDirection = Ascending | Descending

data Expression
      = Column String
      -- | alias.column
      | QualifiedColumn String String
      | Star
      -- | Server side parameter ($n)
      | Parameter Foreign
      -- | Printed as is
      | Literal String
      | DefaultValue
      | Function String (Array Expression)
      -- | expression ORDER BY expressions, as used in aggregate function arguments
      | OrderedBy Expression (Array Expression)
      | Sorted Expression SortDirection
      | Distinct (Array Expression)
      -- | expression AS alias
      | As Expression String
      -- | expression alias; used to name qualified columns in projections
      | Labeled Expression String
      -- | Comparisons and logical operators. `Nothing` simply juxtaposes both sides
      | Operation (Maybe BinaryOperator) Expression Expression
      | Not Expression
      | IsNull Expression
      | IsNotNull Expression
      | In Expression (Array Expression)
      | Exists Statement
      -- | (statement)
      | SubQueryExpression Statement

-- | Rendered SQL: text with parameter placeholders, which are numbered by `print`
data Chunk = Text String | Placeholder Foreign

type Rendered = Array Chunk

text ∷ String → Rendered
text s = [ Text s ]

-- | Comma separated list
list ∷ ∀ a. (a → Rendered) → Array a → Rendered
list f = DF.intercalate (text comma) <<< map f

-- | (rendered)
bracketed ∷ Rendered → Rendered
bracketed r = text openBracket <> r <> text closeBracket

-- | Render a statement without numbering its parameters
render ∷ Statement → Rendered
render statement = DA.concatMap renderClause statement

renderClause ∷ Clause → Rendered
renderClause = case _ of
      Select columns → text selectKeyword <> list renderExpression columns
      From source → text fromKeyword <> renderSource source
      Where condition → text whereKeyword <> renderExpression condition
      GroupBy columns → text groupByKeyword <> list renderExpression columns
      OrderBy columns → text (orderKeyword <> byKeyword) <> list renderExpression columns
      Limit n → text (limitKeyword <> show n)
      Offset n → text (offsetKeyword <> show n)
      Union isAll statement other → bracketed (render statement <> text (unionKeyword <> (if isAll then allKeyword else "")) <> render other)
      Update table → text (updateKeyword <> quote table)
      Set pairs → text setKeyword <> list renderPair pairs
      Delete → text deleteKeyword
      InsertInto table columns → text (insertKeyword <> quote table <> space) <> bracketed (list renderExpression columns)
      InsertDefaultValues table → text (insertKeyword <> quote table <> space <> defaultKeyword <> valuesKeyword)
      Values rows → text (valuesKeyword <> space) <> list (bracketed <<< list renderExpression) rows
      Returning columns → text returningKeyword <> list renderExpression columns
      CreateTable table definitions → text (createKeyword <> tableKeyword <> quote table <> space) <> bracketed (list text definitions)
      AlterTableAdd table definition → text (alterKeyword <> tableKeyword <> quote table <> space <> addKeyword <> definition)
      DropTable table → text (dropKeyword <> tableKeyword <> quote table)
      where
      renderPair (Tuple name value) = text (quote name <> equalsSymbol) <> renderExpression value

renderSource ∷ Source → Rendered
renderSource = case _ of
      Table name → text (quote name)
      AliasedTable name alias → text (quote name <> asKeyword <> quote alias)
      SubQuery statement alias → bracketed (render statement) <> text (asKeyword <> quote alias)
      Join joinType left right condition → renderSource left <> text (printJoinType joinType) <> renderSource right <> text onKeyword <> renderExpression condition

renderExpression ∷ Expression → Rendered
renderExpression = case _ of
      Column name → text (quote name)
      QualifiedColumn alias name → text (quote alias <> dotSymbol <> quote name)
      Star → text starSymbol
      Parameter value → [ Placeholder value ]
      Literal value → text value
      DefaultValue → text defaultKeyword
      Function name arguments → text name <> bracketed (list renderExpression arguments)
      OrderedBy expression columns → renderExpression expression <> text (orderKeyword <> byKeyword) <> list renderExpression columns
      Sorted expression direction → renderExpression expression <> text case direction of
            Ascending → ascKeyword
            Descending → descKeyword
      Distinct columns → text distinctKeyword <> list renderExpression columns
      As expression alias → renderExpression expression <> text (asKeyword <> quote alias)
      Labeled expression alias → renderExpression expression <> text (space <> quote alias)
      Operation operator left right →
            let
                  operation = renderExpression left <> text (printOperator operator) <> renderExpression right
            in
                  if operator == Just And || operator == Just Or then bracketed operation else operation
      Not expression → text notKeyword <> renderExpression expression
      IsNull expression → renderExpression expression <> text isNullKeyword
      IsNotNull expression → renderExpression expression <> text isNotNullKeyword
      In expression values → renderExpression expression <> text inKeyword <> bracketed (list renderExpression values)
      Exists statement → text existsKeyword <> bracketed (render statement)
      SubQueryExpression statement → bracketed (render statement)

-- | Print a statement, numbering its parameters ($1, $2, ...) in the order they appear
print ∷ Statement → { query ∷ String, parameters ∷ Array Foreign }
print statement = { query: DST.joinWith "" (DA.reverse result.sql), parameters: result.parameters }
      where
      result = DF.foldl number { sql: [], parameters: [] } (render statement)
      number acc = case _ of
            Text s → acc { sql = s : acc.sql }
            Placeholder value →
                  let
                        parameters = DA.snoc acc.parameters value
                  in
                        acc { sql = (parameterSymbol <> show (DA.length parameters)) : acc.sql, parameters = parameters }

printJoinType ∷ JoinType → String
printJoinType = case _ of
      InnerJoin → innerKeyword <> joinKeyword
      LeftJoin → leftKeyword <> joinKeyword

printOperator ∷ Maybe BinaryOperator → String
printOperator = case _ of
      Nothing → ""
      Just op → case op of
            Equals → equalsSymbol
            NotEquals → notEqualsSymbol
            LesserThan → lesserThanSymbol
            GreaterThan → greaterThanSymbol
            LesserEqualsThan → lesserEqualsThanSymbol
            GreaterEqualsThan → greaterEqualsThanSymbol
            And → andKeyword
            Or → orKeyword

quote ∷ String → String
quote name = quoteSymbol <> name <> quoteSymbol
