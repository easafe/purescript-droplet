module Test.SubQuery where

import Droplet.Language
import Prelude
import Test.Types

import Data.Maybe (Maybe(..))
import Droplet.Language.Internal.Query as Query
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests :: TestSuite
tests = do
      TU.suite "subquery" do
            TU.test "null" do
                  let q = select (select id # from users # wher (id .=. 1)) # from messages # orderBy id # limit 1
                  TM.parameterized "SELECT (SELECT id FROM users WHERE id = $1) FROM messages ORDER BY id LIMIT 1" $ Query.query q
                  TM.result q [{id: Just 1}]
            TU.test "nested null" do
                  let q = select (select created # from tags) # from messages # orderBy id # limit 1
                  TM.notParameterized "SELECT (SELECT created FROM tags) FROM messages ORDER BY id LIMIT 1" $ Query.query q
                  --avoid (Maybe (Maybe t))
                  TM.result q [{created: Nothing}]