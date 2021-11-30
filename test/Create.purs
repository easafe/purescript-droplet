module Test.Create where

import Droplet.Language
import Prelude
import Test.Types

import Data.BigInt (BigInt)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU

tests ∷ TestSuite
tests =
      TU.suite "create" do
            TU.suite "table" do
                  TU.test "plain types" do
                        let q = create # table (Table :: Table "test" (id :: Int, name :: String, set :: Boolean, n :: Number, bigId :: BigInt, date :: Date, dateTime :: DateTime))
                        TM.notParameterized """CREATE TABLE "test" ("bigId" BIGINT NOT NULL, "date" DATE NOT NULL, "dateTime" TIMESTAMPTZ NOT NULL, "id" INTEGER NOT NULL, "n" DOUBLE PRECISION NOT NULL, "name" TEXT NOT NULL, "set" BOOL NOT NULL);""" $ DLIQ.buildQuery q
                        void $ TM.resultOnly q
