module Test.Create where

import Droplet.Language
import Prelude
import Test.Types

import Data.BigInt (BigInt)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Language.Internal.Translate as DLIQ
import Test.Model as TM
import Test.Spec (Spec)
import Test.Spec as TS

tests ∷ Spec Unit
tests =
      TS.describeOnly "create" do
            TS.describe "table" do
                  TS.it "plain types" do
                        let q = create # table (Table :: Table "test" (id :: Int, name :: String, set :: Boolean, n :: Number, bigId :: BigInt, date :: Date, dateTime :: DateTime))
                        TM.notParameterized """CREATE TABLE "test" ("bigId" BIGINT NOT NULL, "date" DATE NOT NULL, "dateTime" TIMESTAMPTZ NOT NULL, "id" INTEGER NOT NULL, "n" DOUBLE PRECISION NOT NULL, "name" TEXT NOT NULL, "set" BOOL NOT NULL);""" $ DLIQ.buildQuery q
                        void $ TM.resultOnly q
                  TS.it "maybe type" do
                        let q = create # table (Table :: Table "test" (id :: Maybe Int, name :: String))
                        TM.notParameterized """CREATE TABLE "test" ("id" INTEGER, "name" TEXT NOT NULL);""" $ DLIQ.buildQuery q
                        void $ TM.resultOnly q
                  TS.describe "constraints" do
                        TS.it "single" do
                              let q = create # table (Table :: Table "test" (id :: Maybe Int, name :: String, c :: Column Int Identity ))
                              TM.notParameterized """CREATE TABLE "test" ("c" INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY, "id" INTEGER, "name" TEXT NOT NULL);""" $ DLIQ.buildQuery q
                              void $ TM.resultOnly q
                        TS.it "many" do
                              let q = create # table (Table :: Table "test" (id :: Maybe Int, d :: Column String (PrimaryKey /\ Unique), name :: String, c :: Column Int Identity ))
                              TM.notParameterized """CREATE TABLE "test" ("c" INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY, "d" TEXT NOT NULL PRIMARY KEY UNIQUE, "id" INTEGER, "name" TEXT NOT NULL);""" $ DLIQ.buildQuery q
                              void $ TM.resultOnly q
                        -- TS.it "foreign key" do
                        --       let q = create # table (Table :: Table "test" (id :: Maybe Int, pk :: Column Int (ForeignKey "id" UsersTable)))
                        --       TM.notParameterized """CREATE TABLE "test" ("id" INTEGER, "name" TEXT NOT NULL);""" $ DLIQ.buildQuery q
                        --       void $ TM.resultOnly q