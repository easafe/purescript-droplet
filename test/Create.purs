module Test.Create where

import Droplet.Language
import Prelude
import Prim hiding (Constraint)
import Test.Types

import Data.BigInt (BigInt)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Driver as DD
import Droplet.Language.Internal.Translate as DLIQ
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.Model (connectionInfo)
import Test.Model as TM
import Test.RunTestFailure as TR
import Test.Spec (Spec)
import Test.Spec as TS
import Type.Proxy (Proxy(..))

tests ∷ Spec Unit
tests =
      TS.describe "create" do
            TS.describe "table" do
                  TS.it "plain types" do
                        let q = create # table (Table ∷ Table "test" (id ∷ Int, name ∷ String, set ∷ Boolean, n ∷ Number, bigId ∷ BigInt, date ∷ Date, dateTime ∷ DateTime))
                        TM.notParameterized """CREATE TABLE "test" ("bigId" BIGINT NOT NULL, "date" DATE NOT NULL, "dateTime" TIMESTAMPTZ NOT NULL, "id" INTEGER NOT NULL, "n" DOUBLE PRECISION NOT NULL, "name" TEXT NOT NULL, "set" BOOL NOT NULL)""" $ DLIQ.buildQuery q
                        void $ TM.resultOnly q
                  TS.it "maybe type" do
                        let q = create # table (Table ∷ Table "test" (id ∷ Maybe Int, name ∷ String))
                        TM.notParameterized """CREATE TABLE "test" ("id" INTEGER, "name" TEXT NOT NULL)""" $ DLIQ.buildQuery q
                        void $ TM.resultOnly q
                  TS.describe "constraints" do
                        TS.it "single" do
                              let q = create # table (Table ∷ Table "test" (id ∷ Maybe Int, name ∷ String, c ∷ Column Int Identity))
                              TM.notParameterized """CREATE TABLE "test" ("c" INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY, "id" INTEGER, "name" TEXT NOT NULL)""" $ DLIQ.buildQuery q
                              void $ TM.resultOnly q
                        TS.it "named" do
                              let q = create # table (Table ∷ Table "test" (id ∷ Maybe Int, name ∷ String, c ∷ Column Int (Constraint "pk" Identity)))
                              TM.notParameterized """CREATE TABLE "test" ("c" INTEGER NOT NULL CONSTRAINT "pk" GENERATED ALWAYS AS IDENTITY, "id" INTEGER, "name" TEXT NOT NULL)""" $ DLIQ.buildQuery q
                              void $ TM.resultOnly q
                        TS.it "many" do
                              let q = create # table (Table ∷ Table "test" (id ∷ Maybe Int, d ∷ Column String (PrimaryKey /\ Unique), name ∷ String, c ∷ Column Int Identity))
                              TM.notParameterized """CREATE TABLE "test" ("c" INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY, "d" TEXT NOT NULL PRIMARY KEY UNIQUE, "id" INTEGER, "name" TEXT NOT NULL)""" $ DLIQ.buildQuery q
                              void $ TM.resultOnly q
                        TS.it "foreign key" do
                              let q = create # table (Table ∷ Table "test" (id ∷ Maybe Int, fk ∷ Column Int (ForeignKey "id" UsersTable)))
                              TM.notParameterized """CREATE TABLE "test" ("fk" INTEGER NOT NULL REFERENCES "users" ("id"), "id" INTEGER)""" $ DLIQ.buildQuery q
                              void $ TM.resultOnly q
                        TS.describe "composite" do
                              TS.it "primary key" do
                                    let q = create # table (Table ∷ Table "test" (c ∷ Column Int (Constraint (Composite "pk") PrimaryKey), d ∷ Column Int (Constraint (Composite "pk") PrimaryKey)))
                                    TM.notParameterized """CREATE TABLE "test" ("c" INTEGER NOT NULL, "d" INTEGER NOT NULL, CONSTRAINT "pk" PRIMARY KEY("c", "d"))""" $ DLIQ.buildQuery q
                                    void $ TM.resultOnly q
                              TS.it "foreign key" do
                                    let q = create # table (Table ∷ Table "test" (id ∷ Maybe Int, fk1 ∷ Column Int (Constraint (Composite "fk") (ForeignKey "id" DoublePrimaryKeyTable)), fk2 ∷ Column Int (Constraint (Composite "fk") (ForeignKey "second_id" DoublePrimaryKeyTable))))
                                    TM.notParameterized """CREATE TABLE "test" ("fk1" INTEGER NOT NULL, "fk2" INTEGER NOT NULL, "id" INTEGER, CONSTRAINT "fk" FOREIGN KEY("fk1", "fk2") REFERENCES "double_primary_key" ("id", "second_id"))""" $ DLIQ.buildQuery q
            TS.describe "index" do
                  TS.describe "invariants" do
                        TS.it "column must be present in the table" do
                              TR.testFailure "index-test-1" "TypesDoNotUnify" "anniversary"
                  TS.it "single column" do
                        let q = create # index # on testTable name
                        TM.notParameterized """CREATE INDEX ON "test" ("name")""" $ DLIQ.buildQuery q
                        createTestTable
                        void $ TM.resultOnly q
                  TS.it "many columns" do
                        let q = create # index # on testTable (id /\ name)
                        TM.notParameterized """CREATE INDEX ON "test" ("id", "name")""" $ DLIQ.buildQuery q
                        createTestTable
                        void $ TM.resultOnly q
                  TS.it "named" do
                        let q = create # index (Proxy ∷ _ "test_name") # on testTable name
                        TM.notParameterized """CREATE INDEX "test_name" ON "test" ("name")""" $ DLIQ.buildQuery q
                        createTestTable
                        void $ TM.resultOnly q
                  TS.it "unique" do
                        let q = create # unique # index # on testTable name
                        TM.notParameterized """CREATE UNIQUE INDEX ON "test" ("name")""" $ DLIQ.buildQuery q
                        createTestTable
                        void $ TM.resultOnly q
                  TS.it "named unique" do
                        let q = create # unique # index (Proxy ∷ _ "test_name") # on testTable (id /\ name)
                        TM.notParameterized """CREATE UNIQUE INDEX "test_name" ON "test" ("id", "name")""" $ DLIQ.buildQuery q
                        createTestTable
                        void $ TM.resultOnly q

testTable ∷ Table "test" (id ∷ Int, name ∷ String)
testTable = Table

createTestTable ∷ Aff Unit
createTestTable = do
      pool ← liftEffect $ DD.newPool connectionInfo
      void <<< DD.withTransaction pool $ \c → DD.execute c $ create # table testTable
