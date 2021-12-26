module Test.Alter where

import Droplet.Language
import Prelude hiding (add)
import Prim hiding (Constraint)
import Test.Types

import Data.BigInt (BigInt)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Droplet.Driver as DD
import Droplet.Language.Internal.Translate as DLIQ
import Effect.Class (liftEffect)
import Test.Model (connectionInfo)
import Test.Model as TM
import Test.Spec (Spec)
import Test.Spec as TS
import Type.Proxy (Proxy(..))

tests ∷ Spec Unit
tests = TS.describe "alter" do
      TS.describe "table" do
            TS.describe "add" do
                  TS.it "no constraint" do
                        let test = Table ∷ Table "test" (id ∷ Int)
                        pool ← liftEffect $ DD.newPool connectionInfo
                        void $ DD.withTransaction pool $ \c → DD.execute c $ create # table test

                        let q = alter # table test # add name (Proxy :: _ String)
                        TM.notParameterized """ALTER TABLE "test" ADD "name" TEXT NOT NULL""" $ DLIQ.buildQuery q
                        void $ TM.resultOnly q
                  TS.describe "constraint" do
                        TS.it "simple" do
                              let test = Table ∷ Table "test" (id ∷ Int)
                              pool ← liftEffect $ DD.newPool connectionInfo
                              void $ DD.withTransaction pool $ \c → DD.execute c $ create # table test

                              let q = alter # table test # add name (Column :: Column String Unique)
                              TM.notParameterized """ALTER TABLE "test" ADD "name" TEXT NOT NULL UNIQUE""" $ DLIQ.buildQuery q
                              void $ TM.resultOnly q
                        TS.it "named" do
                              let test = Table ∷ Table "test" (id ∷ Int)
                              pool ← liftEffect $ DD.newPool connectionInfo
                              void $ DD.withTransaction pool $ \c → DD.execute c $ create # table test

                              let q = alter # table test # add name (Column :: Column String (Constraint "named" Unique))
                              TM.notParameterized """ALTER TABLE "test" ADD "name" TEXT NOT NULL CONSTRAINT "named" UNIQUE""" $ DLIQ.buildQuery q
                              void $ TM.resultOnly q