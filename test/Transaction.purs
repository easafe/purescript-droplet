module Test.Transaction where

import Droplet.Language
import Prelude
import Test.Types

import Control.Monad.Error.Class as EA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Driver as DD
import Effect.Class (liftEffect)
import Effect.Exception as EE
import Test.Model (connectionInfo)
import Test.Model as TM

import Test.Spec (Spec)
import Test.Spec as TS
import Test.Spec.Assertions as TSA

tests ∷ Spec Unit
tests = do
      TS.describe "transactions" do
            TS.it "commited" do
                  pool ← liftEffect $ DD.newPool connectionInfo
                  void $ DD.withTransaction pool $ \connection → do
                        TM.truncateTables connection

                        let ins = insert # into users (name /\ surname /\ birthday) # values ("josh" /\ "j." /\ TM.makeDate 1990 1 1)
                        errors ← DD.execute connection ins
                        TSA.shouldEqual Nothing errors

                        let sel = select id # from users
                        result ← DD.single connection sel
                        TSA.shouldEqual (Right (Just { id: 1 })) result

                        let upd = update users # set ((name .=. "Mary") /\ (surname .=. "Sue")) # wher (id .=. 1)
                        errors ← DD.execute connection upd
                        TSA.shouldEqual Nothing errors

                  let q = select (name /\ surname) # from users
                  TM.result' q [ { name: "Mary", surname: "Sue" } ]
            TS.it "rolled back" do
                  pool ← liftEffect $ DD.newPool connectionInfo
                  flip EA.catchError (const (pure unit)) <<< void <<< DD.withTransaction pool $ \connection → do
                        TM.truncateTables connection

                        let ins = insert # into users (name /\ surname /\ birthday) # values ("josh" /\ "j." /\ TM.makeDate 1990 1 1)
                        errors ← DD.execute connection ins
                        TSA.shouldEqual Nothing errors

                        void <<< EA.throwError $ EE.error "pretend it happened for some reason"

                        let upd = update users # set ((name .=. "Mary") /\ (surname .=. "Sue")) # wher (id .=. 1)
                        errors2 ← DD.execute connection upd
                        TSA.shouldEqual Nothing errors2

                  let q = select (name /\ surname) # from users
                  TM.result' q []
