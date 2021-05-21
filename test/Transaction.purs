module Test.Transaction where

import Droplet.Internal.Language.Condition
import Droplet.Internal.Language.Syntax
import Prelude
import Test.Types

import Control.Monad.Error.Class as EA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Internal.Driver.Query as DIMD
import Droplet.Internal.Driver.Pool as DIMP
import Effect.Class (liftEffect)
import Effect.Exception as EE
import Test.Model (connectionInfo)
import Test.Model as TM
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "transactions" do
            TU.test "commited" do
                  pool <- liftEffect $ DIMP.newPool connectionInfo
                  void $ DIMD.withTransaction pool $ \connection -> do
                        TM.truncateTables connection

                        let ins = insert # into users (name /\ surname /\ birthday) # values ("josh" /\ "j." /\ TM.makeDate 1990 1 1)
                        errors <- DIMD.execute connection ins
                        TUA.equal Nothing errors

                        let sel = select id # from users
                        result <- DIMD.single connection sel
                        TUA.equal (Right (Just { id : 1})) result

                        let upd = update users # set ((name /\ "Mary") /\ (surname /\ "Sue")) # wher (id .=. 1)
                        errors <- DIMD.execute connection upd
                        TUA.equal Nothing errors

                  let q = select (name /\ surname) # from users
                  TM.result' q [{name: "Mary", surname: "Sue"}]
            TU.test "rolled back" do
                  pool <- liftEffect $ DIMP.newPool connectionInfo
                  flip EA.catchError (const (pure unit)) <<< void <<< DIMD.withTransaction pool $ \connection -> do
                        TM.truncateTables connection

                        let ins = insert # into users (name /\ surname /\ birthday) # values ("josh" /\ "j." /\ TM.makeDate 1990 1 1)
                        errors <- DIMD.execute connection ins
                        TUA.equal Nothing errors

                        void <<< EA.throwError $ EE.error "pretend it happened for some reason"

                        let upd = update users # set ((name /\ "Mary") /\ (surname /\ "Sue")) # wher (id .=. 1)
                        errors <- DIMD.execute connection upd
                        TUA.equal Nothing errors

                  let q = select (name /\ surname) # from users
                  TM.result' q []


