module Droplet.Language.Internal.Function (count, class ToCount, Aggregate(..)) where

import Data.BigInt (BigInt)
import Prim.Row (class Cons)
import Type.Proxy (Proxy)
import Droplet.Language.Internal.Definition(Star)

data Aggregate inp (fields :: Row Type) (out :: Type) =
      Count inp -- |
      -- Avg |
      -- Max |
      -- Min |
      -- Sum

class ToCount (f :: Type) (fields :: Row Type) | f -> fields

instance Cons name t e fields => ToCount (Proxy name) fields

instance ToCount Star fields

count :: forall f fields. ToCount f fields => f -> Aggregate f fields BigInt
count = Count