module Droplet.Internal.Definition where

data Field (name :: Symbol) = Field

data Parameter (name :: Symbol) = Parameter

data Table (name :: Symbol) (fields :: Row Type) = Table