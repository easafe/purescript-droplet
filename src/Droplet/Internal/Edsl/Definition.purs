module Droplet.Internal.Edsl.Definition where

data Field (name :: Symbol) = Field

data Alias (name :: Symbol) = Alias

data Star = Star

star :: Star
star = Star

data Parameter (name :: Symbol) = Parameter

data Table (name :: Symbol) (fields :: Row Type) = Table