module Droplet (module Exported) where

import Droplet.Internal.Edsl.Language as Exported
import Droplet.Internal.Mapper.Query hiding (query) as Exported
import Droplet.Internal.Edsl.Definition as Exported
import Droplet.Internal.Edsl.Condition as Exported

{-
select fieldsList ✓ | * ✓ | sub select ✓ | function | scalars ✓ | column names

limit

from table name ✓ | sub select and table names

where field op field ✓ | field op parameter ✓ | and/or ✓ | sub select (maybe only exists?)

group by fields

order by fields

join right | left
-}