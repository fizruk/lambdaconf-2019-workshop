module ARCube.Utils.Miso where

import           Miso
import           Miso.String

prop_ :: MisoString -> MisoString -> Attribute action
prop_ = prop
