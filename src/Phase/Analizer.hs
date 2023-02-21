module Phase.Analizer (
    analizer,
  ) where

import qualified Data.Primitive as P
import qualified Data.ImprovedPrimitive as IP
import Enviroment.Error (Error(..))

import Control.Monad.Except (MonadError)

analizerError :: String -> Error
analizerError = Error "AnalizingError"

analizer :: MonadError Error m => P.Primitive -> m IP.Primitive
analizer = undefined
