module Hahet.Logging where

import Control.Monad.Logger
import Language.Haskell.TH
-- import System.Log.FastLogger
-- import qualified System.Console.ANSI as A
-- import qualified System.IO as IO

status, action :: Q Exp
status = logOther "status"
action = logOther "action"

-- * Logging

--     let logHandle = IO.stdout -- XXX: duplicate to a file?

--     logger <- mkLogger True logHandle

-- ** Logging in configurations
