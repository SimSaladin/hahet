module Hahet.Logging where

import Control.Monad.Logger
import Language.Haskell.TH
-- import System.Log.FastLogger
-- import qualified System.Console.ANSI as A
-- import qualified System.IO as IO

debug, status, action :: Q Exp
debug = logDebug
status = logOther "status"
action = logOther "action"
