module Hahet.Logging where

import Prelude
import Data.Text (Text)
import Control.Monad.Logger
import Control.Monad.RWS (liftIO, MonadIO)
import System.Log.FastLogger
import Language.Haskell.TH
import Text.InterpolatedString.Perl6
import qualified System.Console.ANSI as A
-- import System.Log.FastLogger
-- import qualified System.Console.ANSI as A
-- import qualified System.IO as IO

data LogMsg = LogMsgDebug Text
            | LogMsgError Text
            | LogMsgFrom  Text LogMsg

status, action :: Q Exp
status = logOther "status"
action = logOther "action"

applyLogger :: (ToLogStr msg, MonadIO m)
            => Loc -> LogSource -> LogLevel -> msg -> Logger -> m ()
applyLogger loc logsource loglevel msg logger =
    liftIO . loggerPutStr logger $
        heading ++ ( {- LS currentModule : -} LS color : toLogStr msg : footing )
                ++ [ LS reset, LS "\n" ]
    where
        reset         = A.setSGRCode [A.Reset]
        color         = A.setSGRCode $ case loglevel of
            LevelError -> [A.SetColor A.Foreground A.Vivid A.Red     ]
            LevelWarn  -> [A.SetColor A.Foreground A.Vivid A.Yellow  ]
            LevelDebug -> [A.SetColor A.Foreground A.Dull  A.Cyan    ]
            LevelInfo  -> [A.Reset]
            LevelOther "status" -> [ A.SetColor A.Foreground A.Dull  A.Magenta  ]
            LevelOther "action" -> [ A.SetColor A.Foreground A.Vivid A.Green ]
            LevelOther{} -> [A.SetColor A.Foreground A.Dull A.Yellow ]
        heading = case loglevel of
            LevelDebug          -> LS "(" : LS color : LS " Debug " : LS reset : [ LS ") " ]
            LevelError          -> LS "(" : LS color : LS " Error " : LS reset : [ LS ") " ]
            LevelWarn           -> LS "(" : LS color : LS "Warning" : LS reset : [ LS ") " ]
            LevelOther "status" -> LS "(" : LS color : LS "Status " : LS reset : [ LS ") " ]
            LevelOther "action" -> LS "(" : LS color : LS "==> "    : LS reset : [ LS ") " ]
            _                   -> []
        footing = case loglevel of
            LevelDebug -> [ LS $ A.setSGRCode [A.SetColor A.Foreground A.Dull A.Cyan ]
                          , LS [qc|{logsource} {loc_filename loc} {loc_start loc} |]
                          , LS reset ]
            _          -> []
