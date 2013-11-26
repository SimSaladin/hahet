-- | Often used re-exports from external modules. Only relevent
-- classes/functions are exported. For details see the source.
module Hahet.Imports
    ( module Prelude
    , module Data.Typeable
    , module Data.Default
    , module Data.Text
    , module Data.Monoid
    , module Control.Lens
    , module Control.Monad
    , module Shelly
    , module Text.InterpolatedString.Perl6
    , convertFilePath
    ) where

import Prelude                       hiding (FilePath)
import Data.Typeable                 (Typeable(..))
import Data.Default                  (Default(..))
import Data.Text                     (Text)
import qualified Data.Text as T
import Data.Monoid
import Control.Lens
import Control.Monad
import Shelly                        hiding ((<.>))
import Text.InterpolatedString.Perl6

-- | Helper to convert from shelly FilePath to a String.
convertFilePath :: FilePath -> String
convertFilePath = T.unpack . toTextIgnore
