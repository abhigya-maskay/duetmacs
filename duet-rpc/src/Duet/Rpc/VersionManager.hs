module Duet.Rpc.VersionManager
  ( renderVersion
  ) where

import Data.Version (showVersion)
import Paths_duet_rpc (version)

renderVersion :: String
renderVersion = showVersion version
