module Duet.Rpc.VersionManager
  ( renderVersion,
  )
where

import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_duet_rpc (version)

renderVersion :: T.Text
renderVersion = T.pack (showVersion version)
