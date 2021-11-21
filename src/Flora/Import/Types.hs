module Flora.Import.Types where

import Data.Text

data ImportError
  = InvalidPackageName Text
  | NoSourceRepoFound
  deriving stock (Eq, Show)

