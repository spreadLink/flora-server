module Flora.Import.Package where

import Data.Maybe
import Data.Text
import Data.Text.Display
import Data.Time
import Distribution.Types.PackageDescription (PackageDescription (..))
import qualified Distribution.Types.PackageDescription as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.SourceRepo as Cabal
import Optics.Core

import Flora.Import.Types
import Flora.Model.Package
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.User

cabalToPackage :: PackageId -> UserId -> UTCTime -> PackageDescription -> Either ImportError Package
cabalToPackage packageId ownerId timestamp packageDesc = do
  name <- getPackageName (packageDesc ^. #package)
  let license = Cabal.license packageDesc
  sourceRepo <- getRepoURL $ packageDesc ^. #sourceRepos
  let homepage = Just (display $ packageDesc ^. #homepage)
  let documentation = ""
  let bugTracker = Just (display $ packageDesc ^. #bugReports)
  let metadata = PackageMetadata{..}
  let namespace = fromJust $ parseNamespace "haskell"
  let synopsis = display (packageDesc ^. #synopsis)
  let createdAt = timestamp
  let updatedAt = timestamp
  pure $ Package{..}

getPackageName :: Cabal.PackageIdentifier -> Either ImportError PackageName
getPackageName pkgId = do
  let pkgName = display (pkgId ^. #pkgName)
  case parsePackageName pkgName of
    Nothing   -> Left $ InvalidPackageName pkgName
    Just name -> Right name

getRepoURL :: [Cabal.SourceRepo] -> Either ImportError Text
getRepoURL []       = Left NoSourceRepoFound
getRepoURL (repo:_) = Right $ display $ fromMaybe "" $ repo ^. #repoLocation
