module Flora.Import.Package where

import qualified Data.ByteString as B
import Data.Char
import Data.List (sortOn)
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple (VersionIntervals, intersectVersionIntervals, fromVersionIntervals, toVersionIntervals)
import Distribution.Types.CondTree
import Distribution.Types.Dependency (Dependency(..))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Flora.Model.Package.Orphans ()
import Distribution.PackageDescription.Parsec
import Distribution.Types.UnqualComponentName
import Data.Bifunctor

type DependencyTree = CondTree ConfVar [Dependency] IsBuildable
data IsBuildable = Buildable
                 | NotBuildable
                   deriving (Eq, Show)

loadFile :: FilePath -> IO GenericPackageDescription
loadFile path = fromJust . parseGenericPackageDescriptionMaybe <$> B.readFile path

libraryDeps :: GenericPackageDescription -> [(String, DependencyTree)]
libraryDeps desc = maybeToList $ (\lib -> ("library", lib)) <$> libDeps
  where
    libDeps = depTree libBuildInfo <$> condLibrary desc

subLibraryDeps :: GenericPackageDescription -> [(String, DependencyTree)]
subLibraryDeps desc = maybeToList undefined
  where
    results = depTree libBuildInfo <$> subLibs
    subLibs = fmap (first unUnqualComponentName) (condSubLibraries desc)

depTree :: (a -> BuildInfo) -> CondTree ConfVar [Dependency] a -> DependencyTree
depTree getBuildInfo = mapTreeData isBuildable . mapTreeConstrs simplifyDeps
  where
    simplifyDeps = sortDeps . combineDepsBy intersectVersionIntervals
    isBuildable ctData = if buildable $ getBuildInfo ctData
                           then Buildable
                           else NotBuildable


sortDeps :: [Dependency] -> [Dependency]
sortDeps = sortOn $ \(Dependency pkgname _ _) -> map toLower (prettyShow pkgname)


combineDepsBy :: (VersionIntervals -> VersionIntervals -> VersionIntervals)
              -> [Dependency] -> [Dependency]
combineDepsBy f =
    map (\(pkgname, ver) -> Dependency pkgname (fromVersionIntervals ver) Set.empty) -- XXX: ok?
  . Map.toList
  . Map.fromListWith f
  . map (\(Dependency pkgname ver _) -> (pkgname, toVersionIntervals ver))

-- cabalToPackage :: PackageId
--                -> UserId
--                -> UTCTime
--                -> PackageDescription
--                -> Either ImportError Package
-- cabalToPackage packageId ownerId timestamp packageDesc = do
--   name <- getPackageName (packageDesc ^. #package)
--   let license = Cabal.license packageDesc
--   sourceRepo <- getRepoURL $ packageDesc ^. #sourceRepos
--   let homepage = Just (prettyShow $ packageDesc ^. #homepage)
--   let documentation = ""
--   let bugTracker = Just (prettyShow $ packageDesc ^. #bugReports)
--   let metadata = PackageMetadata{..}
--   let namespace = fromJust $ parseNamespace "haskell"
--   let synopsis = prettyShow (packageDesc ^. #synopsis)
--   let createdAt = timestamp
--   let updatedAt = timestamp
--   pure $ Package{..}

-- getPackageName :: Cabal.PackageIdentifier -> Either ImportError PackageName
-- getPackageName pkgId = do
--   let pkgName = prettyShow (pkgId ^. #pkgName)
--   case parsePackageName pkgName of
--     Nothing   -> Left $ InvalidPackageName pkgName
--     Just name -> Right name

-- getRepoURL :: [Cabal.SourceRepo] -> Either ImportError Text
-- getRepoURL []       = Left NoSourceRepoFound
-- getRepoURL (repo:_) = Right $ prettyShow $ fromMaybe "" $ repo ^. #repoLocation
