module Flora.Import.Package where

import Control.Monad.Except
import qualified Data.ByteString as B
import Data.Maybe
import Data.Text (Text)
import Data.Text.Display
import Data.Time
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Distribution.PackageDescription (Condition (..), PackageDescription,
                                        libBuildInfo, targetBuildDepends,
                                        unUnqualComponentName)
import qualified Distribution.PackageDescription as Cabal hiding (PackageName)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Types.CondTree
import Distribution.Pretty
import Distribution.Types.ConfVar
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.Library
import Distribution.Types.LibraryName
import Distribution.Types.Version
import Optics.Core

import Control.Monad.Reader
import qualified Data.Text as T
import Flora.Environment
import Flora.Import.Types
import Flora.Model.Package
import Flora.Model.Package.Component (PackageComponent(..), ComponentType, ComponentId(..))
import qualified Flora.Model.Package.Component as Component
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.Release
import Flora.Model.Requirement (Requirement (..), RequirementId(..),
                                RequirementMetadata (..), flag)
import Flora.Model.User
import Flora.Publish
import FloraWeb.Types

type DependencyTree = CondBranch ConfVar [Cabal.Dependency] Cabal.Library

-- | Import Steps
-- 1. Load the .cabal file at the given path
-- 2. Translate it to a GenericPackageDescription
-- 3. Extract a 'Package', see if it already exists
-- 4. Extract a 'Release'
-- 5. Extract multiple 'PackageComponent's
-- 6. Extract multiple 'Requirement's
-- 7. Insert everything
importCabal :: UserId    -- ^ The UserId of the stand-in user for Hackage, for instance.
            -> Namespace -- ^ The namespace to which the package will belong.
            -> FilePath  -- ^ Path to the .cabal file
            -> FloraM ()
importCabal userId namespace path = do
  FloraEnv{pool} <- ask
  genDesc <- liftIO $ loadFile path
  result <- runExceptT $ do
    name <- getPackageName genDesc
    package <- liftIO (withPool pool $ getPackageByNamespaceAndName namespace name)
       >>= \case
               Nothing -> cabalToPackage userId (genDesc ^. #packageDescription) namespace name
               Just package -> pure package
    release <- liftIO (withPool pool $ getReleaseByVersion (package ^. #packageId) (genDesc ^. #packageDescription ^. #package ^. #pkgVersion))
        >>= \case
                Nothing -> createRelease (package ^. #packageId)  (genDesc ^. #packageDescription ^. #package ^. #pkgVersion)
                Just release -> pure release
    components <- undefined
    pure (package, release, components)
  case result of
    Left _ -> error "whatever"
    Right (package, release, components) ->
      liftIO $ withPool pool $ publishPackage _ components release package undefined

loadFile :: FilePath -> IO GenericPackageDescription
loadFile path = fromJust . parseGenericPackageDescriptionMaybe <$> B.readFile path

isFlagBranch :: DependencyTree -> Bool
isFlagBranch (CondBranch (Var (PackageFlag _name)) _ _) = True
isFlagBranch _                                          = False

getOptionalDeps :: [DependencyTree] -> [([Cabal.Dependency], Condition ConfVar)]
getOptionalDeps tree = map (\x -> (getDependencies x, condBranchCondition x)) flagDeps
  where
    flagDeps = filter isFlagBranch tree
    getDependencies :: DependencyTree -> [Cabal.Dependency]
    getDependencies = targetBuildDepends . libBuildInfo . condTreeData . condBranchIfTrue

extractComponents :: _ -> [(PackageComponent, [Requirement])]
extractComponents = do
  case genDesc ^. #packageDescription ^. #library of
    Just lib -> do
      result <- extractFromLib (release ^. #releaseId) (package ^. #packageId) (package ^. #name) lib
      Just result
    Nothing -> Nothing

extractFromLib :: ReleaseId -> PackageId -> PackageName -> Library -> FloraM (PackageComponent, [Requirement])
extractFromLib releaseId packageId packageName library = do
  let dependencies = library ^. #libBuildInfo ^. #targetBuildDepends
  let libraryName = getLibName $ library ^. #libName
  let componentType = Component.Library
  component <- createComponent releaseId libraryName componentType
  requirements <- traverse (\dependency -> depToRequirement dependency (component ^. #componentId)) dependencies
  pure (component, requirements)
  where
    getLibName :: LibraryName -> Text
    getLibName LMainLibName        = "lib:" <> display packageName
    getLibName (LSubLibName lname) = T.pack $ "lib:" <> unUnqualComponentName lname

    depToRequirement :: Cabal.Dependency -> ComponentId -> ExceptT ImportError FloraM Requirement
    depToRequirement cabalDependency packageComponentId = do
      requirementId <- RequirementId <$> liftIO UUID.nextRandom
      let requirement = display $ prettyShow $ Cabal.depVerRange cabalDependency
      let metadata = RequirementMetadata{ flag = Nothing }
      pure Requirement{..}

createComponent :: ReleaseId -> Text -> ComponentType -> FloraM PackageComponent
createComponent releaseId name componentType = do
  componentId <- ComponentId <$> liftIO UUID.nextRandom
  pure PackageComponent{..}

createRelease :: PackageId -> Version -> ExceptT ImportError FloraM Release
createRelease packageId version = do
  releaseId <- ReleaseId <$> liftIO UUID.nextRandom
  timestamp <- liftIO getCurrentTime
  let archiveChecksum = mempty
  let createdAt = timestamp
  let updatedAt = timestamp
  pure Release{..}

cabalToPackage :: UserId
               -> PackageDescription
               -> Namespace
               -> PackageName
               -> ExceptT ImportError FloraM Package
cabalToPackage ownerId packageDesc namespace name = do
  timestamp <- liftIO getCurrentTime
  packageId <- PackageId <$> liftIO UUID.nextRandom
  sourceRepo <- getRepoURL $ packageDesc ^. #sourceRepos
  let license = Cabal.license packageDesc
  let homepage = Just (display $ packageDesc ^. #homepage)
  let documentation = ""
  let bugTracker = Just (display $ packageDesc ^. #bugReports)
  let metadata = PackageMetadata{..}
  let synopsis = display $ packageDesc ^. #synopsis
  let createdAt = timestamp
  let updatedAt = timestamp
  pure $ Package{..}

getPackageName :: GenericPackageDescription -> ExceptT ImportError FloraM PackageName
getPackageName genDesc = do
  let pkgName = display $ genDesc ^. #packageDescription ^. #package ^. #pkgName
  case parsePackageName pkgName of
    Nothing   -> throwError $ InvalidPackageName pkgName
    Just name -> pure name

getRepoURL :: [Cabal.SourceRepo] -> ExceptT ImportError FloraM Text
getRepoURL []       = throwError NoSourceRepoFound
getRepoURL (repo:_) = pure $ display $ fromMaybe mempty (repo ^. #repoLocation)
