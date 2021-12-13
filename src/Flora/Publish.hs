{-# LANGUAGE LambdaCase #-}
module Flora.Publish where

import Database.PostgreSQL.Transact
import Optics.Core

import Control.Monad
import Flora.Model.Package
import Flora.Model.Package.Component
import Flora.Model.Release (Release (..), insertRelease)
import Flora.Model.Requirement (Requirement, insertRequirement)
import Flora.Model.User (User)

{- TODO: Audit log of the published package
   TODO: Publish artifacts
-}
publishPackage :: [Requirement] -> [PackageComponent] -> Release -> Package -> User -> DBT IO ()
publishPackage requirements components release package _user =
  getPackageById (package ^. #packageId)
    >>= \case
          Nothing -> do
            insertPackage package
            insertRelease release
            forM_ components insertPackageComponent
            forM_ requirements insertRequirement
            refreshDependents
          Just _existingPackage -> do
            insertRelease release
            forM_ components insertPackageComponent
            forM_ requirements insertRequirement
            refreshDependents
