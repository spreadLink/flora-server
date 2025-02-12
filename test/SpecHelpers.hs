module SpecHelpers where

import Control.Monad
import Data.Pool
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (Connection, close)
import Database.PostgreSQL.Simple.Migration
import Flora.Model.User
import Flora.PackageFixtures
import Flora.Publish
import Flora.UserFixtures

migrate :: Connection -> IO ()
migrate conn = do
  void $ runMigrations conn defaultOptions [MigrationInitialization, MigrationDirectory "./migrations"]
  pool <- createPool (pure conn) close 1 10 1
  withPool pool $ do
    insertUser user1
    insertUser user2

    publishPackage [] ghcPrimRelease ghcPrim user2
    publishPackage [ghcBignumDepOnGhcPrim] ghcBignumRelease ghcBignum user1
    publishPackage [baseDepOnGhcPrim, baseDepOnGhcBignum] baseRelease base user1
    publishPackage [arrayDepOnBase] arrayRelease array user1
    publishPackage [deepseqDepOnBase, deepseqDepOnArray, deepseqDepOnGhcPrim] deepseqRelease deepseq user1
    publishPackage [stmDepOnBase, stmDepOnArray] stmRelease stm user1
    publishPackage [containersDepOnBase, containersDepOnArray, containersDepOnDeepseq] containersRelease containers user1
    publishPackage [integerGmpDepOnGhcPrim] integerGmpRelease integerGmp user1
    publishPackage [bytestringDepOnBase, bytestringDepOnDeepseq, bytestringDepOnGhcBignum, bytestringDepOnGhcPrim, bytestringDepOnIntegerGmp] bytestringRelease bytestring user2
    publishPackage [binaryDepOnArray, binaryDepOnBase, binaryDepOnBytestring, binaryDepOnContainers, binaryDepOnGhcPrim] binaryRelease binary user2
