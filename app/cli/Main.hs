module Main where

import Options.Applicative

import Database.PostgreSQL.Entity.DBT
import Flora.Environment
import Flora.Model.User
import Flora.PackageFixtures
import Flora.Publish
import Flora.UserFixtures

data Options = Options
  { cliCommand :: Command
  } deriving stock (Show, Eq)

data Command
  = Provision
  deriving stock (Show, Eq)

main :: IO ()
main = runOptions =<< execParser (parseOptions `withInfo` "CLI helper for flora-server")

parseOptions :: Parser Options
parseOptions =
  Options <$> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $
  command "provision-fixtures" (parseProvision `withInfo` "Load the fixtures into the database")

parseProvision :: Parser Command
parseProvision = pure Provision

runOptions :: Options -> IO ()
runOptions (Options Provision) = do
  env <- getFloraEnv
  withPool (pool env) $ do
    insertUser hackageUser
    insertUser adminUser

    publishPackage [] ghcPrimRelease ghcPrim hackageUser
    publishPackage [ghcBignumDepOnGhcPrim] ghcBignumRelease ghcBignum hackageUser
    publishPackage [baseDepOnGhcPrim, baseDepOnGhcBignum] baseRelease base hackageUser
    publishPackage [arrayDepOnBase] arrayRelease array hackageUser
    publishPackage [deepseqDepOnBase, deepseqDepOnArray, deepseqDepOnGhcPrim] deepseqRelease deepseq hackageUser
    publishPackage [stmDepOnBase, stmDepOnArray] stmRelease stm hackageUser
    publishPackage [containersDepOnBase, containersDepOnArray, containersDepOnDeepseq] containersRelease containers hackageUser
    publishPackage [integerGmpDepOnGhcPrim] integerGmpRelease integerGmp hackageUser
    publishPackage [bytestringDepOnBase, bytestringDepOnDeepseq, bytestringDepOnGhcBignum, bytestringDepOnGhcPrim, bytestringDepOnIntegerGmp] bytestringRelease bytestring hackageUser
    publishPackage [binaryDepOnArray, binaryDepOnBase, binaryDepOnBytestring, binaryDepOnContainers, binaryDepOnGhcPrim] binaryRelease binary hackageUser

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
