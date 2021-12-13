{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
module Flora.Model.Package.Component where

import Data.Aeson
import Data.Aeson.Orphans ()
import Data.ByteString
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display
import Data.Text.Encoding
import Data.UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), queryOne)
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName, field)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField (..), returnError)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (..))
import Database.PostgreSQL.Transact (DBT)
import GHC.Generics

import Database.PostgreSQL.Entity
import Flora.Model.Release

newtype ComponentId = ComponentId { getComponentId :: UUID }
  deriving stock (Generic)
  deriving (Eq, Ord, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID

data ComponentType
  = Library
  | Executable
  | Test
  | Benchmark
  deriving stock (Eq, Show, Generic, Bounded, Enum)

instance Display ComponentType where
  displayBuilder Library    = "library"
  displayBuilder Executable = "executable"
  displayBuilder Test       = "test"
  displayBuilder Benchmark  = "benchmark"

instance FromField ComponentType where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case parseComponentType bs of
          Just a -> pure a
          Nothing -> returnError ConversionFailed f $ T.unpack $  "Conversion error: Expected component to be one of " <> display @[ComponentType] [minBound .. maxBound] <> ", but instead got " <> decodeUtf8 bs

parseComponentType :: ByteString -> Maybe ComponentType
parseComponentType "library"    = Just Library
parseComponentType "executable" = Just Executable
parseComponentType "test"       = Just Test
parseComponentType "benchmark"  = Just Benchmark
parseComponentType _            = Nothing

instance ToField ComponentType where
  toField = Escape . encodeUtf8 . display

data PackageComponent = PackageComponent
  { componentId   :: ComponentId
  , releaseId     :: ReleaseId
  , name          :: Text -- "lib:flora", "test:flora-test", "exe:flora-server", "bench:flora-import"
  , componentType :: ComponentType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "requirements"] PackageComponent)

insertPackageComponent :: PackageComponent -> DBT IO ()
insertPackageComponent = insert @PackageComponent

getComponentById :: ComponentId -> DBT IO (Maybe PackageComponent)
getComponentById componentId = selectById @PackageComponent (Only componentId)

getComponent :: ReleaseId -> Text -> ComponentType -> DBT IO (Maybe PackageComponent)
getComponent releaseId name componentType =
  queryOne Select (_selectWhere @PackageComponent queryFields) (releaseId, name, componentType)
    where
      queryFields :: Vector Field
      queryFields = [ [field| release_id |], [field| name |],[field| component_type |] ]

