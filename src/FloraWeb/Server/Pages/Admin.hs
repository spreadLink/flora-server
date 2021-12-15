module FloraWeb.Server.Pages.Admin where

import Control.Monad.Reader
import Database.PostgreSQL.Entity.DBT (withPool)
import FloraWeb.Types
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid
import Servant.Server.Generic

import Flora.Environment
import Flora.Model.Admin.Report
import FloraWeb.Templates
import qualified FloraWeb.Templates.Admin as Templates
import FloraWeb.Templates.Types

type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { index :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

server :: ToServant Routes' (AsServerT FloraM)
server = genericServerT Routes'
  { index = indexHandler
  }

indexHandler :: FloraM (Html ())
indexHandler = do
  FloraEnv{pool} <- ask
  report <- liftIO $ withPool pool getReport
  render emptyAssigns (Templates.index report)
