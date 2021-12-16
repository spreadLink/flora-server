module FloraWeb.Server.Sessions where

import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid
import Web.FormUrlEncoded          (FromForm)
import Data.Text

type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { new    :: mode :- Get '[HTML] (Html ())
  , create :: mode :- ReqBody '[FormUrlEncoded] LoginForm :> Post '[HTML] (Html ())
  } deriving stock (Generic)

data LoginForm = LoginForm
  { username :: Text
  , password :: Text
  }
  deriving stock (Generic)
  deriving anyclass FromForm
