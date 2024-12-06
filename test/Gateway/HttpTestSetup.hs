module Gateway.HttpTestSetup
  ( makeClientEnv,
  )
where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Servant.Client as Client

makeClientEnv :: Int -> IO Client.ClientEnv
makeClientEnv port = do
  manager <- newManager defaultManagerSettings
  let baseUrl = Client.BaseUrl Client.Http "localhost" port ""
  pure $ Client.mkClientEnv manager baseUrl
