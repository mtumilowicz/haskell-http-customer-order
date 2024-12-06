module Gateway.CustomerApiSpec (spec) where

import qualified Assertions
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Domain.Customer.CustomerService
import Gateway.Customer.Handler
import Gateway.Customer.In.CreateCustomerApiInput as CreateCustomerApiInput
import Gateway.Customer.Out.CustomerApiOutput as CustomerApiOutput
import qualified Gateway.HttpTestSetup as HttpTestSetup
import Infrastructure.Customer.CustomerInMemoryRepository as CustomerInMemoryRepository
import Network.Wai.Handler.Warp (testWithApplication)
import Servant
import qualified Servant.Client as Client
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

newtype CreateCustomerApiInputArb = CreateCustomerApiInputArb CreateCustomerApiInput.CreateCustomerApiInput
  deriving (Show)

instance Arbitrary CreateCustomerApiInputArb where
  arbitrary = CreateCustomerApiInputArb <$> genericArbitrary

spec :: Spec
spec = describe "Gateway.Customer.Handler Integration Tests" $ around (testWithApplication app) $ do
  it "creates a new customer and retrieves it successfully" $ \port -> do
    -- given
    clientEnv <- HttpTestSetup.makeClientEnv port

    -- and
    CreateCustomerApiInputArb input <- generate arbitrary

    -- when
    createCustomerApiOutput <- Assertions.assertRightM $ Client.runClientM (createCustomerClient input) clientEnv

    -- then
    CustomerApiOutput.name createCustomerApiOutput `shouldBe` CreateCustomerApiInput.name input

    -- and
    let customerId = CustomerApiOutput.id createCustomerApiOutput
    getCustomerApiOutput <- Assertions.assertRightM $ Client.runClientM (getCustomerClient customerId) clientEnv
    CustomerApiOutput.name getCustomerApiOutput `shouldBe` CreateCustomerApiInput.name input

  it "returns 404 for non-existent customers" $ \port -> do
    -- given
    clientEnv <- HttpTestSetup.makeClientEnv port

    -- and
    nonExistingId <- liftIO $ UUID.toString <$> UUIDv4.nextRandom

    -- when
    getCustomerError <- Assertions.assertLeftM $ Client.runClientM (getCustomerClient nonExistingId) clientEnv

    -- then
    show getCustomerError `shouldContain` "404"
  where
    getCustomerClient :<|> createCustomerClient = Client.client customerApi
    setupInMemoryService = mkCustomerService <$> CustomerInMemoryRepository.new
    app = setupInMemoryService <&> (serve customerApi . handler)
