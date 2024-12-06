module Gateway.OrderApiSpec (spec) where

import qualified Assertions
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Domain.Order.OrderService
import qualified Gateway.HttpTestSetup as HttpTestSetup
import Gateway.Order.Handler
import Gateway.Order.In.CreateOrderApiInput as CreateOrderApiInput
import Gateway.Order.Out.OrderApiOutput as OrderApiOutput
import Infrastructure.Order.OrderInMemoryRepository as OrderInMemoryRepository
import Network.Wai.Handler.Warp (testWithApplication)
import Servant
import qualified Servant.Client as Client
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

newtype CreateOrderApiInputArb = CreateOrderApiInputArb CreateOrderApiInput.CreateOrderApiInput
  deriving (Show)

instance Arbitrary CreateOrderApiInputArb where
  arbitrary = CreateOrderApiInputArb <$> genericArbitrary

spec :: Spec
spec = describe "Gateway.Order.Handler Integration Tests" $ around (testWithApplication app) $ do
  it "creates a new order and retrieves it successfully" $ \port -> do
    -- given
    clientEnv <- HttpTestSetup.makeClientEnv port

    -- and
    CreateOrderApiInputArb input <- generate arbitrary

    -- when
    createOrderApiOutput <- Assertions.assertRightM $ Client.runClientM (createOrderClient input) clientEnv

    -- then
    OrderApiOutput.name createOrderApiOutput `shouldBe` CreateOrderApiInput.name input

    -- and
    let orderId = OrderApiOutput.id createOrderApiOutput
    getOrderApiOutput <- Assertions.assertRightM $ Client.runClientM (getOrderClient orderId) clientEnv
    OrderApiOutput.name getOrderApiOutput `shouldBe` CreateOrderApiInput.name input

  it "returns 404 for non-existent orders" $ \port -> do
    -- given
    clientEnv <- HttpTestSetup.makeClientEnv port

    -- and
    nonExistingId <- liftIO $ UUID.toString <$> UUIDv4.nextRandom

    -- when
    getOrderError <- Assertions.assertLeftM $ Client.runClientM (getOrderClient nonExistingId) clientEnv

    -- then
    show getOrderError `shouldContain` "404"
  where
    getOrderClient :<|> createOrderClient = Client.client orderApi
    setupInMemoryService = mkOrderService <$> OrderInMemoryRepository.new
    app = setupInMemoryService <&> (serve orderApi . handler)
