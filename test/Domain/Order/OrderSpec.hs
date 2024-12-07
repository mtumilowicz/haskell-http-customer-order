module Domain.Order.OrderSpec (spec) where

import qualified Assertions
import Control.Monad.Except (runExceptT, throwError)
import qualified Data.UUID.V4 as UUIDv4
import Domain.Order.Errors
import Domain.Order.OrderRepository
import Domain.Order.OrderService
import Domain.Order.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "OrderService" $ do
    it "finds an existing order" $ do
      -- given
      uuid <- UUIDv4.nextRandom
      let oid = OrderId uuid
      let order = Order oid (OrderName "Macbook")
      let repo =
            OrderRepository
              { getOrder = \u ->
                  if u == oid
                    then return order
                    else throwError $ OrderNotFound u,
                saveOrder = \_ -> return order
              }

      -- and
      let service = mkOrderService repo

      -- when
      result <- Assertions.assertRightM $ runExceptT $ findOrder service oid

      -- then
      result `shouldBe` order

    it "returns error when order not found" $ do
      -- given
      uuid <- UUIDv4.nextRandom
      let oid = OrderId uuid
      let repo =
            OrderRepository
              { getOrder = \_ -> throwError $ OrderNotFound oid,
                saveOrder = undefined
              }

      -- and
      let service = mkOrderService repo

      -- when
      result <- Assertions.assertLeftM $ runExceptT $ findOrder service oid

      -- then
      result `shouldBe` OrderNotFound oid

    it "creates a new order" $ do
      -- given
      uuid <- UUIDv4.nextRandom
      let oid = OrderId uuid
      let oname = OrderName "Macbook"
      let order = Order oid oname
      let command = CreateOrderCommand oname
      let repo =
            OrderRepository
              { getOrder = undefined,
                saveOrder = \_ -> return order
              }

      -- and
      let service = mkOrderService repo

      -- when
      result <- Assertions.assertRightM $ runExceptT $ createOrder service command

      -- then
      result `shouldBe` order

    it "returns error when creating a order that already exists" $ do
      -- given
      uuid <- UUIDv4.nextRandom
      let oid = OrderId uuid
      let oname = OrderName "Macbook"
      let command = CreateOrderCommand oname
      let repo =
            OrderRepository
              { getOrder = undefined,
                saveOrder = \_ -> throwError $ OrderAlreadyExists oid
              }

      -- and
      let service = mkOrderService repo

      -- when
      result <- Assertions.assertLeftM $ runExceptT $ createOrder service command

      -- then
      result `shouldBe` OrderAlreadyExists oid
