module Domain.Customer.CustomerSpec (spec) where

import qualified Assertions
import Control.Monad.Except (runExceptT, throwError)
import qualified Data.UUID.V4 as UUIDv4
import Domain.Customer.CustomerRepository
import Domain.Customer.CustomerService
import Domain.Customer.Errors
import Domain.Customer.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "CustomerService" $ do
    it "finds an existing customer" $ do
      -- given
      uuid <- UUIDv4.nextRandom
      let cid = CustomerId uuid
      let customer = Customer cid (CustomerName "Alice")
      let repo =
            CustomerRepository
              { getCustomer = \u ->
                  if u == cid
                    then return customer
                    else throwError $ CustomerNotFound u,
                saveCustomer = \_ -> return customer
              }

      -- and
      let service = mkCustomerService repo

      -- when
      result <- Assertions.assertRightM $ runExceptT $ findCustomer service cid

      -- then
      result `shouldBe` customer

    it "returns error when customer not found" $ do
      -- given
      uuid <- UUIDv4.nextRandom
      let cid = CustomerId uuid
      let repo =
            CustomerRepository
              { getCustomer = \_ -> throwError $ CustomerNotFound cid,
                saveCustomer = undefined
              }

      -- and
      let service = mkCustomerService repo

      -- when
      result <- Assertions.assertLeftM $ runExceptT $ findCustomer service cid

      -- then
      result `shouldBe` CustomerNotFound cid

    it "creates a new customer" $ do
      -- given
      uuid <- UUIDv4.nextRandom
      let cid = CustomerId uuid
      let cname = CustomerName "Carol"
      let customer = Customer cid cname
      let command = CreateCustomerCommand cname
      let repo =
            CustomerRepository
              { getCustomer = undefined,
                saveCustomer = \_ -> return customer
              }

      -- and
      let service = mkCustomerService repo

      -- when
      result <- Assertions.assertRightM $ runExceptT $ createCustomer service command

      -- then
      result `shouldBe` customer

    it "returns error when creating a customer that already exists" $ do
      -- given
      uuid <- UUIDv4.nextRandom
      let cid = CustomerId uuid
      let cname = CustomerName "Carol"
      let command = CreateCustomerCommand cname
      let repo =
            CustomerRepository
              { getCustomer = undefined,
                saveCustomer = \_ -> throwError $ CustomerAlreadyExists cid
              }

      -- and
      let service = mkCustomerService repo

      -- when
      result <- Assertions.assertLeftM $ runExceptT $ createCustomer service command

      -- then
      result `shouldBe` CustomerAlreadyExists cid
