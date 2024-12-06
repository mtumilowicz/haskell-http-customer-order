module Domain.Customer.CustomerSpec (spec) where

import qualified Assertions
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Data.UUID.V4 as UUIDv4
import Domain.Customer.CustomerRepository
import Domain.Customer.CustomerService
import Domain.Customer.Errors
import Domain.Customer.Types
import Test.Hspec

data MockCustomerRepository = MockCustomerRepository
  { mockGetCustomer :: CustomerId -> ExceptT CustomerNotFound IO Customer,
    mockSaveCustomer :: Customer -> ExceptT CustomerAlreadyExists IO Customer
  }

instance CustomerRepository MockCustomerRepository where
  getCustomer = mockGetCustomer
  saveCustomer = mockSaveCustomer

spec :: Spec
spec = do
  describe "CustomerService" $ do
    it "finds an existing customer" $ do
      -- given
      uuid <- UUIDv4.nextRandom
      let cid = CustomerId uuid
      let customer = Customer cid (CustomerName "Alice")
      let repo =
            MockCustomerRepository
              { mockGetCustomer = \u ->
                  if u == cid
                    then return customer
                    else throwError $ CustomerNotFound u,
                mockSaveCustomer = \_ -> return customer
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
            MockCustomerRepository
              { mockGetCustomer = \_ -> throwError $ CustomerNotFound cid,
                mockSaveCustomer = undefined
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
            MockCustomerRepository
              { mockGetCustomer = undefined,
                mockSaveCustomer = \_ -> return customer
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
            MockCustomerRepository
              { mockGetCustomer = undefined,
                mockSaveCustomer = \_ -> throwError $ CustomerAlreadyExists cid
              }

      -- and
      let service = mkCustomerService repo

      -- when
      result <- Assertions.assertLeftM $ runExceptT $ createCustomer service command

      -- then
      result `shouldBe` CustomerAlreadyExists cid
