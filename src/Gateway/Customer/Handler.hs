{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Gateway.Customer.Handler
  ( handler,
    CustomerApiOutput,
    CreateCustomerApiInput,
    CustomerAPI,
  )
where

import Control.Lens
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Generics.Labels ()
import Domain.Customer.CustomerService
import Domain.Customer.Errors
import Domain.Customer.Types
import Gateway.Customer.In.CreateCustomerApiInput as CreateCustomerApiInput
import Gateway.Customer.Out.CustomerApiOutput as CustomerApiOutput
import Servant

type CustomerAPI =
  "customers" :> Capture "customerId" String :> Get '[JSON] CustomerApiOutput
    :<|> "customers" :> ReqBody '[JSON] CreateCustomerApiInput :> Post '[JSON] CustomerApiOutput

handler :: CustomerService IO -> Server CustomerAPI
handler service = getCustomerHandler :<|> createCustomerHandler
  where
    getCustomerHandler :: String -> Handler CustomerApiOutput
    getCustomerHandler cidStr =
      case parseCustomerId cidStr of
        Left err -> throwError err400 {errBody = BL.pack $ "Invalid customer ID: " <> err}
        Right cid -> do
          result <- (service ^. #findCustomer) cid & runExceptT & liftIO
          case result of
            Left (CustomerNotFound _) ->
              throwError err404 {errBody = "Customer not found for ID: " <> BL.pack cidStr}
            Right customer -> return $ CustomerApiOutput.fromCustomer customer

    createCustomerHandler :: CreateCustomerApiInput -> Handler CustomerApiOutput
    createCustomerHandler input = do
      let command = CreateCustomerApiInput.toDomain input
      result <- (service ^. #createCustomer) command & runExceptT & liftIO
      case result of
        Left (CustomerAlreadyExists cid) ->
          throwError err409 {errBody = "Customer already exists with ID: " <> BL.pack (show $ unCustomerId cid)}
        Right customer -> return $ CustomerApiOutput.fromCustomer customer
