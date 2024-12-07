{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import Domain.Customer.CustomerRepository as CustomerRepository
import Domain.Order.OrderRepository as OrderRepository
import Domain.Services
import Domain.Customer.CustomerService
import Domain.Order.OrderService
import Gateway.AppServer
import Infrastructure.Config (loadConfig)

main :: IO ()
main = do
  customerRepository <- CustomerRepository.newInMemoryRepository
  orderRepository <- OrderRepository.newInMemoryRepository
  let customerService = mkCustomerService customerRepository
      orderService = mkOrderService orderRepository

  config <-
    runExceptT (loadConfig "config.yaml") >>= \case
      Left err -> fail (show err)
      Right cfg -> return cfg

  runServer (Services customerService orderService) config
