{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import Domain.Customer.CustomerRepository as CustomerRepository
import Domain.Order.OrderRepository as OrderRepository
import Gateway.AppServer
import Infrastructure.Config (loadConfig)

main :: IO ()
main = do
  customerRepo <- CustomerRepository.newInMemoryRepository
  orderRepo <- OrderRepository.newInMemoryRepository

  config <-
    runExceptT (loadConfig "config.yaml") >>= \case
      Left err -> fail (show err)
      Right cfg -> return cfg

  runServer customerRepo orderRepo config
