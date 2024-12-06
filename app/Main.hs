{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import Gateway.AppServer
import Infrastructure.Config (loadConfig)
import Infrastructure.Customer.CustomerInMemoryRepository as CustomerInMemoryRepository
import Infrastructure.Order.OrderInMemoryRepository as OrderInMemoryRepository

main :: IO ()
main = do
  customerRepo <- CustomerInMemoryRepository.new
  orderRepo <- OrderInMemoryRepository.new

  config <-
    runExceptT (loadConfig "config.yaml") >>= \case
      Left err -> fail (show err)
      Right cfg -> return cfg

  runServer customerRepo orderRepo config
