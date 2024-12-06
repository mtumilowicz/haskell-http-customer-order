module Assertions
  ( assertRightM,
    assertLeftM,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Test.Hspec (expectationFailure)

assertRightM :: (Show err, MonadIO m) => m (Either err a) -> m a
assertRightM action = action >>= assertRight

assertLeftM :: (Show a, MonadIO m) => m (Either err a) -> m err
assertLeftM action = action >>= assertLeft

assertRight :: (Show err, MonadIO m) => Either err a -> m a
assertRight (Left err) = failExpectation $ "Unexpected error: " ++ show err
assertRight (Right value) = return value

assertLeft :: (Show a, MonadIO m) => Either err a -> m err
assertLeft (Left err) = return err
assertLeft (Right value) = failExpectation $ "Expected Left but got Right: " ++ show value

failExpectation :: (MonadIO m) => String -> m a
failExpectation msg = liftIO $ expectationFailure msg >> pure undefined
