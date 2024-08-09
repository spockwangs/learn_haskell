{-# LANGUAGE FlexibleContexts #-}
module SafeDiv where

import Control.Monad.Error.Class

data Show a =>
  DivByError a = DivBy0
               | ForbiddenDenominator a
               | OtherDivByError String
               deriving (Eq, Read, Show)

divByGeneric :: (Show a, Integral a, MonadError (DivByError a) m) => a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0:_) = throwError DivBy0
divByGeneric _ (10:_) = throwError (ForbiddenDenominator 10)
divByGeneric numerator (denominator:xs) = do
  next <- divByGeneric numerator xs
  return ((numerator `div` denominator) : next)

divBy :: (Integral a, Show a) => a -> [a] -> Either (DivByError a) [a]
divBy = divByGeneric
