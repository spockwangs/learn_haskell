{-# LANGUAGE InstanceSigs #-}
module State where

import System.Random
import Control.Monad (liftM2)

newtype State s a = State {
  runState :: s -> (a, s)
}

instance Functor (State s) where
  fmap :: (a -> b) ->  State s a -> State s b
  fmap f sa = State $ \s -> let (a, s') = runState sa s
                            in (f a, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  sf <*> sa = State $ \s -> let (f, s') = runState sf s
                                (a, s'') = runState sa s'
                            in (f a, s'')

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  m >>= k = State $ \s -> let (a, s') = runState m s
                          in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
--getRandom = get >>= \gen -> let (val, gen') = random gen
--                            in put gen' >> return val
--getRandom = State $ \gen -> let (val, gen') = random gen
--                            in (val, gen')
getRandom = do
  gen <- get
  let (val, gen') = random gen
  put gen'
  return val

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  gen <- getStdGen
  let (result, _) = runState getTwoRandoms gen
  return result
