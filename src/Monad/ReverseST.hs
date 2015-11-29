-------------------------------------------------------------------------------
-- A reverse state transformer
--
-- Pepe Gallardo, 2015
-------------------------------------------------------------------------------

module Monad.ReverseST(RST,get,set,run) where

newtype RST s a = RST (s -> (s,a))

instance Functor (RST s) where
  fmap f (RST st) =
    RST $ \s -> let (s',x) = st s in (s', f x)

instance Applicative (RST s) where
  pure x = RST $ \s -> (s, x)
  RST mf <*> RST st =
    RST $ \s0 -> let
      (s1, f) = mf s2
      (s2, x) = st s0
      in (s1, f x)

instance Monad (RST s) where
  RST st1 >>= f =
    RST $ \s0 -> let
      (s1, x) = st1 s2
      RST st2 = f x
      (s2, y) = st2 s0
      in (s1, y)

get :: RST s s
get = RST $ \s -> (s, s)

set :: s -> RST s ()
set s = RST $ const (s, ())

run :: s -> RST s a -> a
run s (RST st) = let (_, x) = st s in x

demo :: RST Integer (Integer, Integer, Integer)
demo = do
  x <- get
  set 10
  y <- get
  set 20
  z <- get
  return (x,y,z)

nats :: RST [Integer] [Integer]
nats = do
    xs <- get
    set (0 : map (+ 1) xs)
    return xs

main :: IO ()
main = do
  print (run 0 demo)
  print (take 20 $ run [] nats)
