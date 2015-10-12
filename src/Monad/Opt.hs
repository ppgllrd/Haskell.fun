-------------------------------------------------------------------------------
-- Monad for computations that may fail
--
-- Pepe Gallardo, 2015
-------------------------------------------------------------------------------

module Opt(Opt,Exception,err,catch,run) where

type Exception = String

data Opt a = OK a | Err Exception

instance Functor Opt where
  fmap f (OK a)  = OK (f a)
  fmap f (Err s) = Err s

instance Applicative Opt where
  pure x = OK x
  OK f  <*> OK x  = OK (f x)
  _     <*> Err s = Err s
  Err s <*> _     = Err s

instance Monad Opt where
  (OK x) >>= f = f x
  Err s  >>= f = Err s

catch :: Opt a -> (Exception -> Opt a) -> Opt a
OK x  `catch` f = OK x
Err s `catch` f = f s

err :: Exception -> Opt a
err = Err

run :: Opt a -> a
run (OK x)  = x
run (Err s) = error s

divi :: (Integral a) => a -> a -> Opt a
divi x 0 = err "division by 0"
divi x y = return (div x y)

demo :: (Integral a) => a -> a -> Opt a
demo x y = do
  let z = 10 * x
  divi z y

demo' :: (Integral a) => a -> a -> Opt a
demo' x y = demo x y `catch` f
  where
    f "division by 0" = return 1000
    f s               = err s

main :: IO ()
main = do
  print (run $ demo 100 20)
  print (run $ demo' 100 0)
  print (run $ demo 100 0)
