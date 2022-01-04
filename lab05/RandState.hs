module RandState where

import System.Random

newtype RandState a = RandState {
  runRandState :: StdGen -> (a, StdGen)
} 

instance Functor RandState where
  fmap f p = RandState $ \s -> let (a,t) = runRandState p s in (f a,t)

instance Applicative RandState where
  pure a = RandState $ \s -> (a,s)
  af <*> aa = RandState $ \s -> 
        let (f,t)= runRandState af s 
            (a,u) = runRandState aa t 
        in (f a, u)

instance Monad RandState where
  ma >>= f = RandState $ \s -> 
        let (a,t) = runRandState ma s 
            mb = f a
            (b,u) = runRandState mb t
        in (b,u)

{- primitive manipulation functions -}

get :: RandState StdGen
get = RandState $ \gen -> (gen, gen)

put :: StdGen -> RandState ()
put gen' = RandState $ \gen -> ((), gen')

-- runRandom runs a RandState monad, given an initial random number generator
-- runRandom is equivalent to evalState
runRandom :: RandState a -> StdGen -> a
runRandom (RandState f) s = fst $ f s

-- rand is a helper function that generates a random instance of any
--  type in the Random class, using the RandState monad.
rand :: Random a => RandState a
rand = do
    gen <- get
    let (x, gen') = random gen
    put gen'
    return x
