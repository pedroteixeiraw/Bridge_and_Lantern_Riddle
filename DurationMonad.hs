module DurationMonad where

-- Defining a monad (the duration monad) --
data Duration a = Duration (Int, a) deriving Show

getDuration :: Duration a -> Int
getDuration (Duration (d,_)) = d

getValue :: Duration a -> a
getValue (Duration (_,x)) = x

-- Functor Duration
instance Functor Duration where
  fmap f (Duration (i,x)) = Duration (i,f x)

-- Applicative Duration
instance Applicative Duration where
  pure x = (Duration (0,x))
  (Duration (i,f)) <*> (Duration (j, x)) = (Duration (i+j, f x))

-- Monad Duration  
instance Monad Duration where
    (Duration (i,x)) >>= k = Duration (i + (getDuration (k x)), getValue (k x))
    return = pure

wait1 :: Duration a -> Duration a
wait1 (Duration (d,x)) = Duration (d+1,x)

wait :: Int -> Duration a -> Duration a
wait i (Duration (d,x)) = Duration (i + d, x) 