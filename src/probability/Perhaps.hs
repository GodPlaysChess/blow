{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Probability.Perhaps
where

newtype Prob = P Float
    deriving (Eq, Ord, Num)

instance Show Prob where
    show (P p) = show intPart ++ "." ++ show fracPart ++ "%"
        where digits = round (1000 * p)
              intPart = digits `div` 10
              fracPart = digits `mod` 10

data Perhaps a = Perhaps a Prob
      deriving (Show, Functor)



neverHappens :: Perhaps a -> Bool
neverHappens (Perhaps _ 0) = True
neverHappens _             = False


instance Applicative Perhaps where
        pure = return
        ff <*> fa = ff >>= \f -> f <$> fa

instance Monad Perhaps where
      return x = Perhaps x 1
      ph >>= f| neverHappens ph  = never
              | otherwise        = Perhaps x (p1 * p2)
        where (Perhaps (Perhaps x p1) p2) = fmap f ph

class Monad m => MonadPerhaps m where
        perhaps :: a -> Prob -> m a
        never :: m a

instance MonadPerhaps Perhaps where
        never = Perhaps undefined 0
        perhaps = Perhaps

--         newtype PerhapsT m a = PerhapsT { runPerhapsT :: m (Perhaps a) }

-- instance MonadTrans PerhapsT where
--         lift x = PerhapsT (liftM return x)

-- instance Monad m => Functor (PerhapsT m) where
--         fmap = liftM

-- instance Monad m => Monad (PerhapsT m) where
--         return = lift . return
--         m >>= f = PerhapsT bound
--                 where bound = do
--                         ph <- runPerhapsT m
--                         case ph of
--                              (Perhaps x1 p1)  | p1 == 0    -> return never
--                                                | otherwise  -> do
--                                 (Perhaps x2 p2) <- runPerhapsT (f x1)
--                                 return (Perhaps x2 (p1 * p2))
