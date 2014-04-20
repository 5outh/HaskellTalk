import Prelude hiding (Monad(..))

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

instance Monad Maybe where
  return x       = Nothing
  (Just a) >>= f = f a
  Nothing  >>= f = Nothing

instance Monad [] where
  return x  = [x]
  xs >>= f  = concat $ map f xs