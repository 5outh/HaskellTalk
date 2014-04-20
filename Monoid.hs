class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

instance Monoid [a] where
  mempty  = [] 
  mappend = (++)

instance Num a => Monoid a where
  mempty  = 0
  mappend = (+)

instance Monoid Bool where
  mempty  = True
  mappend = (&&)