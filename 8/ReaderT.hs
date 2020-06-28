module CustomReaderT where
newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }
newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T (\e1 e2 -> return (f e1 e2))

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T (\e1 e2 e3 -> return (f e1 e2 e3))

instance Functor m => Functor (Arr2T e1 e2 m) where
  fmap f (Arr2T g) = arr2 $ \e1 e2 -> f $ g e1 e2