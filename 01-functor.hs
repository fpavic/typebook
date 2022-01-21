class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
    fmap' _ [] = []
    fmap' f (x:xs) = f x : fmap' f xs

instance Functor' Maybe where
    fmap' _ Nothing = Nothing
    fmap' f (Just x) = Just $ f x

instance Functor' (Either e) where
    fmap' _ (Left x) = Left x
    fmap' f (Right y) = Right $ f y

instance Functor' ((->) e) where
    fmap' f g = f . g

instance Functor' ((,) e) where
    fmap' f (x, y) = (x, f y)

data Pair a = Pair a a deriving Show

instance Functor' Pair where
    fmap' f (Pair x y) = Pair (f x) (f y)

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor' ITree where
    fmap' f (Leaf g) = Leaf (f . g)
    fmap' f (Node xs) = Node (fmap' (fmap' f) xs)

{-
Type of kind * -> * that can't be made an instance of Functor

newtype T a = T (a -> Int)
-}

{-
The compositions of two functors is also a Functor

newtype Compose f g a = Compose { getCompose :: f (g a)}
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap h (Compose x) = Compose (fmap (fmap h) x)
-}

{-
Functor Laws

fmap id = id 
fmap id $ Just n == id $ Just n

fmap (g . h) == (fmap g) . (fmap h)
fmap ((== 3) . (+2)) $ Just 1 == fmap (== 3) . fmap (+ 2) $ Just 1
-}

{-
Bogus Functor instance which satisfies the composition law but not the identity law.
The opposite is not possible.

data F a = X | Y deriving Show

instance Functor' F where
    fmap' _ _ = Y

fmap' id X != id X
fmap' (id . const X) X  == fmap' id . fmap' (const X) $ X
-}

{-
Bogus Functor instance which doesn't satisfy any law

instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap _ [] = []
  fmap g (x:xs) = g x : g x : fmap g xs

  fmap id [1, 2, 3] == [1, 1, 2, 2, 3, 3] /=
      id [1, 2, 3] == [1, 2, 3]

  fmap ((+1) . (+1)) [1, 2, 3] == [3, 3, 4, 4, 5, 5] /= 
      fmap (+1) . fmap (+1) $ [1, 2, 3] == [3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5]
-}