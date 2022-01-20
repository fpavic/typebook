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
-}
newtype T a = T (a -> Int)

{-
The compositions of two functors is also a Functor

newtype Compose f g a = Compose { getCompose :: f (g a)}
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap h (Compose x) = Compose (fmap (fmap h) x)
-}