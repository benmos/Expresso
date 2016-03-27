{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Utils(
  Fix(..),
  cata,
  para,
  (&&&),
  first,
  second,
  tshow
)
where

import qualified Data.Text as T

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . unFix

para :: Functor f => (f (b, Fix f) -> b) -> Fix f -> b
para phi = phi . fmap (para phi &&& id) . unFix

-- Equivalent to specialized version from Arrow
(&&&) :: (a -> b) -> (a -> c) -> (a -> (b,c))
f &&& g = \a -> (f a, g a)

-- Equivalent to specialized version from Arrow
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

-- Equivalent to specialized version from Arrow
second :: (b -> c) -> (a,b) -> (a,c)
second f (a,b) = (a, f b)

instance (Functor f, Show (f (Fix f))) => Show (Fix f) where
    showsPrec d (Fix f) = showsPrec d f

instance (Functor f, Eq (f (Fix f))) => Eq (Fix f) where
    fa == fb = unFix fa == unFix fb

instance (Functor f, Ord (f (Fix f))) => Ord (Fix f) where
    compare fa fb = compare (unFix fa) (unFix fb)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

