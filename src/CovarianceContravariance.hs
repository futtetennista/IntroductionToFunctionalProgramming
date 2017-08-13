{-# LANGUAGE DeriveFunctor #-}
module CovarianceContravariance
where

import Data.Functor.Contravariant


-- https://www.fpcomplete.com/blog/2016/11/covariance-contravariance
-- Positive and negative position - Ex. 3
newtype E1 a =
  E1 (a -> ())


instance Contravariant E1 where
  -- contramap :: (a -> b) -> E1 b -> E1 a :: (a -> b) -> (b -> ()) -> a -> ()
  contramap f (E1 bFunc) =
    E1 $ \a -> bFunc (f a)


newtype E2 a =
  E2 (a -> () -> ())


instance Contravariant E2 where
  -- contramap :: (a -> b) -> E2 b -> E2 a :: (a -> b) -> (b -> () -> ()) -> (a -> () -> ())
  --           :: (a -> b) -> (b -> () -> ()) -> a -> () -> ()
  contramap f (E2 bFunc) =
    E2 $ \a () -> bFunc (f a) ()


newtype E3 a =
  E3 ((a -> ()) -> ())
  deriving Functor


newtype E4 a =
  E4 ((a -> () -> ()) -> ())
  deriving Functor


newtype E5 a =
  E5 ((() -> () -> a) -> ())


instance Contravariant E5 where
  -- contramap :: (a -> b) -> E5 b -> E5 a
  --           :: (a -> b) -> ((() -> () -> b) -> ()) -> ((() -> () -> a) -> ())
  --           :: (a -> b) -> ((() -> () -> b) -> ()) -> (() -> () -> a) -> ()
  contramap f (E5 g) =
    -- E5 $ \h -> g (\() () -> f (h () ()))
    E5 $ \h -> g (\x y -> f (h x y))

{-
(() -> (a -> a)) -> (): a in both + and - positions
==> E6 is invariant
-}
newtype E6 a =
  E6 ((() -> a -> a) -> ())


{-
(() -> a): a in + position
((() -> () -> a) -> a): ((() -> () -> a)) in - position
Multiplication rule: + * - = -
((() -> () -> a) -> a): a in + position
==> E7 is invariant
-}
newtype E7 a =
  E7 ((() -> () -> a) -> a)


{-
(a -> ()) : a in - position
(() -> (a -> ())) : (a -> ()) in + position
(() -> (a -> ())) -> a : () -> (a -> ()) in - position
Multiplication rule: - * + * - = +
(() -> (a -> ())) -> a : a in + position
==> E8 is covariant (both a's are in positive positions)
-}
newtype E8 a =
  -- E8 ((() -> (a -> ())) -> a)
  E8 ((() -> a -> ()) -> a)
  deriving Functor


newtype E9 a =
  E9 ((() -> () -> ()) -> ())
  deriving Functor


instance Contravariant E9 where
  -- contramap :: (a -> b) -> E9 b -> E9 a
  --           :: (a -> b) -> ((() -> () -> ()) -> ()) -> ((() -> () -> ()) -> ())
  --           :: (a -> b) -> ((() -> () -> ()) -> ()) -> (() -> () -> ()) -> ()
  contramap _f (E9 g) =
    -- E9 $ \h -> g (\() () -> h () ())
    E9 $ \h -> g (\x y -> h x y)
