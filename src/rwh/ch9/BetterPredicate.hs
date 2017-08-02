module RWH.Ch9.BetterPredicate
where

import RWH.Ch9.ControlledVisit (Info(..))


type InfoP a =
  Info -> a


pathP :: InfoP FilePath
pathP =
  infoPath


sizeP :: InfoP (Maybe Integer)
sizeP =
  infoSize


-- equalP :: Eq a => (Info -> a) -> a -> (Info -> Bool)
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f x info =
   f info == x


-- liftP :: (a -> b -> c) -> (Info -> a) -> b -> (Info -> c)
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP f g y info =
  f (g info) y


-- liftP :: (a -> b -> c) -> (Info -> a) -> (Info -> b) -> (Info -> c)
liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 f g h info =
  f (g info) (h info)


-- liftP' :: (a -> b -> c) -> (Info -> a) -> b -> (Info -> c)
liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP' f g y =
  liftP2 f g (const y)


infix 4 ==?
(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) =
  equalP

infixr 3 &&?, ||?
(&&?), (||?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) =
  liftP2 (&&)


(||?) =
  liftP2 (||)


infix 4 >?, <?
(>?), (<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) =
  liftP' (>)


(<?) =
  liftP' (<)
