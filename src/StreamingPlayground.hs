{-# LANGUAGE RankNTypes #-}
module StreamingPlayground

where

import Conduit
import Data.MonoTraversable (Element, MonoFoldable, ofoldMap)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B


-- https://haskell-lang.org/library/conduit
myYieldMany :: (Monad m, MonoFoldable mono) => mono -> Producer m (Element mono)
myYieldMany =
  ofoldMap yield


yieldManyList :: Monad m => [a] -> Producer m a
yieldManyList [] =
  return ()
yieldManyList (x:xs) =
  yield x >> yieldManyList xs


myMapC :: Monad m => (i -> o) -> ConduitM i o m ()
myMapC f =
  loop
  where
    loop = do
      mx <- await
      case mx of
        Nothing -> return ()
        Just x -> do
          yield (f x)
          loop


myFilterC :: Monad m => (i -> Bool) -> ConduitM i i m ()
myFilterC p =
  loop
  where
    loop = do
      mx <- await
      maybe (return ()) (\x -> if p x then yield x >> loop else loop) mx


myMapMC :: Monad m => (i -> m o) -> ConduitM i o m ()
myMapMC f =
  loop
  where
    loop = do
      mx <- await
      case mx of
        Nothing ->
          return ()

        Just x -> do
          x' <- lift (f x)
          yield x'
          loop


myMapM_C :: Monad m => (i -> m ()) -> ConduitM i o m ()
myMapM_C f =
  loop
  where
    loop = do
      mx <- await
      case mx of
        Nothing ->
          return ()

        Just x -> do
          _ <- lift (f x)
          loop


myPeek :: Monad m => ConduitM i o m (Maybe i) -- Consumer i m (Maybe i)
myPeek = do
  mx <- await
  maybe (return Nothing) (\x -> leftover x >> return (Just x)) mx


mySinkNull :: Monad m => ConduitM i o m ()
mySinkNull =
  loop
  where
    loop = do
      mx <- await
      maybe (return ()) (const loop) mx


myTakeCE :: Monad m => Int -> ConduitM B.ByteString B.ByteString m ()
myTakeCE n = do
  mx <- await
  maybe (return ()) myTakeE mx
  where
    myTakeE xs =
      let
        (ls, rs) =
          B.splitAt n xs
      in
        yield ls >> leftover rs

{-
runConduit $ yield xs
  .| do myTakeCE 1 .| myMapM_C print
        myMapM_C print

output:
"f"
"oo"
-}


tagger :: Monad m => ConduitM Int (Either Int Int) m ()
tagger =
  mapC $ \i -> if even i then Left i else Right i


evens, odds :: Monad m => ConduitM Int String m ()
evens =
  mapC $ \i -> "Even number: " ++ show i


odds =
  mapC $ \i -> "Odd number: "++ show i


left :: Either l r -> Maybe l
left =
  either Just (const Nothing)


right :: Either l r -> Maybe r
right =
  either (const Nothing) Just


inside :: Monad m => ConduitM (Either Int Int) String m ()
inside =
  getZipConduit
    $ ZipConduit (concatMapC left .| evens)
    *> ZipConduit (concatMapC right .| odds)


main :: IO ()
main = do
  runConduit $ yieldManyList [1..10]
    .| myMapC (+1)
    .| myFilterC (<10)
    .| do myPeek >>= liftIO . putStrLn . show
          myMapM_C print
    -- this doesn't print anything since everything has been consumed
    .| myMapM_C (print :: Int -> IO ())

  runConduit $ enumFromToC 1 10
    .| tagger
    .| inside
    .| mapM_C putStrLn

  let src = yieldMany [1..3 :: Int]
      conduit1 = mapC (+1)
      conduit2 = concatMapC (replicate 2)
      conduit = getZipConduit $ ZipConduit conduit1 *> ZipConduit conduit2
      sink = mapM_C print
  -- src $$ conduit =$ sink
  runConduit $ src
    .| conduit -- () <$ sequenceConduits [conduit1, conduit2]
    .| sink
