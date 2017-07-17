{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RWH.Ch15.HandleClass ( MonadHandle (..) )
where

import qualified System.IO as SIO
import qualified Control.Monad.Writer as W


-- we can use ANY monad, not only IO! Great for testing purposes for example
class Monad m => (MonadHandle m) h | m -> h where
  openFile :: FilePath -> SIO.IOMode -> m h
  hClose :: h -> m ()
  hPutStr :: h -> String -> m ()
  hGetContents :: h -> m String

  hPutStrLn :: h -> String -> m ()
  hPutStrLn h s =
    hPutStr h s >> hPutStr h "\n"


instance (MonadHandle IO) SIO.Handle where
  openFile =
    SIO.openFile

  hClose =
    SIO.hClose

  hPutStr =
    SIO.hPutStr

  hGetContents =
    SIO.hGetContents

  hPutStrLn =
    SIO.hPutStrLn


data Event =
  Open FilePath SIO.IOMode
  | Put String String
  | Close String
  | GetContents String
  deriving Show


newtype WriterIO a =
  W { runW :: W.Writer [Event] a }
  deriving (Functor, Applicative, Monad, W.MonadWriter [Event])


-- UHU ?! The book claims this instance is not needed but without ghci complains: `No instance for (MonadHandle WriterIO h0) arising from a use of ‘safeHello`…how can the compiler know how to handle the different actions for WriterIO if they're not specified ?!
instance (MonadHandle WriterIO) String where
  openFile p m =
    do W.tell [Open p m]; return p

  hClose h =
    do W.tell [Close h]; return ()

  hPutStr h xs =
    do W.tell [Put h xs]; return ()

  hGetContents h =
    do W.tell [GetContents h] ; return h

  hPutStrLn h xs =
    do W.tell [Put h xs] ; return ()


runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO =
  W.runWriter . runW


safeHello :: MonadHandle m h => FilePath -> m ()
safeHello path =
  do h <- openFile path SIO.WriteMode ; hPutStrLn h "hello world" ; hClose h


-- Ex. 1
