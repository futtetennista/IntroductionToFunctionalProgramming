{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RWH.Ch15.HandleClass ( MonadHandle (..) )
where

import qualified System.IO as SIO
import qualified Control.Monad.Writer as W
import qualified Data.Sequence as Seq


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
  W { runW :: W.Writer (Seq.Seq Event) a }
  deriving (Functor, Applicative, Monad, W.MonadWriter (Seq.Seq Event))


-- UHU ?! The book claims this instance is not needed but without ghci complains: `No instance for (MonadHandle WriterIO h0) arising from a use of ‘safeHello`…how can the compiler know how to handle the different actions for WriterIO if they're not specified ?!
-- Aha! I misinterpreted what they wrote: they were referring to creating a new type instead of making W.Writer an instance of MonadHandle, not that the type class should not be instanciated.
instance (MonadHandle WriterIO) FilePath where
  openFile p m =
    W.tell (Seq.singleton $ Open p m) >> return p

  hClose h =
    W.tell (Seq.singleton $ Close h) >> return ()

  hPutStr h xs =
    W.tell (Seq.singleton $ Put h xs) >> return ()

  hGetContents h =
    W.tell (Seq.singleton $ GetContents h) >> return h

  hPutStrLn h xs =
    W.tell (Seq.singleton $ Put h xs) >> return ()


runWriterIO :: WriterIO a -> (a, Seq.Seq Event)
runWriterIO =
  W.runWriter . runW


safeHello :: MonadHandle m h => FilePath -> m ()
safeHello path =
  do h <- openFile path SIO.WriteMode ; hPutStrLn h "hello world" ; hClose h


-- Ex. 1
