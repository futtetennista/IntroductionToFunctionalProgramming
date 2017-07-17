{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RWH.Ch15.HandleIO ( HandleIO
                         , SIO.Handle
                         , SIO.IOMode (..)
                         , runHandleIO
                         , openFile
                         , hClose
                         , hPutStrLn
                         )
where

import Control.Monad.Trans (MonadIO (..))
import qualified System.IO as SIO


newtype HandleIO a =
  HandleIO { runHandleIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)


openFile :: FilePath -> SIO.IOMode -> HandleIO SIO.Handle
openFile path mode =
  HandleIO (SIO.openFile path mode)


hClose :: SIO.Handle -> HandleIO ()
hClose =
  HandleIO . SIO.hClose


hPutStrLn :: SIO.Handle -> String -> HandleIO ()
hPutStrLn h s =
  HandleIO (SIO.hPutStrLn h s)

-- runHandleIO (safeHello "goodbye" >> removeFile "goodbye") 💥


-- instance MonadIO HandleIO where
  -- liftIO :: HandleIO => IO a -> HandleIO a
--   liftIO = HandleIO
