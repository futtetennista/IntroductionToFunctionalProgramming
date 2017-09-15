{-# LANGUAGE FlexibleContexts #-}
module Ch27.SyslogUDPClient
where


import Data.Bits
import Network.Socket hiding (sendTo)
import Network.Socket.ByteString (sendTo)
import Ch27.SyslogTypes
import qualified Data.ByteString.Char8 as Strict
import qualified Data.ByteString.Lazy.Char8 as Lazy (toStrict)
import Data.ByteString.Builder (Builder, toLazyByteString, stringUtf8, charUtf8, byteString)
import Data.Monoid ((<>))
import Control.Monad.Except (MonadError, throwError, catchError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception -- (IOException)
import Data.Typeable (Typeable)
-- import Control.Monad.Catch
-- import Control.Monad (join)


data SyslogHandle =
  SyslogHandle { slSocket :: Socket,
                 slProgram :: Strict.ByteString,
                 slAddress :: SockAddr
               }


-- port number or name
type Port =
  Strict.ByteString


type ProgName =
  Strict.ByteString


openlog :: MonadIO m => HostName -> Port -> ProgName -> m SyslogHandle
openlog hostname port progname = do
  addrinfos <- liftIO $ getAddrInfo Nothing (Just hostname) (Just (Strict.unpack port))
  let
    serveraddr =
      head addrinfos
  sock <- liftIO $ socket (addrFamily serveraddr) Datagram defaultProtocol
  return $ SyslogHandle sock progname (addrAddress serveraddr)


data SyslogException
  = InvalidCode
  | Generic String
  deriving (Show, Typeable)


instance Exception SyslogException


-- Goal: catch both SyslogExceptions and IOExceptions

{-
1st try: use the MonadError type class.

Issue: the context can never be satisfied in the IO monad!
That's because there is a fundep in the MonadError class declaration:
  class MonadError e m | m -> e

and the instance for IO is:
  instance MonadError IOException IO

so the following won't compile:
  foo :: SyslogHandle -> IO ()
  foo h = syslog h fac prio "foo"
-}
syslog :: (MonadError SomeException m, MonadIO m)
       => SyslogHandle
       -> Facility
       -> Priority
       -> Strict.ByteString
       -> m ()
syslog syslogh fac pri msg =
  case toSyslogCode fac pri of
    Nothing ->
      throwError (toException InvalidCode)

    Just code -> do
      eres <- runExceptT $ sendstr $ Lazy.toStrict (toLazyByteString sendmsgBuilder)
      either (throwError . toException) return eres
      where
        sendmsgBuilder :: Builder
        sendmsgBuilder =
          charUtf8 '<' <>  stringUtf8 (show code) <> charUtf8 '>'
            <> byteString (slProgram syslogh) <> stringUtf8 ": " <> byteString msg

        sendstr :: (MonadError IOException m, MonadIO m)
                => Strict.ByteString
                -> m ()
        sendstr omsg
          | Strict.null omsg =
              return ()
          | otherwise = do
              liftIO $ catchError
                (() <$ sendTo (slSocket syslogh) omsg (slAddress syslogh))
                throwError

{-
2hd try: do without the MonadError type class and manually handle failure

Issue: it's re-doing what MonadError already does under the hood
-}
syslog' :: (MonadIO m)
       => Facility
       -> Priority
       -> Strict.ByteString
       -> SyslogHandle
       -> m (Either SomeException ())
syslog' fac pri msg h =
  case toSyslogCode fac pri of
    Nothing ->
      return . Left . toException $ InvalidCode

    Just code -> do
      liftIO $ catch
        (Right <$> sendstr (Lazy.toStrict (toLazyByteString sendmsgBuilder)))
        (return . Left . toException :: MonadIO m
                                     => IOException
                                     -> m (Either SomeException ()))
      where
        sendmsgBuilder :: Builder
        sendmsgBuilder =
         charUtf8 '<' <>  stringUtf8 (show code) <> charUtf8 '>'
           <> byteString (slProgram h) <> stringUtf8 ": " <> byteString msg

        sendstr :: (MonadIO m) => Strict.ByteString -> m ()
        sendstr omsg
          | Strict.null omsg =
              return ()
          | otherwise = do
              sent <- liftIO $ sendTo (slSocket h) omsg (slAddress h)
              sendstr (Strict.drop sent omsg)
              -- liftIO $ throwIO (userError "Boom") -- caught
              -- liftIO $ throwIO InvalidCode -- uncaught and the type system doesn't help


closelog :: MonadIO m => SyslogHandle -> m ()
closelog syslogh =
  liftIO $ close (slSocket syslogh)


toSyslogCode :: Facility -> Priority -> Maybe Int
toSyslogCode fac prio =
  mkCode <$> codeOfFac fac <*> Just (fromEnum prio)
  where
    mkCode :: Int -> Int -> Int
    mkCode facCode prioCode =
      (facCode `shiftL` 3) .|. prioCode


oneshotlog :: HostName -> Port -> ProgName
           -> Facility -> Priority -> Strict.ByteString
           -> IO (Either SomeException ())
oneshotlog hn p pn fac prio msg =
  bracket (openlog hn p pn) closelog (syslog' fac prio msg)
  -- this doesn't compile: how can I solve it? Maybe ExceptT isn't the right tool for the job
  -- bracket (openlog hn p pn) closelog (\h -> syslog h fac prio msg)
