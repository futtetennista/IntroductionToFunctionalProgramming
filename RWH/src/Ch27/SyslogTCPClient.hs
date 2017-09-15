module Ch27.SyslogTCPClient
where

import Data.Bits
import Network.Socket
import Ch27.SyslogTypes
import System.IO
import qualified Data.ByteString.Char8 as Strict
import qualified Data.ByteString.Lazy.Char8 as Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString, stringUtf8, charUtf8, byteString)
import Data.Monoid ((<>))
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)


data SyslogHandle =
  SyslogHandle { slHandle :: !Handle
               , slProgram :: !Strict.ByteString
               }


-- port number or name
type Port =
  Strict.ByteString


type ProgName =
  Strict.ByteString


openlog :: (MonadIO m) => HostName -> Port -> ProgName -> m SyslogHandle
openlog hostname port progname =
  liftIO $ do
    addrinfos <- getAddrInfo Nothing (Just hostname) $ Just (Strict.unpack port)
    let
      serverAddr =
        head addrinfos
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serverAddr)
    h <- socketToHandle sock WriteMode
    hSetBuffering h (BlockBuffering Nothing)
    return $ SyslogHandle h progname


syslog :: (MonadIO m) => Facility -> Priority -> Strict.ByteString -> SyslogHandle -> m ()
syslog fac prio msg h =
  liftIO $ do
    hPutStrLn (slHandle h) msg'
    hFlush (slHandle h)
  where
    code =
      makeCode fac prio

    msg' =
      Strict.unpack . Lazy.toStrict . toLazyByteString $ sendMsgBuilder

    sendMsgBuilder =
      charUtf8 '<' <> stringUtf8 (show code) <> charUtf8 '>'
        <> byteString (slProgram h) <> stringUtf8 ": " <> byteString msg


closelog :: MonadIO m => SyslogHandle -> m ()
closelog syslogh =
  liftIO $ hClose (slHandle syslogh)


makeCode :: Facility -> Priority -> Maybe Int
makeCode fac prio =
  (\facCode -> (facCode `shiftL` 3) .|. priocode) <$> codeOfFac fac
  where
    priocode =
      fromEnum prio


oneshotlog :: HostName -> Port -> ProgName
           -> Facility -> Priority -> Strict.ByteString
           -> IO ()
oneshotlog hn p pn fac prio msg =
  bracket (openlog hn p pn) closelog (syslog fac prio msg)
