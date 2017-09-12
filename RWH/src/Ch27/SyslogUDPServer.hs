module Ch27.SyslogUDPServer
where


import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as Strict

type HandlerFunc =
  SockAddr -> Strict.ByteString -> IO ()


type Port =
  Strict.ByteString


serveLog :: Port -> HandlerFunc -> IO ()
serveLog port handlerfunc =
  withSocketsDo $ do
    addrInfos <- getAddrInfo (Just addrInfo) Nothing (Just (Strict.unpack port))
    let
      serverAddr =
        head addrInfos
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    bind sock (addrAddress serverAddr)
    procMessages sock
      where
        addrInfo =
          defaultHints {addrFlags = [AI_PASSIVE]}

        procMessages sock = do
          (msg, addr) <- recvFrom sock 1024
          handlerfunc addr msg
          procMessages sock


-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg =
  putStrLn $ "From " ++ show addr ++ ": " ++ Strict.unpack msg
