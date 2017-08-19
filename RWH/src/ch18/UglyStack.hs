{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ch18.UglyStack
where

import qualified Data.Monoid as Mono
import qualified Data.Sequence as Seq
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W
import qualified Control.Monad.IO.Class as IO
import qualified System.Directory as D
import qualified System.FilePath as FP


data AppConfig
  = AppConfig { cfgMaxDepth :: Int }
  deriving (Show)


data AppState
  = AppState { stDeepestReached :: Int }
  deriving Show


-- Ex. 1, 2
type App =
  -- R.ReaderT AppConfig (S.StateT AppState IO)
  -- S.StateT AppState (R.ReaderT AppConfig IO)
  S.StateT AppState (W.WriterT (Seq.Seq (String, Int)) (R.ReaderT AppConfig IO))


-- runApp :: App a -> Int -> IO (a, AppState)
runApp :: App a -> Int -> IO ((a, AppState), Seq.Seq (String, Int))
runApp app maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in
    -- S.runStateT (R.runReaderT k config) state
    -- R.runReaderT (S.runStateT app state) config
    R.runReaderT (W.runWriterT (S.runStateT app state)) config


-- Ex. 3
-- constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount :: Int -> FilePath -> MyApp ()
constrainedCount curDepth path = do
  contents <- IO.liftIO . D.listDirectory $ path
  W.tell $ Seq.singleton (path, length contents)
  cfg <- R.ask
  _ <- S.forM contents $ \name -> do
    let newPath = path FP.</> name
    isDir <- IO.liftIO $ D.doesDirectoryExist newPath
    if isDir && curDepth < cfgMaxDepth cfg
      then do let newDepth = curDepth + 1
              st <- S.get
              S.when (stDeepestReached st < newDepth) $
                S.put st { stDeepestReached = newDepth }
              constrainedCount newDepth newPath
      else return () -- else return []
  return () -- return $ (path, length contents) : concat rest


type AppLog =
  Seq.Seq (String, Int)


newtype MyApp a =
  MyA { runA :: R.ReaderT AppConfig (W.WriterT AppLog (S.StateT AppState IO)) a }
  deriving (Functor, Applicative, Monad, IO.MonadIO, R.MonadReader AppConfig, S.MonadState AppState, W.MonadWriter AppLog)


runMyApp :: MyApp a -> Int -> IO ((a, AppLog), AppState)
runMyApp app maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in S.runStateT (W.runWriterT . R.runReaderT (runA app) $ config) state
