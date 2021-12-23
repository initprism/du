{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppRTWTST where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import AppTypes

newtype MyApp w s a = MyApp { runApp :: ReaderT AppEnv (WriterT [w] (StateT s IO)) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadWriter [w], MonadState s)

runMyApp :: MyApp w s a -> AppConfig -> s -> IO (a, [w])
runMyApp app config = evalStateT (runWriterT (runReaderT (runApp app) (initialEnv config)))
