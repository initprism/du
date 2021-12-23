module AppRWST where

import Control.Monad.RWS
import AppTypes

type MyApp w s = RWST AppEnv [w] s IO

runMyApp :: MyApp w s a -> AppConfig -> s -> IO (a, [w])
runMyApp app config = evalRWST app (initialEnv config)