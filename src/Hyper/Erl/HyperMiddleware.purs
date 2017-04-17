module Hyper.Erl.HyperMiddleware where

import Prelude
import Control.Monad.Eff
import Erl.Cowboy
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Foldable (for_, sequence_, traverse_)
import Data.List (foldM)
import Erl.Atom (Atom, atom)
import Erl.Cowboy.Req (Req)
import Erl.Data.List (List, toUnfoldable)
import Erl.Data.Tuple (Tuple2, tuple2, tuple3)

foreign import fromFn :: forall eff. ProtoEnv -> (Req -> Eff eff Req) -> (Req -> Eff eff Req)

execute :: Req -> List ProtoEnv -> Tuple2 Atom Req
execute req env = unsafePerformEff $ do
  req' <- foldM (\r f -> fromFn f pure $ r) req $ toUnfoldable env
  pure $ tuple2 (atom "stop") req'
