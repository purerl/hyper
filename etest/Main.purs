module Test.Main where

import Prelude
import Control.IxMonad ((:*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Erl.Cowboy (PROCESS)
import Hyper.Conn (Conn)
import Hyper.Erl.Server (defaultServerOpts, runServer)
import Hyper.Middleware (Middleware)
import Hyper.Response (class Response, class ResponseWritable, ResponseEnded, StatusLineOpen, closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)

main :: Eff (console :: CONSOLE, process :: PROCESS) Unit
main = do
  runServer defaultServerOpts unit app
  log "Started server"
  where
    app :: forall m req res c rb
               . ( Monad m
                 , Response res m rb
                 , ResponseWritable rb m String
                 )
              => Middleware
                 m
                 (Conn req (res StatusLineOpen) c)
                 (Conn req (res ResponseEnded) c)
                 Unit
    app = (writeStatus statusOK
            :*> closeHeaders
            :*> respond "Hello, Hyper!\n")
