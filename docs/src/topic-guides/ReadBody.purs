module ReadBody where

import Prelude
import Control.IxMonad ((:>>=), (:*>))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Request (class ReadableBody, getRequestData, readBody)
import Hyper.Response (class Response, class ResponseWritable, ResponseEnded, StatusLineOpen, closeHeaders, respond, writeStatus)
import Hyper.Status (statusBadRequest, statusMethodNotAllowed)
import Node.Buffer (BUFFER)
import Node.HTTP (HTTP)

onPost
  :: forall m b req res c
  . Monad m
  => ReadableBody req m String
  => Response res m b
  => ResponseWritable b m String
  => Middleware
     m
     (Conn req (res StatusLineOpen) c)
     (Conn req (res ResponseEnded) c)
     Unit
-- start snippet onPost
onPost =
  readBody :>>=
  case _ of
    "" ->
      writeStatus statusBadRequest
      :*> closeHeaders
      :*> respond "... anyone there?"
    msg ->
      writeStatus statusBadRequest
      :*> closeHeaders
      :*> respond ("You said: " <> msg)
-- end snippet onPost

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, exception :: EXCEPTION, avar :: AVAR, buffer :: BUFFER | e) Unit
main =
  let
    router =
      _.method <$> getRequestData :>>=
      case _ of
        Left POST -> onPost
        Left method ->
          writeStatus statusMethodNotAllowed
          :*> closeHeaders
          :*> respond ("Method not supported: " <> show method)
        Right customMethod ->
          writeStatus statusMethodNotAllowed
          :*> closeHeaders
          :*> respond ("Custom method not supported: " <> show customMethod)

  -- Let's run it.
  in runServer defaultOptionsWithLogging {} router
