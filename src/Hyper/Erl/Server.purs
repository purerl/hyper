module Hyper.Erl.Server where

import Prelude
import Control.IxMonad (ipure, (:*>), (:>>=))
import Control.Monad.Eff (Eff)
import Data.HTTP.Method (fromString)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.StrMap (empty)
import Data.Tuple (Tuple(..))
import Erl.Atom (Atom, atom)
import Erl.Cowboy (EnvValue, PROCESS, ProtoOpt(..), TransOpt(..), startHttp, toEnvValue)
import Erl.Cowboy.Req (Req, StatusCode(..), replyStatus, setBody, setHeader, path, method)
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (Tuple2, Tuple4, tuple2, tuple4)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, evalMiddleware)
import Hyper.Middleware.Class (getConn, modifyConn)
import Hyper.Request (class Request, RequestData, parseUrl)
import Hyper.Response (class Response, class ResponseWritable, ResponseEnded, StatusLineOpen)

data HttpRequest = HttpRequest RequestData
data HttpResponse a = HttpResponse Req (Maybe StatusCode)

mkHttpRequest :: Req -> HttpRequest
mkHttpRequest req = HttpRequest { url, parsedUrl, contentLength, headers, method: method' }
  where
  contentLength = Nothing
  url = path req
  parsedUrl = defer \_ -> parseUrl url
  method' = fromString (method req)
  headers = empty

instance requestHttpRequest :: Monad m => Request HttpRequest m where
  getRequestData = do
    getConn :>>=
    case _ of
      { request: HttpRequest d } -> ipure d

type ServerOptions = {
  port :: Int,
  ip :: IP
}

data IP = IP Int Int Int Int

ipToTuple :: IP → Tuple4 Int Int Int Int
ipToTuple (IP a b c d) = tuple4 a b c d

defaultServerOpts :: ServerOptions
defaultServerOpts = {
  port: 8080,
  ip: IP 0 0 0 0
}

newtype StringResponse = StringResponse String
 -- let statusCode = StatusCode $ _.code $ unwrap status in

instance responseHttpResponse :: (Monad m) => Response HttpResponse m StringResponse where
  -- Status is always set when sending, store it
  writeStatus status = modifyConn (\(c@{ response: HttpResponse req _ }) →
    let statusCode = StatusCode $ _.code $ unwrap status in
    c { response = HttpResponse req (Just statusCode) })

  -- Accumulate headers
  writeHeader (Tuple k v) = modifyConn (\(c@{ response: HttpResponse req s }) →
    c { response = HttpResponse (setHeader k v req) s })

  -- Noop, headers are sent when sending
  closeHeaders = modifyConn (\(c@{ response: HttpResponse req s }) → c { response = HttpResponse req s })

  send (StringResponse r) = modifyConn (\(c@{ response: HttpResponse req s }) →
    c { response = HttpResponse (setBody r req) s })

  end = modifyConn (\(c@{ response: HttpResponse req s }) →
    let status = fromMaybe (StatusCode 500) s in
    c { response = HttpResponse (replyStatus status req) Nothing })


instance responseWritableStringResponse :: (Monad m) =>  ResponseWritable StringResponse m String where
  toResponse = ipure <<< StringResponse

serverFn :: forall eff. (Req → Eff eff Req) → Tuple2 Atom EnvValue
serverFn f = tuple2 (atom "fn") (toEnvValue f)

runServer
  :: forall e c c'.
     ServerOptions
  → c
  → Middleware
     (Eff (process :: PROCESS | e))
     (Conn HttpRequest (HttpResponse StatusLineOpen) c)
     (Conn HttpRequest (HttpResponse ResponseEnded) c')
     Unit
  → (Eff (process :: PROCESS | e)) Unit
runServer { port, ip } components middleware = do
  let transOpts = Ip (ipToTuple ip) : Port port : nil
      protoOpts = Env (serverFn middlewareFn : nil)
        : Middlewares  (atom "hyper_erl_hyperMiddleware@ps" : nil)
        : nil
  _ ← startHttp 10 transOpts protoOpts
  pure unit

  where
    middlewareFn :: Req → Eff (process :: PROCESS | e) Req
    middlewareFn req = do

      let conn = { request: mkHttpRequest req
                 , response: HttpResponse req Nothing
                 , components: components
                 }
      { response: HttpResponse req' _ } ← evalMiddleware middleware conn
      pure req'
