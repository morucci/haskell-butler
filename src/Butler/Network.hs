module Butler.Network (WaiApplication, webService) where

import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

import Butler.Prelude

type WaiApplication = Wai.Application

webService :: [XStaticFile] -> Wai.Application -> Port -> IO Void
webService xs app port = do
  Warp.run port handler
  fail "warp exited?!"
  where
    staticApp = xstaticApp xs
    handler req resp =
        app
            req
            ( \appResp -> case HTTP.statusCode (Wai.responseStatus appResp) of
                404 -> staticApp req resp
                _ -> resp appResp
            )
