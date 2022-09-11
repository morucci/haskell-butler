module Butler.WebSocket (
    WebSocketAPI,
    WebSocketServer (..),
    websocketServer,
    ChannelName (..),
) where

import Lucid
import Lucid.Base (makeAttribute)
import Network.Socket
import Network.WebSockets qualified as WS
import Servant
import Servant.API.WebSocket
import Servant.HTML.Lucid

import Lucid.Htmx
import Servant.Auth as SA
import Servant.Auth.Server as SAS

import Butler.OS
import Butler.Prelude
import Butler.Session
import Web.FormUrlEncoded (FromForm)

type AuthResp = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ())

type LoginAPI =
    QueryParam "invite" InviteID :> Get '[HTML] (Html ())
        :<|> "login" :> ReqBody '[FormUrlEncoded] LoginForm :> Post '[HTML] AuthResp

type AuthAPI = Auth '[SA.JWT, SA.Cookie] SessionID :> LoginAPI

data LoginForm = LoginForm
    { invite :: Maybe InviteID
    , username :: UserName
    }
    deriving (Generic, FromJSON, ToJSON)

instance FromForm LoginForm

websocketHtml :: SessionID -> Html ()
websocketHtml sessionID = do
    let wsUrl = "/ws/htmx?session=" <> from sessionID
    with div_ [id_ "display-ws", class_ "h-full", makeAttribute "hx-ext" "ws", makeAttribute "ws-connect" wsUrl] do
        with div_ [id_ "display-root", class_ "h-full"] mempty
        script_ $ "globalThis.wsUrl = n => 'wss://' + window.location.host + '/ws/' + n + '?session=" <> from sessionID <> "';"

-- with div_ [id_ "display-menu"] mempty

splashHtml :: Html () -> Html ()
splashHtml content = do
    with div_ [id_ "display-lock", class_ "h-screen w-screen absolute bg-gray-100 flex flex-col"] do
        with div_ [class_ "basis-1/6 flex bg-sky-600 border-sky-800 border-b-8"] mempty

        with div_ [class_ "grow flex bg-sky-200 flex-col justify-center"] do
            with div_ [class_ "flex flex-row justify-center"] do
                with div_ [class_ "p-3 rounded bg-sky-900"] do
                    content

        with div_ [class_ "basis-1/6 flex bg-sky-600 border-sky-800 border-t-8"] mempty

welcomeForm :: Maybe InviteID -> Html ()
welcomeForm inviteM = do
    with form_ [id_ "splash-form", hxPost_ "/login"] do
        with div_ [class_ "text-white font-semibold pb-2 flex flex-row justify-center"] do
            "Welcome to ButlerOS"
        with (input_ mempty) [name_ "username", type_ "text", placeholder_ "What is your name?"]
        case inviteM of
            Just (InviteID invite) -> with (input_ mempty) [name_ "invite", type_ "hidden", value_ (from $ show invite)]
            _ -> pure ()

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings{cookieIsSecure = NotSecure, cookieSameSite = SameSiteStrict, cookieXsrfSetting = Nothing}

authServer :: Sessions -> (Html () -> Html ()) -> JWTSettings -> ServerT AuthAPI ProcessIO
authServer sessions mkIndexHtml jwtSettings auth = loginServer
  where
    loginServer :: ServerT LoginAPI ProcessIO
    loginServer = indexRoute auth :<|> getSessionRoute

    indexRoute :: AuthResult SessionID -> Maybe InviteID -> ProcessIO (Html ())
    indexRoute ar inviteM = liftIO $ case ar of
        Authenticated sessionID -> do
            isSessionValid <- atomically (checkSession sessions sessionID)
            case isSessionValid of
                Just _ -> pure $ mkIndexHtml $ (websocketHtml sessionID)
                Nothing -> loginPage
        _OtherAuth -> loginPage
      where
        loginPage = do
            isValid <- atomically (validClient inviteM)
            pure $
                mkIndexHtml $
                    splashHtml $
                        if isValid
                            then welcomeForm inviteM
                            else div_ "access denied"

    swapSplash = with div_ [id_ "display-lock", hxSwapOob_ "outerHTML"]

    denyResp :: ProcessIO AuthResp
    denyResp = clearSession cookieSettings . swapSplash <$> indexRoute SAS.NoSuchUser Nothing

    validClient :: Maybe InviteID -> STM Bool
    validClient = \case
        Just invite -> checkInvite sessions invite
        Nothing -> isEmptySessions sessions

    welcomeResp :: SessionID -> ProcessIO AuthResp
    welcomeResp sessionID = do
        resp <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings sessionID
        pure $ case resp of
            Just r -> do
                r (swapSplash $ websocketHtml sessionID)
            Nothing -> error "oops?!"

    getSessionRoute :: LoginForm -> ProcessIO AuthResp
    getSessionRoute form = case isValidUserName (coerce form.username) of
        Just username -> do
            logInfo "Validating form" [("form" .= form)]
            sessionM <- createSession sessions form.username form.invite
            case sessionM of
                Just session -> welcomeResp session.sessionID
                Nothing -> denyResp
        Nothing -> denyResp

type ClientAPI = "ws" :> Capture "channel" ChannelName :> QueryParam "reconnect" Bool :> QueryParam "session" SessionID :> WebSocket

clientServer :: Sessions -> (ChannelName -> Session -> WS.Connection -> ProcessIO ()) -> ServerT ClientAPI ProcessIO
clientServer sessions onConnect = connectRoute
  where
    connectRoute :: ChannelName -> Maybe Bool -> Maybe SessionID -> WS.Connection -> ProcessIO ()
    connectRoute name (fromMaybe False -> reconnect) sessionIDM connection
        | reconnect = doReload
        | otherwise = do
            isSessionValid <- atomically do
                case sessionIDM of
                    Just sessionID -> checkSession sessions sessionID
                    Nothing -> pure Nothing
            case isSessionValid of
                Nothing -> doReload
                Just session -> onConnect name session connection
      where
        doReload = liftIO $ WS.sendTextData connection $ renderText do
            with span_ [id_ "display-status"] do
                "<reconnecting...>"
                script_ "window.location.reload()"

newtype ChannelName = ChannelName Text
    deriving newtype (Eq, Show, Ord, FromHttpApiData, ToJSON, IsString)

type ChannelAPI = "channel" :> Capture "name" ChannelName :> ClientAPI

type WebSocketAPI = RemoteHost :> (AuthAPI :<|> ClientAPI)

data WebSocketServer = WebSocketServer
    { mkIndexHtml :: Html () -> Html ()
    , jwtSettings :: JWTSettings
    , onConnect :: SockAddr -> ChannelName -> Session -> WS.Connection -> ProcessIO ()
    }

websocketServer :: Sessions -> WebSocketServer -> ServerT WebSocketAPI ProcessIO
websocketServer sessions wss = displayRoute
  where
    displayRoute :: ServerT WebSocketAPI ProcessIO
    displayRoute clientAddr =
        authServer sessions wss.mkIndexHtml wss.jwtSettings
            :<|> clientServer sessions (wss.onConnect clientAddr)
