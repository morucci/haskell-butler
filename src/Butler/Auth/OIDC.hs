module Butler.Auth.OIDC (oIDCAuthApp) where

import Lucid

import Butler.Core
import Butler.Display
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Display.WebSocket
import Butler.Prelude

import Data.Time (UTCTime (..), fromGregorian)
import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.HTML.Lucid
import Data.Map.Strict qualified as HM
import Web.OIDC.Client qualified as O
import Network.HTTP.Client (Manager)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Char8 qualified as B
import qualified System.Random as Random
import System.Random (genByteString)
import Crypto.Hash.SHA256 (hash)
import Data.Aeson (encode)
import Network.HTTP.Client.TLS (newTlsManager)

type AuthResp = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ())
type LoginAPI = Get '[HTML] (Html ()) :<|> "_login" :> Get '[JSON] NoContent :<|> "_cb" :> Get '[HTML] AuthResp

type AuthAPI = Auth '[SA.JWT, SA.Cookie] SessionID :> (LoginAPI :<|> Capture "workspace" Workspace :> LoginAPI)

authServer :: OS -> Process -> Sessions -> (Html () -> Html ()) -> JWTSettings -> ServerT AuthAPI Servant.Handler
authServer os process sessions mkIndexHtml jwtSettings auth =
    let loginSrv = loginServer os process sessions mkIndexHtml jwtSettings auth
     in loginSrv Nothing :<|> (loginSrv . Just)

data OIDCProviderConfig = OIDCProviderConfig
  { opIssuerURL :: Text
  , opClientID :: Text
  , opClientSecret :: Text
  , opAppPublicURL :: Text
  , opUserClaim :: Maybe Text
  , opEnforceAuth :: Bool
  , opName :: Text
  }
data OIDCEnv = OIDCEnv
  { oidc :: O.OIDC
  , manager :: Manager
  , provider :: O.Provider
  , redirectUri :: ByteString
  , sessionStoreStorage :: MVar (HM.Map O.State O.Nonce)
  , providerConfig :: OIDCProviderConfig
  }

initOIDCEnv :: OIDCProviderConfig -> IO OIDCEnv
initOIDCEnv providerConfig@OIDCProviderConfig {..} = do
  manager <- newTlsManager
  provider <- O.discover opIssuerURL manager
  sessionStoreStorage <- newMVar HM.empty
  let redirectUri = encodeUtf8 opAppPublicURL <> "_cb"
      clientId = encodeUtf8 opClientID
      clientSecret = encodeUtf8 opClientSecret
      oidc = O.setCredentials clientId clientSecret redirectUri (O.newOIDC provider)
  pure OIDCEnv {..}

data OIDCState = OIDCState
  { randomT :: Text
  , uri :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON OIDCState

instance ToJSON OIDCState

-- | Generate a random fixed size string of 42 char base64 encoded
genRandomB64 :: IO ByteString
genRandomB64 = B64.encode . hash <$> genRandom

-- | Generate a random fixed size string of 1024 Bytes
genRandom :: IO ByteString
genRandom = do
  g <- Random.newStdGen
  let (bs, _ng) = genByteString 1024 g
  pure bs

mkSessionStore :: OIDCEnv -> Maybe O.State -> Maybe Text -> O.SessionStore IO
mkSessionStore OIDCEnv {sessionStoreStorage} stateM uriM = do
  let sessionStoreGenerate = do
        rb <- liftIO genRandomB64
        let s = OIDCState (decodeUtf8 rb) uriM
        pure (B64.encode $ BSL.toStrict $ Data.Aeson.encode s)
      sessionStoreSave = storeSave
      sessionStoreGet = storeGet
      sessionStoreDelete = case stateM of
        Just state' -> modifyMVar_ sessionStoreStorage $ \store -> pure $ HM.delete state' store
        Nothing -> pure ()
   in O.SessionStore {..}
 where
  storeSave :: O.State -> O.Nonce -> IO ()
  storeSave state' nonce = modifyMVar_ sessionStoreStorage $ \store -> pure $ HM.insert state' nonce store
  storeGet :: O.State -> IO (Maybe O.Nonce)
  storeGet state' = do
    store <- readMVar sessionStoreStorage
    let nonce = HM.lookup state' store
    pure nonce

loginServer :: OS -> Process -> Sessions -> (Html () -> Html ()) -> JWTSettings -> AuthResult SessionID -> Maybe Workspace -> ServerT LoginAPI Servant.Handler
loginServer os process sessions mkIndexHtml jwtSettings auth mWorkspace =
    indexRoute auth :<|> loginRoute :<|> callbackRoute
  where
    callbackRoute :: Servant.Handler AuthResp
    callbackRoute = do
        let provider = SessionProvider "google"
        session <- liftIO $ runProcessIO os process $ newSession sessions provider "guest"
        liftIO (SAS.acceptLogin cookieSettings jwtSettings session.sessionID) >>= \case
            Just r -> do
                let page = workspaceUrl mWorkspace
                pure $ r (script_ $ "window.location.href = " <> showT page)
            Nothing -> error "oops?!"

    indexRoute :: AuthResult SessionID -> Servant.Handler (Html ())
    indexRoute = \case
        Authenticated sessionID -> do
            mSession <- atomically (lookupSession sessions sessionID)
            case mSession of
                Just _ ->
                    pure $ mkIndexHtml (websocketHtml (workspaceUrl mWorkspace))
                Nothing -> do
                    liftIO $ runProcessIO os process $ logError "no more auth?" ["id" .= sessionID]
                    pure "Unknown session"
        _ -> throwError $ err303{errHeaders = [("Location", encodeUtf8 (workspaceUrl mWorkspace) <> "_login")]}

    loginRoute :: Servant.Handler NoContent
    loginRoute = do
        oidcenv <- liftIO . initOIDCEnv $
                OIDCProviderConfig
                    "https://accounts.google.com"
                    "my_client_id"
                    "my_client_secret"
                    "https://localhost:8080"
                    (Just "email")
                    False
                    "Test"
        redirectLocation <- liftIO (genOIDCURL oidcenv)
        throwError $ err303{errHeaders = [("Location", redirectLocation)]}
        where
            genOIDCURL :: OIDCEnv -> IO ByteString
            genOIDCURL oidcenv@OIDCEnv {oidc} = do
                loc <- O.prepareAuthenticationRequestUrl (mkSessionStore oidcenv Nothing Nothing) oidc [O.openId] mempty
                pure . B.pack $ show loc

cookieSettings :: CookieSettings
cookieSettings =
    defaultCookieSettings
        { cookieSameSite = SameSiteStrict
        , cookieXsrfSetting = Nothing
        , cookieExpires = Just (UTCTime (fromGregorian 2030 1 1) 0)
        }

oIDCAuthApp :: Sessions -> (Html () -> Html ()) -> ProcessIO AuthApplication
oIDCAuthApp sessions mkIndexHtml = do
    JwkStorage myKey <- fst <$> newProcessMemory "display-key.jwk" (JwkStorage <$> liftIO generateKey)
    let jwtSettings = defaultJWTSettings myKey
    let cfg = cookieSettings :. jwtSettings :. EmptyContext
    env <- ask
    let authSrv = authServer env.os env.process sessions mkIndexHtml jwtSettings
    let app = Servant.serveWithContextT (Proxy @AuthAPI) cfg id authSrv
    pure $ AuthApplication app
