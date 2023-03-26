-- | This module contains the logic for graphical app definition.
module Butler.App where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Lucid
import Network.WebSockets qualified as WS

import Butler.Core
import Butler.Core.Clock
import Butler.Core.Dynamic
import Butler.Core.File
import Butler.Core.Pipe
import Butler.Display.Client
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Display.WebSocket (ChannelName)
import Butler.Frame
import Butler.Prelude

data Display = Display
    { sessions :: Sessions
    , clients :: TVar (Map SessionID [DisplayClient])
    }

data DisplayEvent
    = UserConnected ChannelName DisplayClient
    | UserDisconnected ChannelName DisplayClient
    deriving (Generic, ToJSON)

instance Show DisplayEvent where
    show = \case
        UserConnected{} -> "UserConnected"
        UserDisconnected{} -> "UserDisconnected"

-- | Application tag.
newtype AppTag = AppTag Text
    deriving (Show, Generic)
    deriving newtype (Ord, Eq, Semigroup, Serialise, IsString, FromJSON, ToJSON, ToHtml)

data UserEvent = UserJoined DisplayClient | UserLeft DisplayClient
    deriving (Generic, ToJSON)

-- | The type of event an app receive
data AppEvent
    = -- | A display event (e.g. to mount the UI)
      AppDisplay UserEvent
    | -- | A trigger event (e.g. onclick)
      AppTrigger GuiEvent
    | -- | A data event (e.g. for raw data)
      AppData DataEvent
    | -- | A file event (e.g. a new file opened)
      AppFile Directory (Maybe File)
    | -- | A sync event (e.g. a query that needs a reply)
      AppSync SyncEvent
    deriving (Generic, ToJSON)

newtype SyncEvent = SyncEvent
    { reply :: TMVar Dynamic
    }

instance ToJSON SyncEvent where toJSON = const "SyncEvent"

appCall :: Typeable a => AppInstance -> ProcessIO (Maybe a)
appCall appInstance = do
    mvReply <- newEmptyTMVarIO
    writePipe appInstance.pipe (AppSync (SyncEvent mvReply))
    res <- atomically =<< waitTransaction 100 (takeTMVar mvReply)
    pure $ case res of
        WaitTimeout -> Nothing
        WaitCompleted dyn -> fromDynamic dyn

eventFromMessage :: DisplayClient -> WS.DataMessage -> Maybe (AppID, AppEvent)
eventFromMessage client = \case
    WS.Text lbs _ -> do
        htmxEvent <- decodeJSON @HtmxEvent lbs
        (wid, TriggerName -> trigger) <- decodeNaturalSuffix htmxEvent.trigger
        pure (AppID (unsafeFrom wid), AppTrigger (GuiEvent client trigger htmxEvent.body))
    WS.Binary lbs -> do
        let rawBuf = from lbs
        (wid, buf) <- decodeMessage rawBuf
        pure (AppID $ unsafeFrom wid, AppData (DataEvent client buf rawBuf))

-- | A graphical application definition.
data App = App
    { name :: ProgramName
    -- ^ The application name.
    , tags :: Set AppTag
    -- ^ Its categories.
    , title :: Text
    -- ^ Its title.
    , description :: Text
    -- ^ A description.
    , size :: Maybe (Int, Int)
    -- ^ An optional size.
    , xfiles :: [XStaticFile]
    -- ^ Required XStaticFile
    , acceptFiles :: Maybe ContentType
    , start :: AppContext -> ProcessIO ()
    -- ^ Start action.
    }

newtype Service = Service App

defaultApp :: ProgramName -> (AppContext -> ProcessIO ()) -> App
defaultApp name start =
    App
        { name
        , tags = mempty
        , title = mempty
        , description = mempty
        , size = Nothing
        , xfiles = []
        , acceptFiles = Nothing
        , start
        }

-- | The application context
data AppContext = AppContext
    { wid :: AppID
    -- ^ the instance identifier. The app should mount its UI with `with div_ [wid_ wid] "body"`, and the trigger must container the AppID suffix too.
    , pipe :: Pipe AppEvent
    -- ^ the channel to receive events.
    , shared :: AppSharedContext
    }

data AppSharedContext = AppSharedContext
    { display :: Display
    , processEnv :: ProcessEnv
    , appSet :: AppSet
    , clients :: DisplayClients
    -- ^ the list of all the connected clients. To send update, app should uses `sendsHtml clients ""`
    , dynamics :: Dynamics
    , apps :: Apps
    , extraHandlers :: TVar (Map ChannelName (DisplayEvent -> ProcessIO ()))
    }

newAppSharedContext :: Display -> ProcessEnv -> AppSet -> STM AppSharedContext
newAppSharedContext display processEnv appSet =
    AppSharedContext display processEnv appSet <$> newDisplayClients <*> newDynamics <*> newApps <*> newTVar mempty

newtype Apps = Apps (TVar (Map AppID AppInstance))

newApps :: STM Apps
newApps = Apps <$> newTVar mempty

unregisterApp :: Apps -> AppInstance -> STM ()
unregisterApp (Apps tv) appInstance = modifyTVar' tv (Map.delete appInstance.wid)

registerApp :: Apps -> AppInstance -> STM ()
registerApp (Apps tv) appInstance = modifyTVar' tv (Map.insert appInstance.wid appInstance)

getApps :: Apps -> STM (Map AppID AppInstance)
getApps (Apps tv) = readTVar tv

data AppInstance = AppInstance
    { app :: App
    , process :: Process
    , wid :: AppID
    , pipe :: Pipe AppEvent
    }
    deriving (Generic)

newtype AppSet = AppSet (Map ProgramName App)

appSetApps :: AppSet -> [App]
appSetApps (AppSet m) = Map.elems m

-- | A convenient helper to mount the UI when a new user connect.
sendHtmlOnConnect :: HtmlT STM () -> AppEvent -> ProcessIO ()
sendHtmlOnConnect htmlT = \case
    AppDisplay (UserJoined client) -> atomically $ sendHtml client htmlT
    _ -> pure ()

newAppSet :: [App] -> AppSet
newAppSet = AppSet . Map.fromList . map (\app -> (app.name, app))

launchApp :: AppSet -> ProgramName -> AppSharedContext -> AppID -> ProcessIO (Maybe AppInstance)
launchApp (AppSet apps) (ProgramName name) shared wid = case Map.lookup (ProgramName appName) apps of
    Just app -> Just <$> startApp "app-" app shared wid
    Nothing -> pure Nothing
  where
    appName = fromMaybe name $ Text.stripPrefix "app-" name

startApp :: Text -> App -> AppSharedContext -> AppID -> ProcessIO AppInstance
startApp prefix app shared wid = do
    -- Start app process
    pipe <- atomically newPipe
    let ctx = AppContext wid pipe shared
    process <- spawnProcess (from prefix <> app.name) do
        app.start ctx

    pure $ AppInstance{app, process, wid, pipe}

-- | Start the application that is in charge of starting the other apps.
startShellApp :: AppSet -> Text -> App -> Display -> ProcessIO (AppSharedContext, AppInstance)
startShellApp appSet prefix app display = do
    let wid = AppID 0
    pipe <- atomically newPipe
    mvShared <- newEmptyMVar
    process <- spawnProcess (from prefix <> app.name) do
        processEnv <- ask
        shared <- atomically (newAppSharedContext display processEnv appSet)
        putMVar mvShared shared
        app.start (AppContext wid pipe shared)
    shared <- takeMVar mvShared
    let appInstance = AppInstance{app, process, wid, pipe}
    atomically (registerApp shared.apps appInstance)
    pure (shared, appInstance)

startApps :: [App] -> Display -> ProcessIO AppSharedContext
startApps apps display = do
    processEnv <- ask
    shared <- atomically (newAppSharedContext display processEnv (newAppSet apps))
    traverse_ (go shared) (zip [0 ..] apps)
    pure shared
  where
    go shared (i, app) = do
        let wid = AppID i
        appInstance <- startApp "app-" app shared wid
        atomically (registerApp shared.apps appInstance)

tagIcon :: AppTag -> Maybe Text
tagIcon = \case
    "Communication" -> Just "ri-signal-tower-fill"
    "Development" -> Just "ri-terminal-box-line"
    "Game" -> Just "ri-gamepad-line"
    "Graphic" -> Just "ri-palette-line"
    "Sound" -> Just "ri-volume-up-line"
    "System" -> Just "ri-settings-3-line"
    "Utility" -> Just "ri-tools-line"
    _ -> Nothing

butlerCheckbox :: AppID -> Text -> [Pair] -> Bool -> Maybe Text -> [Attribute]
butlerCheckbox wid name attrs value mConfirm
    | value = checked_ : attributes
    | otherwise = attributes
  where
    baseAction = sendTriggerScript wid name attrs
    action = case mConfirm of
        Just txt -> "if (window.confirm(\"" <> txt <> "\")) {" <> baseAction <> ";}"
        Nothing -> baseAction
    attributes = [type_ "checkbox", onclick_ (action <> "; return false")]

sendTriggerScriptConfirm :: AppID -> Text -> [Pair] -> Maybe Text -> Text
sendTriggerScriptConfirm wid name attrs mConfirm = case mConfirm of
    Nothing -> script
    Just txt -> "if (window.confirm(\"" <> txt <> "\")) {" <> script <> ";}"
  where
    script = sendTriggerScript wid name attrs

sendTriggerScript :: AppID -> Text -> [Pair] -> Text
sendTriggerScript wid name attrs =
    "sendTrigger(" <> showT wid <> ", \"" <> name <> "\", " <> decodeUtf8 (from obj) <> ")"
  where
    obj = encodeJSON (object attrs)

startAppScript :: App -> [Pair] -> Text
startAppScript app args = sendTriggerScript (AppID 0) "start-app" (["name" .= app.name] <> args)

appSetHtml :: Monad m => AppID -> AppSet -> HtmlT m ()
appSetHtml wid (AppSet apps) = do
    ul_ do
        forM_ cats \cat -> do
            with li_ [class_ "mb-2"] do
                forM_ (tagIcon cat) \icon ->
                    with i_ [class_ $ icon <> " text-blue-600 mr-2 text-xl relative top-1"] mempty
                toHtml cat
                with ul_ [class_ "pl-2 border-solid rounded border-l-2 border-slate-500"] do
                    forM_ (appInCat cat) mkLauncher
  where
    appInCat cat = filter (\app -> Set.member cat app.tags) $ Map.elems apps
    cats = foldMap (.tags) (Map.elems apps)
    mkLauncher app = do
        with
            li_
            [ onclick_ (startAppScript app ["wid" .= wid])
            , class_ "cursor-pointer"
            ]
            do
                toHtml app.name
                span_ do
                    " ("
                    toHtml app.description
                    ")"
