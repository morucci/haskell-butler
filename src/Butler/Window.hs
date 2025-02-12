module Butler.Window where

import Data.Text qualified as Text
import Data.IntMap.Strict qualified as IM
import Data.Char (isDigit)
import Lucid
import Data.Aeson (object)

import Butler.GUI
import Butler.NatMap qualified as NM
import Butler.Prelude
import Butler.Memory

newtype WinID = WinID Int
    deriving newtype (Eq, Ord, Show, ToJSON, Serialise)

data Window = Window
    { position :: (Int, Int)
    , size :: (Int, Int)
    , title :: Text
    }
  deriving (Eq, Generic, Serialise)

data WindowsState = WindowsState {
    windows :: IntMap Window
  , maxID :: WinID
  , focus :: Maybe WinID
  }
  deriving (Generic, Serialise)

type Windows = MemoryVar WindowsState

newWindows :: WindowsState
newWindows = WindowsState mempty (WinID 0) Nothing

lookupWindow :: Windows -> WinID -> STM (Maybe Window)
lookupWindow ws (WinID key) = IM.lookup key . (.windows) <$> readMemoryVar ws

deleteWindow :: Windows -> WinID -> STM ()
deleteWindow ws (WinID key) = modifyMemoryVar ws (#windows %~ IM.delete key)

updateWindow :: Windows -> WinID -> (Window -> Window) -> STM Bool
updateWindow ws (WinID key) f = stateMemoryVar ws $ \ws ->
  case IM.lookup key ws.windows of
    Nothing -> (False, ws)
    Just win ->
      let newWin = f win
       in case newWin == win of
        True -> (False, ws)
        False -> (True, ws & #windows %~ IM.insert key newWin)

newWindow :: Windows -> Text -> STM (WinID, Window)
newWindow ws title = stateMemoryVar ws $ \ws ->
    let WinID prev = ws.maxID
        key = (prev + 1)
        wid = WinID key
        win =
          Window
            (0 + 23 * key, 0 + 23 * key)
            (640, 420)
            title
     in ((wid, win), ws & (#maxID .~ wid) . (#windows %~ IM.insert key win))


renderWindowsContent :: Windows -> HtmlT STM ()
renderWindowsContent ws = do
    wins <- IM.toAscList . (.windows) <$> lift (readMemoryVar ws)
    forM_ wins $ \(idx, _) ->
        with div_ [id_ $ "w-" <> showT idx] mempty

renderWindows :: Windows -> HtmlT STM ()
renderWindows ws = do
    w <- lift (readMemoryVar ws)
    let createWindows = map (renderWindow . first WinID) (IM.toAscList w.windows)
    let script = windowScript : createWindows
    with (script_ $ Text.intercalate ";" script) [type_ "module"]

windowScript :: Text
windowScript =
    [raw|
import WinBox from '/xstatic/winbox.js'
globalThis.WinBox = WinBox
globalThis.onWinEvent = (ev, w) => debounceData(500, (x, y) => {
  if (ev == "resize" && onWindowResize[w] !== undefined) {
    onWindowResize[w]()
  }
  return encodeDataMessage(1, {ev: ev, w: w, x: x, y: y})
})
globalThis.onWinClose = (w) => (force) => {
  let doDelete = force
  if (!force && confirm("Close window?")) {
    butlerDataSocket.send(encodeDataMessage(1, {ev: "close", w: w}))
    doDelete = true
  }
  if (doDelete) {
    let div = document.getElementById("w-" + w);
    if (div) { div.remove(); }
    return false;
  }
  return true
}
butlerDataHandlers[1] = buf => {
  let body = decodeJSON(buf)
  let win = windows[body.w]
  let withoutHandler = (name, cb) => {
    // disable the handler to avoid bouncing loop effect
    let handler = win[name]
    win[name] = undefined
    cb()
    win[name] = handler
  }
  switch (body.ev) {
    case "move":
      withoutHandler("onmove", () => win.move(body.x, body.y))
      break
    case "resize":
      withoutHandler("onresize", () => {
        if (onWindowResize[body.w] !== undefined) {
          onWindowResize[body.w](body.x, body.y)
        }
        win.resize(body.x, body.y)
      })
      break
    case "focus":
      withoutHandler("onfocus", () => win.focus(true))
      break
    case "close":
      win.close(true)
      break
    case "title":
      win.setTitle(body.title)
      break
  }
}
|]

renderWindow :: (WinID, Window) -> Text
renderWindow (WinID idx, Window (x, y) (w, h) title) = do
    let attr k v = k <> ": " <> showT v
        handler n = "on" <> n <> ": onWinEvent(" <> showT n <> ", " <> showT idx <> ")"
        attrs =
            map handler ["resize", "move", "focus"] -- , minimize", "maximize"]
                <> [ attr "x" x
                   , attr "y" y
                   , attr "width" w
                   , attr "height" h
                   , "id: " <> showT idx
                   , "bottom: 36"
                   , "class: [\"no-full\"]"
                   , "root: document.getElementById(\"win-root\")"
                   , "onclose: onWinClose(" <> showT idx <> ")"
                   , "mount: document.getElementById(\"w-" <> showT idx <> "\")"
                   ]
        obj = "{" <> Text.intercalate ",\n" attrs <> "}"
        rs = "if (onWindowResize[" <> showT idx <> "]) {onWindowResize[" <> showT idx <> "]" <> showT (w, h) <> "}; "
     in "windows[" <> showT idx <> "] = (new WinBox(" <> from (show title) <> ", " <> obj <> ")); " <> rs

withWID :: WinID -> Text -> Text
withWID winID n = n <> "-" <> showT winID

withoutWID :: TriggerName -> TriggerName
withoutWID (TriggerName n) = TriggerName $ Text.dropWhileEnd (== '-') . Text.dropWhileEnd isDigit $ n

wid_ :: WinID -> Text -> _
wid_ wid n = id_ (withWID wid n)

scopeTriggers :: WinID -> [TriggerName] -> [TriggerName]
scopeTriggers winID = map (\(TriggerName tn) -> TriggerName (withWID winID tn))
