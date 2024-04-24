{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.Website where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)

#ifdef __GHCJS__
import Data.Maybe (isJust)
import Language.Javascript.JSaddle (JSString, JSVal, ToJSVal (..), textToStr)
#endif

-- This script is executed on page load
#ifdef __GHCJS__
foreign import javascript unsafe
  "runHeadScripts();" runHeadScripts_js :: IO ()

runHeadScripts :: MonadIO m => m ()
runHeadScripts = liftIO runHeadScripts_js
#else
runHeadScripts :: MonadIO m => m ()
runHeadScripts = error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "scrollIntoView($1);" scrollIntoView_js :: JSVal -> IO ()

scrollIntoView :: MonadIO m => Text -> m ()
scrollIntoView elemId = liftIO $ do
  elemId_js <- toJSVal elemId
  scrollIntoView_js elemId_js
#else
scrollIntoView :: MonadIO m => Text -> m ()
scrollIntoView = error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "setElementText($1, $2);" setElementText_js :: JSVal -> JSVal -> IO ()

setElementText :: MonadIO m => Text -> Text -> m ()
setElementText elId txt = liftIO $ do
  elId_js <- toJSVal elId
  txt_js  <- toJSVal txt
  setElementText_js elId_js txt_js
#else
setElementText :: MonadIO m => Text -> Text -> m ()
setElementText = error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "setInputValue($1, $2);" setInputValue_js :: JSVal -> JSVal -> IO ()

setInputValue :: MonadIO m => Text -> Text -> m ()
setInputValue elId txt = liftIO $ do
  elId_js <- toJSVal elId
  txt_js  <- toJSVal txt
  setInputValue_js elId_js txt_js
#else
setInputValue :: MonadIO m => Text -> Text -> m ()
setInputValue = error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "copyElemContent($1);" copyElemContent_js :: JSVal -> IO ()

copyElemContent :: MonadIO m => Text -> m ()
copyElemContent txt = liftIO $ toJSVal txt >>= copyElemContent_js
#else
copyElemContent :: MonadIO m => Text -> m ()
copyElemContent _ = liftIO $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "copyText($1);" copyText_js :: JSVal -> IO ()

copyText :: MonadIO m => Text -> m ()
copyText txt = liftIO $ toJSVal txt >>= copyText_js
#else
copyText :: MonadIO m => Text -> m ()
copyText _ = liftIO $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "saveTextFile($1, $2);" saveTextFile_js :: JSVal -> JSVal -> IO ()

saveTextFile :: MonadIO m => Text -> Text -> m ()
saveTextFile name txt = liftIO $ do
  name_js <- toJSVal name
  txt_js <- toJSVal txt
  saveTextFile_js name_js txt_js
#else
saveTextFile :: MonadIO m => Text -> Text -> m ()
saveTextFile _ _ = error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "console.log($1);" logInfoJS :: JSVal -> IO ()

logInfo :: MonadIO m => Text -> m ()
logInfo txt = liftIO $ toJSVal txt >>= logInfoJS
#else
logInfo :: MonadIO m => Text -> m ()
logInfo = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "saveJSON($1, $2, $3, $4);" saveJSON_js
    :: JSString -> JSString -> JSVal -> JSString -> IO ()

saveJSON :: MonadIO m => Maybe Text -> Text -> Text -> m ()
saveJSON mpass key val = liftIO $ do
  bool_js <- toJSVal $ isJust mpass
  saveJSON_js
    (textToStr key)
    (textToStr val)
    bool_js
    (maybe "" textToStr mpass)
#else
saveJSON :: MonadIO m => Maybe Text -> Text -> Text -> m ()
saveJSON _ _ _ = error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "loadJSON($1, $2, $3, $4);" loadJSON_js
    :: JSString -> JSString -> JSString -> JSVal -> IO ()

loadJSON :: MonadIO m => Text -> Text -> Maybe Text -> m ()
loadJSON key resId mpass = liftIO $ do
  bool_js <- toJSVal $ isJust mpass
  loadJSON_js (textToStr key) (textToStr resId) (maybe "" textToStr mpass) bool_js
#else
loadJSON :: MonadIO m => Text -> Text -> Maybe Text -> m ()
loadJSON _ _ _ = error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "removeKey($1);" removeKey_js :: JSString -> IO ()

removeKey :: MonadIO m => Text -> m ()
removeKey key = liftIO $ removeKey_js (textToStr key)
#else
removeKey :: MonadIO m => Text -> m ()
removeKey _ = error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "setElementStyle($1, $2, $3);" setElementStyle_js :: JSVal -> JSVal -> JSVal -> IO ()

setElementStyle :: MonadIO m => Text -> Text -> Text -> m ()
setElementStyle elId prop val = liftIO $ do
  elId_js <- toJSVal elId
  prop_js <- toJSVal prop
  val_js <- toJSVal val
  setElementStyle_js elId_js prop_js val_js
#else
setElementStyle :: MonadIO m => Text -> Text -> Text -> m ()
setElementStyle _ _ _ = error "GHCJS is required!"
#endif
