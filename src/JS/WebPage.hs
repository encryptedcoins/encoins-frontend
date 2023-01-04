{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.WebPage where

import           Control.Monad.IO.Class      (MonadIO(..))
import           Data.Text                   (Text)

#ifdef __GHCJS__
import           Language.Javascript.JSaddle (ToJSVal(..), JSVal)
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
  "saveTextFile($1);" saveTextFile_js :: JSVal -> IO ()

saveTextFile :: MonadIO m => Text -> m ()
saveTextFile txt = liftIO $ toJSVal txt >>= saveTextFile_js
#else
saveTextFile :: MonadIO m => Text -> m ()
saveTextFile = const $ error "GHCJS is required!"
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
