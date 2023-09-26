{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.DAO where

import           Control.Monad.IO.Class      (MonadIO(..))
import           Data.Text                   (Text)

#ifdef __GHCJS__
import           Language.Javascript.JSaddle (ToJSVal(..), JSVal, JSString, textToStr)
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "daoPollVoteTx($1, $2, $3, $4, $5);"
  daoPollVoteTx_js :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()

daoPollVoteTx :: MonadIO m => Integer -> (Text, Text) -> (Text, Text) -> m ()
daoPollVoteTx n (apiKey, net) (walletName, answer) = liftIO $ do
  n_js          <- toJSVal $ (fromIntegral n :: Int)
  apiKey_js     <- toJSVal apiKey
  net_js        <- toJSVal net
  walletName_js <- toJSVal walletName
  answer_js     <- toJSVal answer
  daoPollVoteTx_js n_js apiKey_js net_js walletName_js answer_js
#else
daoPollVoteTx :: MonadIO m => Integer -> (Text, Text) -> (Text, Text) -> m ()
daoPollVoteTx = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "daoDelegateTx($1, $2, $3, $4);"
  daoDelegateTx_js :: JSVal -> JSVal -> JSVal -> JSVal -> IO ()

daoDelegateTx :: MonadIO m => (Text, Text) -> (Text, Text) -> m ()
daoDelegateTx (apiKey, net) (walletName, url) = liftIO $ do
  apiKey_js     <- toJSVal apiKey
  net_js        <- toJSVal net
  walletName_js <- toJSVal walletName
  url_js        <- toJSVal url
  daoDelegateTx_js apiKey_js net_js walletName_js url_js
#else
daoDelegateTx :: MonadIO m => (Text, Text) -> (Text, Text) -> m ()
daoDelegateTx = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

-- #ifdef __GHCJS__
-- foreign import javascript unsafe
--   "checkUrl($1);"
--   checkUrl_js :: JSString -> JSM JSVal

-- checkUrl :: MonadIO m => Text -> m (Maybe Bool)
-- checkUrl url = liftIO $ do
--   res_js <- checkUrl_js (textToStr url)
--   fromJSVal res_js :: IO (Maybe Bool)
-- #else
-- checkUrl :: MonadIO m => Text -> m (Maybe Bool)
-- checkUrl _ = error "GHCJS is required!"
-- #endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "checkUrl($1);"
  checkUrl_js :: JSString -> IO ()

checkUrl :: MonadIO m => Text -> m ()
checkUrl url = liftIO $ checkUrl_js (textToStr url)
#else
checkUrl :: MonadIO m => Text -> m ()
checkUrl _ = error "GHCJS is required!"
#endif
