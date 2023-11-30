{-# LANGUAGE CPP           #-}
{-# LANGUAGE JavaScriptFFI #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.DAO where

import           Control.Monad.IO.Class      (MonadIO (..))
import           Data.Text                   (Text)

#ifdef __GHCJS__
import           Language.Javascript.JSaddle (JSVal, ToJSVal (..))
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "daoPollVoteTx($1, $2, $3, $4, $5, $6);"
  daoPollVoteTx_js :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()

daoPollVoteTx :: MonadIO m => Int -> Text -> Text -> Text -> (Text, Text) -> m ()
daoPollVoteTx n apiKey net asset (walletName, answer) = liftIO $ do
  n_js          <- toJSVal $ (fromIntegral n :: Int)
  apiKey_js     <- toJSVal apiKey
  net_js        <- toJSVal net
  walletName_js <- toJSVal walletName
  answer_js     <- toJSVal answer
  asset_js      <- toJSVal asset
  daoPollVoteTx_js n_js apiKey_js net_js walletName_js answer_js asset_js
#else
daoPollVoteTx :: MonadIO m => Int -> Text -> Text -> Text -> (Text, Text) -> m ()
daoPollVoteTx = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "daoDelegateTx($1, $2, $3, $4, $5);"
  daoDelegateTx_js :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()

daoDelegateTx :: MonadIO m => Text -> Text -> Text -> (Text, Text) -> m ()
daoDelegateTx apiKey net asset (walletName, url) = liftIO $ do
  apiKey_js     <- toJSVal apiKey
  net_js        <- toJSVal net
  walletName_js <- toJSVal walletName
  url_js        <- toJSVal url
  asset_js        <- toJSVal asset
  daoDelegateTx_js apiKey_js net_js walletName_js url_js asset_js
#else
daoDelegateTx :: MonadIO m => Text -> Text -> Text -> (Text, Text) -> m ()
daoDelegateTx = const $ error "GHCJS is required!"
#endif
