{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.DAO where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)

#ifdef __GHCJS__
import Language.Javascript.JSaddle (JSVal, ToJSVal (..))
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "daoPollVoteTx($1, $2, $3, $4, $5, $6, $7);"
  daoPollVoteTx_js :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()

daoPollVoteTx :: MonadIO m => Int -> Text -> Text -> Text -> Text -> (Text, Text) -> m ()
daoPollVoteTx n apiKey net policyId assetName (walletName, answer) = liftIO $ do
  n_js          <- toJSVal $ (fromIntegral n :: Int)
  apiKey_js     <- toJSVal apiKey
  net_js        <- toJSVal net
  walletName_js <- toJSVal walletName
  answer_js     <- toJSVal answer
  policyId_js   <- toJSVal policyId
  assetName_js  <- toJSVal assetName
  daoPollVoteTx_js n_js apiKey_js net_js walletName_js answer_js policyId_js assetName_js
#else
daoPollVoteTx :: MonadIO m => Int -> Text -> Text -> Text -> Text -> (Text, Text) -> m ()
daoPollVoteTx = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "daoDelegateTx($1, $2, $3, $4, $5, $6);"
  daoDelegateTx_js :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()

daoDelegateTx :: MonadIO m => Text -> Text -> Text -> Text -> (Text, Text) -> m ()
daoDelegateTx apiKey net policyId assetName (walletName, url) = liftIO $ do
  apiKey_js     <- toJSVal apiKey
  net_js        <- toJSVal net
  walletName_js <- toJSVal walletName
  url_js        <- toJSVal url
  policyId_js   <- toJSVal policyId
  assetName_js  <- toJSVal assetName
  daoDelegateTx_js apiKey_js net_js walletName_js url_js policyId_js assetName_js
#else
daoDelegateTx :: MonadIO m => Text -> Text -> Text -> Text -> (Text, Text) -> m ()
daoDelegateTx = const $ error "GHCJS is required!"
#endif
