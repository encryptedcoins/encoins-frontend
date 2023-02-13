{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.App where

import           Control.Monad.IO.Class      (MonadIO(..))
import           Data.Text                   (Text)

#ifdef __GHCJS__
import           Language.Javascript.JSaddle (ToJSVal(..), JSVal)
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "walletLoad($1);" walletLoad_js :: JSVal -> IO ()

walletLoad :: MonadIO m => Text -> m ()
walletLoad walletName = liftIO $ toJSVal walletName >>= walletLoad_js
#else
walletLoad :: MonadIO m => Text -> m ()
walletLoad = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "walletSignTx($1, $2, $3);"
  walletSignTx_js :: JSVal -> JSVal -> JSVal -> IO ()

walletSignTx :: MonadIO m => Text -> Text -> Text -> m ()
walletSignTx walletName tx resId = liftIO $ do
  walletName_js <- toJSVal walletName
  tx_js         <- toJSVal tx
  resId_js      <- toJSVal resId
  walletSignTx_js walletName_js tx_js resId_js
#else
walletSignTx :: MonadIO m => Text -> Text -> Text -> m ()
walletSignTx = const . const . const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "sha2_256($1, $2);"
  sha2_256_js :: JSVal -> JSVal -> IO ()

sha2_256 :: MonadIO m => Text -> Text -> m ()
sha2_256 bs resId = liftIO $ do
  bs_js    <- toJSVal bs
  resId_js <- toJSVal resId
  sha2_256_js bs_js resId_js
#else
sha2_256 :: MonadIO m => Text -> Text -> m ()
sha2_256 = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "ed25519Sign($1, $2, $3);"
  ed25519Sign_js :: JSVal -> JSVal -> JSVal -> IO ()

ed25519Sign :: MonadIO m => Text -> Text -> Text -> m ()
ed25519Sign prvKey msg resId = liftIO $ do
  prvKey_js <- toJSVal prvKey
  msg_js    <- toJSVal msg
  resId_js  <- toJSVal resId
  ed25519Sign_js prvKey_js msg_js resId_js
#else
ed25519Sign :: MonadIO m => Text -> Text -> Text -> m ()
ed25519Sign = const $ error "GHCJS is required!"
#endif