{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.App where

import           Control.Monad.IO.Class      (MonadIO(..))
import           Data.Text                   (Text)

#ifdef __GHCJS__
import           Data.Text                   (pack)
import           Language.Javascript.JSaddle (ToJSVal(..), JSVal)
import           JS.Types
#endif

import           ENCOINS.Core.Types

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "walletEnable($1, $2);" enable_js :: JSVal -> JSVal -> IO ()

enable :: MonadIO m => Text -> Text -> m ()
enable walletName resId = liftIO $ do
  walletName_js <- toJSVal walletName
  resId_js      <- toJSVal resId
  enable_js walletName_js resId_js
#else
enable :: MonadIO m => Text -> Text -> m ()
enable = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "walletAddress($1, $2);" walletAddress_js :: JSVal -> JSVal -> IO ()

walletAddress :: MonadIO m => Text -> Text -> m ()
walletAddress walletName resId = liftIO $ do
  walletName_js <- toJSVal walletName
  resId_js      <- toJSVal resId
  walletAddress_js walletName_js resId_js
#else
walletAddress :: MonadIO m => Text -> Text -> m ()
walletAddress = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "walletAddressBech32($1, $2);" walletAddressBech32_js :: JSVal -> JSVal -> IO ()

walletAddressBech32 :: MonadIO m => Text -> Text -> m ()
walletAddressBech32 walletName resId = liftIO $ do
  walletName_js <- toJSVal walletName
  resId_js      <- toJSVal resId
  walletAddressBech32_js walletName_js resId_js
#else
walletAddressBech32 :: MonadIO m => Text -> Text -> m ()
walletAddressBech32 = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "walletAddressBech32ToBytes($1, $2);" walletAddressBech32ToBytes_js :: JSVal -> JSVal -> IO ()

walletAddressBech32ToBytes :: MonadIO m => Text -> Text -> m ()
walletAddressBech32ToBytes addrBech32 resId = liftIO $ do
  addrBech32_js <- toJSVal addrBech32
  resId_js      <- toJSVal resId
  walletAddressBech32ToBytes_js addrBech32_js resId_js
#else
walletAddressBech32ToBytes :: MonadIO m => Text -> Text -> m ()
walletAddressBech32ToBytes = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "encoinsTx($1, $2, $3);"
  encoinsTx_js :: JSVal -> JSVal -> JSVal -> IO ()

encoinsTx :: MonadIO m => Text -> [GroupElement] -> Text -> m ()
encoinsTx walletName keys resId = liftIO $ do
  walletName_js <- toJSVal walletName
  redeemer_js       <- toJSVal $ EncoinsRedeemer $ (, "1000000" :: Text) $ map ((, "1" :: Text) . pack . show) keys
  resId_js      <- toJSVal resId
  encoinsTx_js walletName_js redeemer_js resId_js
#else
encoinsTx :: MonadIO m => Text -> [GroupElement] -> Text -> m ()
encoinsTx = const . const . const $ error "GHCJS is required!"
#endif