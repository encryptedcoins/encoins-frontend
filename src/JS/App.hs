{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.App where

import           Control.Monad.IO.Class      (MonadIO(..))
import           Data.Text                   (Text)

#ifdef __GHCJS__
import           Language.Javascript.JSaddle (ToJSVal(..), JSVal)
#endif

import           JS.Types

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
  "walletLoad($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11);" walletLoad_js :: 
    JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()

walletLoad :: MonadIO m => Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> m ()
walletLoad walletName networkIdElement balanceElement changeAddressElement changeAddressBech32Element pubKeyHashElement
    stakeKeyHashElement collateralElement utxosElement unusedAddressesElement rewardAddressesElement = liftIO $ do
  walletName_js  <- toJSVal walletName
  networkIdElement_js  <- toJSVal networkIdElement
  balanceElement_js  <- toJSVal balanceElement
  changeAddressElement_js  <- toJSVal changeAddressElement
  changeAddressBech32Element_js  <- toJSVal changeAddressBech32Element
  pubKeyHashElement_js  <- toJSVal pubKeyHashElement
  stakeKeyHashElement_js  <- toJSVal stakeKeyHashElement
  collateralElement_js  <- toJSVal collateralElement
  utxosElement_js  <- toJSVal utxosElement
  unusedAddressesElement_js  <- toJSVal unusedAddressesElement
  rewardAddressesElement_js  <- toJSVal rewardAddressesElement
  walletLoad_js walletName_js networkIdElement_js balanceElement_js changeAddressElement_js changeAddressBech32Element_js
    pubKeyHashElement_js stakeKeyHashElement_js collateralElement_js utxosElement_js unusedAddressesElement_js rewardAddressesElement_js
#else
walletLoad :: MonadIO m => Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> m ()
walletLoad = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "encoinsTx($1, $2, $3, $4);"
  encoinsTxSubmit_js :: JSVal -> JSVal -> JSVal -> JSVal -> IO ()

encoinsTxSubmit :: MonadIO m => Text -> Text -> EncoinsRedeemerFrontend -> Text -> m ()
encoinsTxSubmit walletName tx red resId = liftIO $ do
  walletName_js <- toJSVal walletName
  tx_js         <- toJSVal tx
  redeemer_js   <- toJSVal $ EncoinsRedeemerJS red
  resId_js      <- toJSVal resId
  encoinsTxSubmit_js walletName_js tx_js redeemer_js resId_js
#else
encoinsTxSubmit :: MonadIO m => Text -> Text -> EncoinsRedeemerFrontend -> Text -> m ()
encoinsTxSubmit = const . const . const . const $ error "GHCJS is required!"
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