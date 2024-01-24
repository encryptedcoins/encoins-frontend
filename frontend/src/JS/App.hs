{-# LANGUAGE CPP           #-}
{-# LANGUAGE JavaScriptFFI #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.App where

import           Control.Monad.IO.Class      (MonadIO (..))
import           Data.Text                   (Text)

#ifdef __GHCJS__
import           Control.Monad               (guard)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (FromJSVal (..), JSM, JSString,
                                              JSVal, ToJSVal (..), strToText,
                                              textToStr)
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
  "walletSignTx($1, $2);"
  walletSignTx_js :: JSVal -> JSVal -> IO ()

walletSignTx :: MonadIO m => (Text, Text) -> m ()
walletSignTx (walletName, tx) = liftIO $ do
  walletName_js <- toJSVal walletName
  tx_js         <- toJSVal tx
  walletSignTx_js walletName_js tx_js
#else
walletSignTx :: MonadIO m => (Text, Text) -> m ()
walletSignTx = const $ error "GHCJS is required!"
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

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "window.fingerprintFromAssetName($1, $2)"
  fingerprintFromAssetName_js :: JSVal -> JSVal -> JSM JSVal

fingerprintFromAssetName :: MonadIO m => Text -> Text -> m Text
fingerprintFromAssetName currencySymbol tokenName = liftIO $ do
  currencySymbol_js <- toJSVal currencySymbol
  tokenName_js      <- toJSVal tokenName
  res_js <- fingerprintFromAssetName_js currencySymbol_js tokenName_js
  str_js <- fromJSValUnchecked res_js :: IO String
  return $ T.pack str_js
#else
fingerprintFromAssetName :: MonadIO m => Text -> Text -> m Text
fingerprintFromAssetName = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "pingServer($1)"
  pingServer_js :: JSString -> JSM JSVal

pingServer :: MonadIO m => Text -> m Bool
pingServer baseUrl = liftIO $ pingServer_js (textToStr baseUrl)
  >>= fromJSValUnchecked
#else
pingServer :: MonadIO m => Text -> m Bool
pingServer = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "saveHashedTextToStorage($1, $2);"
  saveHashedTextToStorage_js :: JSString -> JSString -> JSM ()

saveHashedTextToStorage :: MonadIO m => Text -> Text -> m ()
saveHashedTextToStorage key val = liftIO $ saveHashedTextToStorage_js
  (textToStr key) (textToStr val)
#else
saveHashedTextToStorage :: MonadIO m => Text -> Text -> m ()
saveHashedTextToStorage _ _ = error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "loadHashedPassword($1)"
  loadHashedPassword_js :: JSString -> JSM JSString

loadHashedPassword :: MonadIO m => Text -> m (Maybe Text)
loadHashedPassword key = do
  res <- strToText <$> liftIO (loadHashedPassword_js $ textToStr key)
  return $ res <$ guard (not $ T.null res)
#else
loadHashedPassword :: MonadIO m => Text -> m (Maybe Text)
loadHashedPassword _ = error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "checkPassword($1, $2)"
  checkPassword_js :: JSString -> JSString -> JSM JSVal

checkPassword :: MonadIO m => Text -> Text -> m Bool
checkPassword hash raw = liftIO $ checkPassword_js (textToStr hash)
  (textToStr raw) >>= fromJSValUnchecked
#else
checkPassword :: MonadIO m => Text -> Text -> m Bool
checkPassword _ _ = error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "addrLoad($1)"
  addrLoad_js :: JSString -> JSM ()

addrLoad :: MonadIO m => Text -> m ()
addrLoad = liftIO . addrLoad_js . textToStr
#else
addrLoad :: MonadIO m => Text -> m ()
addrLoad = const $ error "GHCJS is required!"
#endif

----------------------------- for IPFS -------------------------------------
#ifdef __GHCJS__
foreign import javascript unsafe
  "generateAESKey($1)" generateAESKey_js :: JSString -> JSM ()

generateAESKey :: MonadIO m => Text -> m ()
generateAESKey = liftIO . generateAESKey_js . textToStr
#else
generateAESKey :: MonadIO m => Text -> m ()
generateAESKey = const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "encryptAES($1, $2, $3)"
  encryptAES_js :: JSString -> JSString -> JSString -> JSM ()

encryptAES :: MonadIO m => (Text, Text, Text) -> m ()
encryptAES (key, resId, txt) = liftIO $ encryptAES_js
  (textToStr key)
  (textToStr resId)
  (textToStr txt)
#else
encryptAES :: MonadIO m => (Text, Text, Text)  -> m ()
encryptAES _ = error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "decryptAES($1, $2, $3)"
  decryptAES_js :: JSString -> JSString -> JSString -> JSM ()

decryptAES :: MonadIO m => (Text, Text, Text) -> m ()
decryptAES (key, resId, encryptedTxt) = liftIO $ decryptAES_js
  (textToStr key)
  (textToStr resId)
  (textToStr encryptedTxt)
#else
decryptAES :: MonadIO m => (Text, Text, Text) -> m ()
decryptAES _ = error "GHCJS is required!"
#endif