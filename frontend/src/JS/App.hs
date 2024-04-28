{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.App where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)

#ifdef __GHCJS__
import qualified Data.Text as T
import Language.Javascript.JSaddle
    ( FromJSVal (..)
    , JSM
    , JSString
    , JSVal
    , ToJSVal (..)
    , strToText
    , textToStr
    )
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
  "loadCacheValue($1)"
  loadCacheValue_js :: JSString -> JSM JSString

loadCacheValue :: MonadIO m => Text -> m Text
loadCacheValue key =
  strToText <$> liftIO (loadCacheValue_js$ textToStr key)
#else
loadCacheValue :: MonadIO m => Text -> m Text
loadCacheValue _ = error "GHCJS is required!"
#endif

-----------------------------------------------------------------

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

----------------------------- for Save -------------------------------------
#ifdef __GHCJS__
foreign import javascript unsafe
  "CloudCrypto.generateCloudKey($1)" generateCloudKey_js :: JSString -> JSM ()

generateCloudKey :: MonadIO m => Text -> m ()
generateCloudKey = liftIO . generateCloudKey_js . textToStr
#else
generateCloudKey :: MonadIO m => Text -> m ()
generateCloudKey = const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "CloudCrypto.encryptSecret($1, $2, $3)"
  encryptSecret_js :: JSString -> JSString -> JSString -> JSM ()

encryptSecret :: MonadIO m => (Text, Text, Text) -> m ()
encryptSecret (key, resId, txt) = liftIO $ encryptSecret_js
  (textToStr key)
  (textToStr resId)
  (textToStr txt)
#else
encryptSecret :: MonadIO m => (Text, Text, Text)  -> m ()
encryptSecret _ = error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "CloudCrypto.decryptSecret($1, $2, $3)"
  decryptSecret_js :: JSString -> JSString -> JSString -> JSM ()

decryptSecret :: MonadIO m => (Text, Text, Text) -> m ()
decryptSecret (key, resId, encryptedTxt) = liftIO $ decryptSecret_js
  (textToStr key)
  (textToStr resId)
  (textToStr encryptedTxt)
#else
decryptSecret :: MonadIO m => (Text, Text, Text) -> m ()
decryptSecret _ = error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "CloudCrypto.decryptSecretList($1, $2, $3)"
  decryptListAES_js :: JSString -> JSString -> JSVal -> JSM ()

decryptSecretList :: MonadIO m => (Text, Text, [(Text,Text)]) -> m ()
decryptSecretList (key, resId, list) = liftIO $ do
  list_js <- toJSVal list
  decryptListAES_js (textToStr key) (textToStr resId) list_js
#else
decryptSecretList :: MonadIO m => (Text, Text, [(Text,Text)]) -> m ()
decryptSecretList _ = error "GHCJS is required!"
#endif
