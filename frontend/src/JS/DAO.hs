{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.DAO where

import           Control.Monad.IO.Class      (MonadIO(..))
import           Data.Text                   (Text)

#ifdef __GHCJS__
import           Language.Javascript.JSaddle (ToJSVal(..), JSVal)
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "daoPollVoteTx($1, $2, $3);"
  daoPollVoteTx_js :: JSVal -> JSVal -> JSVal -> IO ()

daoPollVoteTx :: MonadIO m => Integer -> (Text, Text) -> m ()
daoPollVoteTx n (walletName, answer) = liftIO $ do
  n_js          <- toJSVal $ (fromIntegral n :: Int)
  walletName_js <- toJSVal walletName
  answer_js     <- toJSVal answer
  daoPollVoteTx_js n_js walletName_js answer_js
#else
daoPollVoteTx :: MonadIO m => Integer -> (Text, Text) -> m ()
daoPollVoteTx = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "daoDelegateTx($1);"
  daoDelegateTx_js :: JSVal -> IO ()

daoDelegateTx :: MonadIO m => (Text, Text) -> m ()
daoDelegateTx (walletName, url) = liftIO $ do
  walletName_js <- toJSVal walletName
  url_js     <- toJSVal url
  daoDelegateTx_js walletName_js url_js
#else
daoDelegateTx :: MonadIO m => (Text, Text) -> m ()
daoDelegateTx = const $ error "GHCJS is required!"
#endif