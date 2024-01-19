module ENCOINS.App.Widgets.IPFS where

import           Backend.Protocol.Types
import           Backend.Servant.Requests (cacheRequest)
import           Backend.Utility          (eventEither)
import           ENCOINS.Common.Events
import           ENCOINS.Common.Utils     (toText)

import qualified Crypto.Hash              as Hash
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import           Data.Text.Encoding       (encodeUtf8)
import           Reflex.Dom

updateCacheStatus :: Map Text CloudResponse -> [TokenCacheV3] -> [TokenCacheV3]
updateCacheStatus clouds = map updateCloud
  where
    updateCloud :: TokenCacheV3 -> TokenCacheV3
    updateCloud t =
      let name = tcAssetName t
          mStatus = Map.lookup name clouds
      in case mStatus of
        Nothing -> t
        Just (MkCloudResponse mIpfs mCoin) ->
           t{ tcIpfsStatus = fromMaybe (tcIpfsStatus t) mIpfs
            , tcCoinStatus = fromMaybe (tcCoinStatus t) mCoin
            }

mkCloudRequest :: TokenCacheV3 -> Maybe CloudRequest
mkCloudRequest cache = case (tcIpfsStatus cache, tcCoinStatus cache)  of
  (Unpinned, Minted) -> Just $ MkCloudRequest
    { reqAssetName  = tcAssetName cache
    , reqSecretKey  = "encrypted secret"
    }
  _ -> Nothing

pinValidTokensInIpfs :: MonadWidget t m
  => Dynamic t Text
  -> Dynamic t [TokenCacheV3]
  -> m (Dynamic t [TokenCacheV3])
pinValidTokensInIpfs dWalletAddress dTokenCache = do
  let dValidTokens = mapMaybe mkCloudRequest <$> dTokenCache
  -- logDyn "dValidTokens" dValidTokens
  let eFireCaching = () <$ (ffilter (not . null) $ updated dValidTokens)
  -- logEvent "eFireCaching" eFireCaching
  let dClientId = mkClientId <$> dWalletAddress
  let dReq = zipDynWith (,) dClientId dValidTokens
  -- logDyn "dClientId" dClientId
  eeCloudResponse <- cacheRequest dReq eFireCaching
  -- logEvent "walletTab: cache response:" eeCloudResponse
  let (eCacheError, eCloudResponse) = eventEither eeCloudResponse
  dCloudResponse <- holdDyn Map.empty eCloudResponse
  pure $ ffor2 dCloudResponse dTokenCache updateCacheStatus

mkClientId :: Text -> Text
mkClientId address = toText $ Hash.hash @Hash.SHA512 $ encodeUtf8 address

-- TODO: remove after debug
tokenSample :: CloudRequest
tokenSample = MkCloudRequest
  { reqAssetName = "b47f55bdc1d7615409cf8cc714e3885c42d6cb48629d44ff5a9265c88aa30cdc"
  -- , reqAssetName = "495090d7e6f2911cf0e1bc59ce244983ac5f1fe4adbaec9ce6af3429ad7aec79"
  , reqSecretKey = "super secret key"
  }
