module ENCOINS.App.Widgets.IPFS where

import           Backend.Protocol.Types

import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)

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

-- TODO: remove after debug
tokenSample :: CloudRequest
tokenSample = MkCloudRequest
  { reqAssetName = "b47f55bdc1d7615409cf8cc714e3885c42d6cb48629d44ff5a9265c88aa30cdc"
  -- , reqAssetName = "495090d7e6f2911cf0e1bc59ce244983ac5f1fe4adbaec9ce6af3429ad7aec79"
  , reqSecretKey = "super secret key"
  }
