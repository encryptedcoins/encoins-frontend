module Backend.Servant.Client where

import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import           Reflex.Dom             hiding (Value)
import           Servant.API
import           Servant.Reflex         (BaseUrl, ReqResult (..),
                                         client)

import           Backend.Protocol.Types
import           CSL                    (TransactionInputs)

type API =   "newTx"        :> ReqBody '[JSON] (InputOfEncoinsApi, TransactionInputs)
                            :> Post '[JSON] (Text, Text)
        :<|> "submitTx"     :> ReqBody '[JSON] SubmitTxReqBody
                            :> Post '[JSON] NoContent
        :<|> "ping"         :> Get '[JSON] NoContent
        :<|> "serverTx"     :> ReqBody '[JSON] (InputOfEncoinsApi, TransactionInputs)
                            :> Post '[JSON] NoContent
        :<|> "status"       :> ReqBody '[JSON] EncoinsStatusReqBody
                            :> Post '[JSON] EncoinsStatusResult
        :<|> "version"      :> Get '[JSON] ServerVersion

type RespEvent t a      = Event t (ReqResult () a)
type Res t m res        = Event t () -> m (RespEvent t res)
type DynReqBody t a     = Dynamic t (Either Text a)
type ReqRes t m req res = DynReqBody t req -> Res t m res

data ApiClient t m = ApiClient
  { newTxRequest        :: ReqRes t m (InputOfEncoinsApi, TransactionInputs) (Text, Text)
  , submitTxRequest     :: ReqRes t m SubmitTxReqBody NoContent
  , pingRequest         :: Res t m NoContent
  , serverTxRequest     :: ReqRes t m (InputOfEncoinsApi, TransactionInputs) NoContent
  , statusRequest       :: ReqRes t m EncoinsStatusReqBody EncoinsStatusResult
  , versionRequest      :: Res t m ServerVersion
  }

mkApiClient :: forall t m . MonadWidget t m => BaseUrl -> ApiClient t m
mkApiClient host = ApiClient{..}
  where
    ( newTxRequest :<|>
      submitTxRequest :<|>
      pingRequest :<|>
      serverTxRequest :<|>
      statusRequest :<|>
      versionRequest) = client (Proxy @API) (Proxy @m) (Proxy @()) (pure host)

---------------------------------------------- Utilities ----------------------------------------

makeResponse :: ReqResult tag a -> Maybe a
makeResponse (ResponseSuccess _ a _) = Just a
makeResponse _                       = Nothing

makeResponseEither :: ReqResult tag a -> Either Text a
makeResponseEither (ResponseSuccess _ a _) = Right a
makeResponseEither (ResponseFailure _ txt _) = Left $ "ResponseFailure: " <> txt
makeResponseEither (RequestFailure _ txt) = Left $ "RequestFailure: " <> txt
