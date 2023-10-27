module Backend.Servant.Client where

import           Data.Maybe             (isNothing)
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import           Reflex.Dom             hiding (Value)
import           Servant.API
import           Servant.Reflex         (BaseUrl, ReqResult (..),
                                         client)
import           Witherable             (catMaybes)

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

mkApiClient :: forall t m . MonadWidget t m => Dynamic t BaseUrl -> ApiClient t m
mkApiClient dHost = ApiClient{..}
  where
    ( newTxRequest :<|>
      submitTxRequest :<|>
      pingRequest :<|>
      serverTxRequest :<|>
      statusRequest :<|>
      versionRequest) = client (Proxy @API) (Proxy @m) (Proxy @()) dHost

---------------------------------------------- Utilities ----------------------------------------

makeResponse :: ReqResult tag a -> Maybe a
makeResponse (ResponseSuccess _ a _) = Just a
makeResponse _                       = Nothing

makeResponseDev :: ReqResult tag a -> (Maybe a, Maybe Text)
makeResponseDev (ResponseSuccess _ a _) = (Just a, Nothing)
makeResponseDev (ResponseFailure _ txt _) = (Nothing, Just $ "ResponseFailure: " <> txt)
makeResponseDev (RequestFailure _ txt) = (Nothing, Just $ "RequestFailure: " <> txt)

eventMaybe :: Reflex t => b -> Event t (Maybe a) -> (Event t a, Event t b)
eventMaybe errValue e = (catMaybes e, errValue <$ ffilter isNothing e)
