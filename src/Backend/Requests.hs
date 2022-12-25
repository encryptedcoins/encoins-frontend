module Backend.Requests where

import           Data.Maybe                 (isNothing)
import           Data.Text                  (Text)
import           Reflex.Dom                 hiding (Value)
import           Servant.Reflex             (BaseUrl(..))
import           Witherable                 (catMaybes)

import           Backend.Client
import           JS.Types

eventMaybe :: Reflex t => b -> Event t (Maybe a) -> (Event t a, Event t b)
eventMaybe errValue e = (catMaybes e, errValue <$ ffilter isNothing e)

pabIP :: BaseUrl
pabIP = BasePath "http://localhost:9080"

newEncoinsTxRequest :: MonadWidget t m => Dynamic t EncoinsRedeemerFrontend -> m (Event t Text)
newEncoinsTxRequest dRed = do
  let ApiClient{..} = mkApiClient pabIP
  e' <- fmap makeResponse <$> relayRequest (fmap Right dRed) (() <$ updated dRed)
  return $ catMaybes e'