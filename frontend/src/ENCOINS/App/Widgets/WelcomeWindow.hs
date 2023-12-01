{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module ENCOINS.App.Widgets.WelcomeWindow where

import           Control.Monad                   (when)
import qualified Crypto.Hash                     as Hash
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import           Reflex.Dom
import           Text.RawString.QQ               (r)

import           ENCOINS.App.Widgets.Basic       (loadJsonFromStorage,
                                                  saveJsonToStorage)
import           ENCOINS.Common.Utils            (toText)
import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (btn, lnkInlineInverted)
import           JS.Website                      (setElementStyle)

data WelcomeItem = WelcomeItem
  { elemId  :: Text
  , border  :: Bool
  , title   :: Text
  , message :: [(Text,Text,Text)] -- Text and ref with title for link
  }

welcomeWallet :: [WelcomeItem]
welcomeWallet =
  [ WelcomeItem "welcome-tabs" False "Tabs" [(wTabs, "", "")]
  , WelcomeItem "welcome-wallet-coins" True "Coins in the Wallet" [( wCoinsInWallet, "", "")]
  , WelcomeItem "welcome-coins-mint" True "Coins to mint" [( wCoinsToMint, "", "")]
  , WelcomeItem "welcome-tx-balance" False "Transaction balance" [( wTxBalance, "", "")]
  , WelcomeItem "welcome-send-req" True "Send button" [(wSend, "", "")]
  , WelcomeItem "welcome-import-export" False "Import/Export buttons" [( wImportExport, "", "")]
  , WelcomeItem "welcome-read-docs" False "Useful links"
    [ ("Full user documentation is always available at ", "https://docs.encoins.io", "docs.encoins.io")
    , (". Visit our Discord community at ", "https://discord.gg/Q3gPP87Tcw", "discord.gg/Q3gPP87Tcw")
    , (". Stay up to date with the project's news on Twitter at ", "https://twitter.com/ENCOINS1", "twitter.com/ENCOINS1")
    , (".","","")
    ]
  , WelcomeItem "welcome-disclaimer" False "Disclaimer" [(wDisclaimerText, "", "")]
  ]

welcomeTransfer :: [WelcomeItem]
welcomeTransfer =
  [ WelcomeItem "welcome-coins-transfer" True "Coins in the Wallet" [(tCoinsInWallet,"","")]
  , WelcomeItem "welcome-transfer-btns" True "Send buttons" [(tSend, "", "")]
  ]

welcomeLedger :: [WelcomeItem]
welcomeLedger =
  [ WelcomeItem "welcome-ledger" False "Ledger mode" [(ledger, "", "")]
  , WelcomeItem "welcome-ledger-coins" True "Coins in the Ledger" [(lCoinsInLedger, "", "")]
  , WelcomeItem "welcome-ledger-mint" True "Coins to mint" [(lCoinsToMint, "", "")]
  ]

welcomeTutorial :: MonadWidget t m => [WelcomeItem] -> Event t () -> m (Event t ())
welcomeTutorial [] eOpen = pure eOpen
welcomeTutorial (wi:wis) eOpen = do
  eWiNext <- welcomeItemWidget wi eOpen
  eOpenNext <- delay 0.3 eWiNext
  welcomeTutorial wis eOpenNext

welcomeItemWidget :: MonadWidget t m => WelcomeItem -> Event t () -> m (Event t ())
welcomeItemWidget WelcomeItem{..} eOpen = mdo
  performEvent_ (setElementStyle elemId "z-index" "2000" <$ eOpen)
  performEvent_ (setElementStyle elemId "position" "relative" <$ eOpen)
  performEvent_ (setElementStyle elemId "pointer-events" "none" <$ eOpen)
  when border $ performEvent_ (setElementStyle elemId "border-style" "solid" <$ eOpen)
  eClose <- dialogWindow False eOpen eClose "overflow: auto; max-width: 700px; max-height: 500px; padding-left: 30px; padding-right: 30px; padding-top: 30px; padding-bottom: 30px; top: 10px; position: absolute;" title $ do
    divClass "app-WelcomeBody" $ do
      mapM_ (\(t, lRef, lTitle) -> do text t >> if T.empty == T.strip lRef then blank else lnkInlineInverted lRef lTitle) message
    divClass "app-WelcomeBody_ButtonOkContainer" $
      btn "button-switching inverted flex-center" "" $ text "Ok"
  performEvent_ (setElementStyle elemId "z-index" "" <$ eClose)
  performEvent_ (setElementStyle elemId "position" "" <$ eClose)
  performEvent_ (setElementStyle elemId "pointer-events" "" <$ eClose)
  when border $ performEvent_ (setElementStyle elemId "border-style" "none" <$ eClose)
  return eClose

welcomeWindowWalletStorageKey :: Text
welcomeWindowWalletStorageKey = "encoins-welcome-window-seen-wallet"

welcomeWindowTransferStorageKey :: Text
welcomeWindowTransferStorageKey = "encoins-welcome-window-seen-transfer"

welcomeWindowLedgerStorageKey :: Text
welcomeWindowLedgerStorageKey = "encoins-welcome-window-seen-ledger"

welcomeWindow :: MonadWidget t m => Text -> [WelcomeItem] -> m ()
welcomeWindow key items = do
    let currentHash = toText $ toHash items
    isSeen <- loadJsonFromStorage key
    if isSeen == Just currentHash
        then blank
        else do
            ePb <- getPostBuild
            eWelcome <- delay 0.5 ePb
            eWelcomeClose <- welcomeTutorial items eWelcome
            performEvent_ (saveJsonToStorage key currentHash <$
              eWelcomeClose)

toHash :: [WelcomeItem] -> Hash.MD5
toHash = Hash.hash . foldMap (\(WelcomeItem _ _ t ms) -> TE.encodeUtf8 $ t <> flatMessage ms)
  where
    flatMessage = foldMap (\(b,ref,h) -> b <> ref <> h)


-- Wallet welcome messages

wTabs :: Text
wTabs = [r|Welcome to ENCOINS! You can select the mode of operation from this menu.|]

wCoinsInWallet :: Text
wCoinsInWallet = [r|In the left column, you see the coins from your wallet.
     Clicking on a particular coin reveals its full name and asset fingerprint.
     To select coins to burn in the current transaction, check the boxes on the left.
     To copy the minting key, click on the key icon.|]

wCoinsToMint :: Text
wCoinsToMint = [r|Here, you can add new coins to mint by entering their ADA value and clicking the "+" button (or by pressing "Enter" on the keyboard). You can remove a coin from the current transaction by clicking the "x" button.|]

wTxBalance :: Text
wTxBalance = [r|Here, you can see the net ADA balance of the current transaction and protocol fees. A positive balance means you are withdrawing ADA from the protocol.|]

wSend :: Text
wSend = [r|Once you finished building the transaction, you can press "SEND REQUEST" button to execute it.|]

wImportExport :: Text
wImportExport = [r|All known coins are stored locally on your device. Here, you can import the coins that are new to this device. You can also export your coins for backup or use on another device. We recommend exporting coins after each session as they are currently stored in the browser cache.|]

wDisclaimerText :: Text
wDisclaimerText = [r|Encrypted Coins (aka Encoins) are Cardano native tokens with encrypting redeeming values. The Protocol is comprised of free, public and open-source software including a set of immutable and autonomous smart contracts deployed on the Cardano blockchain.
Your use of the Encoins Protocol involves various significant risks, including, but not limited to, financial loss while digital assets are being managed in the Protocol. Before using the Encoins Protocol, you should closely review the relevant documentation to make sure that you understand how the Protocol works and the risks of your use of the Protocol. The Protocol may be accessed through many web or mobile computer service interfaces; you are responsible for doing your own diligence regarding such interfaces to understand the fees and risks that they present, as for example the custody of your minting keys, fumbling exports, etc.
Although Encoins team developed much of the initial software code for the Encoins Protocol, it does not provide, own, or control the Encoins Protocol, which is run independently by smart contracts deployed on the Cardano blockchain. The Protocol does not constitute an account by which Encoins team or any other third parties act as financial intermediaries or custodians.  While the software code has undergone beta testing and continues to be improved by feedback from the developer community, open-source contributors and beta-testers, we cannot guarantee that there will be no bugs in the Protocol. Upgrades and modifications to the Protocol are or will be managed in a community-driven way through decentralized governance.
As a condition of your use of the Encoins website or any third party website connecting to it (collectively the “Site”), you agree that you: (i) are at least 18 years of age; (ii) are not barred from using the Protocol, the Site, or any connected services under any law applicable to you; (iii) will not interfere with the intended operation of the Protocol or Site, including by hacking, submitting a virus, fraudulent information or tokens, or attempting to overload, “flood,” or “crash” the Protocol or Site; and (iv) you are, and your use of the Protocol is and will be, in compliance at all times with all laws, rules, regulations or orders applicable to you.
THE PROTOCOL, THE SITE AND ALL INFORMATION CONTAINED ON THE SITE, ARE MADE ACCESSIBLE OR AVAILABLE ON AN “AS IS” AND “AS AVAILABLE” BASIS. YOU EXPRESSLY AGREE THAT USE OF THE SITE OR THE PROTOCOL IS AT YOUR SOLE RISK. TO THE FULLEST EXTENT ALLOWED BY APPLICABLE LAW, NONE OF ENCOINS PROTOCOL, AFFILIATES, AND PARTNERS, OR ANY DEVELOPER, EMPLOYEE, AGENT OR LICENSOR ASSOCIATED WITH ANY OF THEM (COLLECTIVELY, THE “PARTIES”), WARRANT THAT USE OF THE SITE OR PROTOCOL WILL BE UNINTERRUPTED, FULLY SECURE, VIRUS- OR ERROR-FREE, NOR DO THEY MAKE ANY WARRANTY AS TO THE RESULTS THAT MAY BE OBTAINED FROM USE OF THE SITE OR THE PROTOCOL.  EACH OF THE PARTIES HEREBY DISCLAIMS ANY AND ALL REPRESENTATIONS, WARRANTIES AND CONDITIONS, WHETHER EXPRESS OR IMPLIED, AS TO THE PROTOCOL, THE SITE OR ANY INFORMATION CONTAINED ON THE SITE, INCLUDING, BUT NOT LIMITED TO, THOSE OF TITLE, NON-INFRINGEMENT, MERCHANTABILITY, SUITABILITY AND FITNESS FOR A PARTICULAR PURPOSE, AS WELL AS WARRANTIES IMPLIED FROM A COURSE OF PERFORMANCE OR COURSE OF DEALING.
IN NO EVENT SHALL ANY OF THE PARTIES BE LIABLE FOR ANY DAMAGES ARISING OUT OF OR RELATED TO: (I) YOUR USE OF OR INABILITY TO USE THE PROTOCOL, OR THE SITE, OR INFORMATION CONTAINED IN THE SITE, (II) YOUR INTERACTIONS WITH OTHER USERS, OR (III) THESE USE TERMS; INCLUDING BUT NOT LIMITED TO (A) DIRECT, INDIRECT, INCIDENTAL, SPECIAL, PUNITIVE, OR CONSEQUENTIAL DAMAGES OF ANY KIND, AND (B) LOSS OF REVENUES, PROFITS, GOODWILL, CRYPTOCURRENCIES, TOKENS OR ANYTHING ELSE OF VALUE.
|]

-- Transfer welcome messages

tCoinsInWallet :: Text
tCoinsInWallet = [r|You are now in the transfer mode! To select the coins to transfer, check the boxes on the left.|]

tSend :: Text
tSend = [r|You can send coins to another user or to the ENCOINS Ledger script for use in the Ledger Mode.|]

-- Ledger welcome messages

ledger :: Text
ledger = [r|You are now in the Ledger mode! The ENCOINS Ledger is the on-chain script that acts as your stealth account. Sending coins to another user is as simple as sharing the minting keys off-chain. To receive a coin, import the minting key and re-mint the coin to gain the full control of it.|]

lCoinsInLedger :: Text
lCoinsInLedger = [r|Here, you can see the known coins that are stored on the ENCOINS Ledger. To select the coins to burn in the current transaction, check the boxes on the left.|]

lCoinsToMint :: Text
lCoinsToMint = [r|Here, you can add new coins to mint as in the Wallet Mode. Use the "Add Change" button to auto-balance the transaction. This creates a pure transfer on the ENCOINS Ledger without withdrawal. You can choose any wallet address as the destination when withdrawing from the ENCOINS Ledger.|]
