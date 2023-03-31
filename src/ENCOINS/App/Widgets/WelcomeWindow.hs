module ENCOINS.App.Widgets.WelcomeWindow (welcomeWindow) where

import           Reflex.Dom

import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (pClass, btn)

welcomeWindow :: MonadWidget t m => m ()
welcomeWindow = mdo
    dWelcomeIsOpen <- holdDyn True (False <$ eWelcomeClose)
    eWelcomeClose <- dialogWindow dWelcomeIsOpen "max-width: 700px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" $ mdo
            divClass "connect-title-div" $ elAttr "div" ("class" =: "app-text-semibold" <> "style" =: "font-size: 22px; margin-bottom: 20px") $
                text "Welcome to the ENCOINS v1.0 Public Test!"
            pClass "p-ispo inverted" $ text "This is a test version of ENCOINS v1.0 Dapp. Notes:"
            elAttr "ul" ("role" =: "list" <> "class" =: "list p-ispo inverted") $ do
                mapM_ (el "li" . text)
                    [
                        "You can mint and burn encoins through the DApp's UI.",
                        "Individual values of coins you mint and burn are private. Only the total transaction balance is public.",
                        "Transacting using encoins makes the amount of ADA you hold and send confidential.",
                        "To burn a coin, a user needs to know the minting key. You can see the minting keys by hovering over the \"key\" icons. These keys are stored on your device.",
                        "To access your encoins on a new device, enter known minting keys using the \"Import\" button.",
                        "You can send encoins to another user directly from your wallet. The minting key for the coin must be sent off-chain. Do not forget to copy it before sending the coin!",
                        "At the moment, the test is limited to the Wallet Mode. Only Nami and Eternl are supported in this test."
                    ]
            btn "button-switching inverted flex-center" "" $ text "Ok" 
    blank