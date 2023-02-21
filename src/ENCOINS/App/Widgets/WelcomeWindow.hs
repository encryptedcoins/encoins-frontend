module ENCOINS.App.Widgets.WelcomeWindow (welcomeWindow) where

import           Reflex.Dom

import           ENCOINS.Common.Widgets.Advanced (dialogWindow)
import           ENCOINS.Common.Widgets.Basic    (pClass, btn)

welcomeWindow :: MonadWidget t m => m ()
welcomeWindow = mdo
    dWelcomeIsOpen <- holdDyn True (False <$ eWelcomeClose)
    eWelcomeClose <- dialogWindow dWelcomeIsOpen "max-width: 700px; padding-left: 70px; padding-right: 70px; padding-top: 30px; padding-bottom: 30px" $ mdo
            divClass "connect-title-div" $ divClass "app-text-semibold" $ text "Welcome to the ENCOINS v1.0 Private Test!"
            pClass "p-ispo inverted" $ text "This is a test version of ENCOINS v1.0 Dapp. Notes:"
            elAttr "ul" ("role" =: "list" <> "class" =: "list p-ispo inverted") $ do
                mapM_ (el "li" . text)
                    [
                        "You can mint and burn encoins through the DApp's UI.",
                        "Individual values of coins that you mint and/or burn are private. Only the total transaction balance is public.",
                        "Transacting with other wallets using encoins makes the amount of ADA you hold and send confidential.",
                        "To burn a coin a user needs to know the minting key. You can see the minting keys by hovering over the \"key\" icons. These keys are stored on your device.",
                        "To access encoins on another device, copy the minting key and use the \"Import\" button. Do not forget to copy the minting key before sending the coin to another user!",
                        "At the moment, the test is limited to the Wallet Mode."
                    ]
            btn "button-switching inverted flex-center" "" $ text "Ok" 
    blank