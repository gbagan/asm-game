module Main where
import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Helpers as Helpers
import Model (initModel, update, Msg(..))
import View (view)
import Pha.App (app)
import Pha.Subscriptions as Subs

main :: Effect Unit
main = do
    levelId <- Helpers.getHash
    app { init: {state: initModel levelId, action: Nothing}
        , update
        , view
        , subscriptions: [Subs.onHashChange $ const (Just HashChanged)]
        , selector: "#root"
        }