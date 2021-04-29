module Main where
import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Model (initModel, update, Msg(..))
import View (view)
import Pha.App (app)
import Pha.Subscriptions as Subs

main :: Effect Unit
main = app 
        {   init: {state: initModel, action: Nothing}
        ,   update
        ,   view
        ,   subscriptions: [Subs.onHashChange $ const (Just HashChanged)]
        ,   selector: "#root"
        }