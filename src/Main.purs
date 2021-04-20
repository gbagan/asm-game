module Main where
import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Model (initModel, update)
import View (view)
import Pha.App (app)

main :: Effect Unit
main = app 
        {   init: {state: initModel, action: Nothing}
        ,   update
        ,   view
        ,   subscriptions: []
        ,   selector: "#root"
        }