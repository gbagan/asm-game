module Main where
import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Logic (State, Instruction(..), InstrNo(..), Program)
import Model (Model, update)
import View (view)
import Pha.App (app)


myProgram :: Program
myProgram = [Input, Output]

myState :: State
myState =
    {   instrNo: InstrNo 0
    ,   currentValue: Nothing
    ,   registers : []
    ,   input: []
    ,   output: []
    }

model :: Model
model =
    {   program: myProgram
    ,   state: myState
    }

main :: Effect Unit
main = app 
        {   init: {state: model, action: Nothing}
        ,   update
        ,   view
        ,   subscriptions: []
        ,   selector: "#root"
        }