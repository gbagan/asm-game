module Main where
import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Logic (State, Instruction(..), Program, Register(..), InstrNo(..))
import Model (Model, Request(..), update)
import View (view)
import Pha.App (app)


myProgram :: Program
myProgram = [Input, CopyTo (Register 0), Input, Add (Register 0), Output, Jump (InstrNo 0)]

myState :: State
myState =
    {   instrNo: InstrNo 0
    ,   currentValue: Nothing
    ,   registers: [Nothing, Nothing, Nothing]
    ,   input: [4, 2, 5, 6]
    ,   output: []
    }

model :: Model
model =
    {   program: myProgram
    ,   state: myState
    ,   dragged: Nothing
    ,   hover: Nothing
    ,   pointer: Nothing
    ,   request: NoRequest
    }

main :: Effect Unit
main = app 
        {   init: {state: model, action: Nothing}
        ,   update
        ,   view
        ,   subscriptions: []
        ,   selector: "#root"
        }