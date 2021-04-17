module View where
import Prelude
import Data.Array (mapWithIndex)
import Pha.Html (Html)
import Pha.Html as H
import Model (Model, Msg)
import Logic (Program, Instruction(..))

view :: Model -> Html Msg
view {program, state} =
    H.div []
    [   H.h1 [] [H.text "Asm Game"]
    ,   viewProgram program
    ,   viewState state
    ]

viewProgram :: Program -> Html Msg
viewProgram program =
    H.div [H.class_ "program"] $ program # mapWithIndex viewInstr

viewInstr :: Int -> Instruction -> Html Msg
viewInstr no instr =
    H.div [H.class_ "instruction"] 
    [   H.div [H.class_ "instruction-no"] [H.text $ show no]
    
    ,   H.div [H.class_ "instruction-data"]
        [   H.text $ case instr of
                Input -> "input"
                Output -> "output"
                _ -> "??"
        ]
    ]

viewState :: State -> Html Msg
viewState {currentValue, input, output, registers} =
    H.div [] []