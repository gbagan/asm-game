module View  (view) where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Logic (Instruction(..), Register(..), InstrNo(..))
import Model (Model, Msg(..), Message(..), Request(..), draggedInstr)
import Pha.Html (Html, Prop, EventHandler)
import Pha.Html as H
import Pha.Html.Events as E
import Pha.Html.Keyed as K
import Pha.Html.Util (pc)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event as Ev
import Web.HTML.HTMLElement as HE
import Web.UIEvent.MouseEvent as ME

view :: Model -> Html Msg
view model@{program,
            state: state@{instrNo: InstrNo instrNo, currentValue, input, output, registers},
            request,
            pointer,
            message
           } =
    H.div [H.class_ "asm-main", E.onMouseUp CancelDrag, E.on "mousemove" move]
    [   H.div []
        [   viewState
        ,   H.button [E.onClick Step, H.class_ "button"] [H.text "step"]
        ,   H.button [E.onClick RunProgram, H.class_ "button is-primary"] [H.text "run"]
        ,   H.button [E.onClick Reset, H.class_ "button is-danger"] [H.text "reset"]
        ,   H.div [H.class_ "asm-message"] [H.text $ case message of
                NoMessage -> ""
                ErrorMessage msg -> "Error: " <> msg
            ]
        ]
    ,   viewInstructions
    ,   viewProgram
    ,   H.fromMaybe $ viewPointer <$> pointer <*> draggedInstr model
    ]
    where
        move e = map (SetPointer <<< Just) <$> pointerDecoder e
        currentInstr = program !! instrNo
        areRegistersSelectable = case request of
            RegisterRequest _ -> true
            _ -> false

        viewState =
            H.div [H.class_ "box asm-state"]
            [   viewInput input
            ,   H.div [H.class_ "asm-state-center"]
                [   H.div [H.class_ "asm-value"] [H.text $ maybe "" (show <<< _.value) currentValue]
                ,   viewRegisters areRegistersSelectable registers
                ]
            ,   viewOutput output
            ]

        viewProgram =
            K.div [H.class_ "box asm-program", E.stopPropagationOn "mouseup" $ E.always $ Just DropOnProgram /\ true] $
                (Array.concat $ program # Array.mapWithIndex \i {id, instr} -> viewInstr' i id instr) <>
                [   "arrow" /\ H.div
                    [   H.class_ "asm-instruction-arrow"
                    ,   H.style "left" "0"
                    ,   H.style "top" $ show (instrNo * 2) <> "rem"
                    ]
                    [   H.text "➤"]
                ]

        viewInstr' no id instr =
            [   ("b" <> show id) /\ H.div
                [   H.class_ "asm-instruction-no"
                ,   H.style "left" "2rem"
                ,   H.style "top" $ show (no * 2) <> "rem"
                ]
                [   H.text $ show no
                ]
            ,   ("c" <> show id) /\ viewInstr
                [   H.class_ "asm-program-instruction"
                ,   H.style "left" "4rem"
                ,   H.style "top" $ show (no * 2) <> "rem"
                ,   E.stopPropagationOn "mouseup" $ E.always $ Just (DropOnInstr no) /\ true
                ,   E.onMouseDown (DragInstrNo no)
                ] (Just no) instr
            ]

viewPointer :: {x :: Number, y :: Number} -> Instruction -> Html Msg
viewPointer {x, y} instr =
    H.div [H.class_ "asm-ui-cursor", H.style "left" $ pc x, H.style "top" $ pc y]
        [viewInstr [] Nothing instr]

viewInstructions :: Html Msg
viewInstructions =
    H.div [H.class_ "box"] $ 
        [Input, Output, CopyFrom (Register 0), CopyTo (Register 0), Add (Register 0), Increment (Register 0), Decrement (Register 0), Jump (InstrNo 0)] <#> \instr -> 
            viewInstr [E.onMouseDown (DragInstr instr)] Nothing instr

instructionColor :: Instruction -> String
instructionColor Input = "lightgreen"
instructionColor Output = "lightgreen"
instructionColor (Jump _) = "orange"
instructionColor _ = "lightblue"

viewInstr :: Array (Prop Msg) -> Maybe Int -> Instruction -> Html Msg
viewInstr props instrNo instr =
    H.div ([H.class_ "asm-instruction-data", H.style "background-color" $ instructionColor instr] <> props)
        case instr of
            Input -> [H.text "input"]
            Output -> [H.text "output"]
            CopyFrom (Register r) -> 
                [   H.text "copyfrom"
                ,   H.div 
                    [   H.class_ "asm-instr-register"
                    ,   E.stopPropagationOn "mousedown" $ E.always (Nothing /\ true)
                    ,   E.onClick (AskRequest (maybe NoRequest RegisterRequest instrNo))
                    ]
                    [H.text $ show r]
                ]
            CopyTo (Register r) -> 
                [   H.text "copyto"
                ,   H.div
                [   H.class_ "asm-instr-register"
                    ,   E.stopPropagationOn "mousedown" $ E.always (Nothing /\ true)
                    ,   E.onClick (AskRequest (maybe NoRequest RegisterRequest instrNo))
                    ]
                    [H.text $ show r]
                ]
            Add (Register r) -> 
                [   H.text "add"
                ,   H.div 
                    [   H.class_ "asm-instr-register"
                    ,   E.stopPropagationOn "mousedown" $ E.always (Nothing /\ true)
                    ,   E.onClick (AskRequest (maybe NoRequest RegisterRequest instrNo))
                    ]
                    [H.text $ show r]
                ]
            Sub (Register r) -> 
                [   H.text "sub"
                ,   H.div
                    [   H.class_ "asm-instr-register"
                    ,   E.stopPropagationOn "mousedown" $ E.always (Nothing /\ true)
                    ,   E.onClick (AskRequest (maybe NoRequest RegisterRequest instrNo))
                    ]
                    [H.text $ show r]
                ]
            Increment (Register r) -> 
                [   H.text "bump+"
                ,   H.div 
                    [   H.class_ "asm-instr-register"
                    ,   E.stopPropagationOn "mousedown" $ E.always (Nothing /\ true)
                    ,   E.onClick (AskRequest (maybe NoRequest RegisterRequest instrNo))
                    ]
                    [H.text $ show r]
                ]
            Decrement (Register r) -> 
                [   H.text "bump-"
                ,   H.div 
                    [   H.class_ "asm-instr-register"
                    ,   E.stopPropagationOn "mousedown" $ E.always (Nothing /\ true)
                    ,   E.onClick (AskRequest (maybe NoRequest RegisterRequest instrNo))
                    ]
                    [H.text $ show r]
                ]
            Jump (InstrNo n) -> [H.text $ "jump " <> show n]
            _ -> [H.text "???"]


viewInput :: Array Int -> Html Msg
viewInput l =
    H.div [H.class_ "asm-input"] $ l <#> \v ->
        H.div [H.class_ "asm-value"] [H.text $ show v]

viewRegisters :: Boolean -> Array (Maybe Int) -> Html Msg
viewRegisters selectable l =
    H.div [H.class_ "asm-registers"] $ l # Array.mapWithIndex \i v ->
        H.div [H.class_ "asm-register", H.class' "asm-register-selectable" selectable, E.onClick (SelectRegister i)] [
            H.maybe v \val ->
                H.div [H.class_ "asm-value"] [H.text $ show val]        
        ]

viewOutput :: Array Int -> Html Msg
viewOutput l =
    H.div [H.class_ "asm-output"] $ l <#> \v ->
        H.div [H.class_ "asm-value"] [H.text $ show v]


pointerDecoder ∷ EventHandler { x ∷ Number, y ∷ Number }
pointerDecoder ev = do
    case ME.fromEvent ev /\ Ev.currentTarget ev of
        Just mouseEv /\ Just el → do
            -- dans l'implémentation actuelle en purescript, getBoundingClientRect ne s'applique
            -- qu'à des HTMLElement et pas à des SVG Elements
            let el' = unsafeCoerce el ∷ HE.HTMLElement
            {left, top, width, height} ← HE.getBoundingClientRect el'
            pure $ Just {
                x: (toNumber(ME.clientX mouseEv) - left) / width,
                y: (toNumber(ME.clientY mouseEv) - top) / height
            }
        _ → pure Nothing