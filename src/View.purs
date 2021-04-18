module View  (view) where

import Prelude

import Data.Array (mapWithIndex, (!!))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Logic (Instruction(..), Register(..), InstrNo(..))
import Model (Model, Msg(..), Request(..), draggedInstr)
import Pha.Html (Html, Prop, EventHandler)
import Pha.Html as H
import Pha.Html.Events as E
import Pha.Html.Util (pc)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event as Ev
import Web.HTML.HTMLElement as HE
import Web.UIEvent.MouseEvent as ME

view :: Model -> Html Msg
view model@{program,
            state: state@{instrNo: InstrNo instrNo, currentValue, input, output, registers},
            request,
            pointer
           } =
    H.div [H.class_ "main", E.onMouseUp CancelDrag, E.on "mousemove" move]
    [   viewState
    ,   viewInstructions
    ,   viewProgram
    ,   H.button [E.onClick Step] [H.text "step"]
    ,   H.button [E.onClick RunProgram] [H.text "run"]
    ,   H.fromMaybe $ viewPointer <$> pointer <*> draggedInstr model
    ]
    where
        move e = map (SetPointer <<< Just) <$> pointerDecoder e
        currentInstr = program !! instrNo
        areRegistersSelectable = case request of
            RegisterRequest _ -> true
            _ -> false

        viewState =
            H.div [H.class_ "state"]
            [   viewInput input
            ,   H.div [H.class_ "state-center"]
                [   H.div [H.class_ "value"] [H.text $ maybe "" (show <<< _.value) currentValue]
                ,   viewRegisters areRegistersSelectable registers
                ]
            ,   viewOutput output
            ]

        viewProgram =
            H.div [H.class_ "program", E.stopPropagationOn "mouseup" $ E.always $ Just DropOnProgram /\ true] $
                program # mapWithIndex \i instr -> viewInstr' i (i == instrNo) instr

        viewInstr' :: Int -> Boolean -> Instruction -> Html Msg
        viewInstr' no isCurrent instr =
            H.div [H.class_ "instruction"] 
            [   H.div [H.class_ "instruction-arrow"] [H.text $ if isCurrent then "➤" else ""]
            ,   H.div [H.class_ "instruction-no"] [H.text $ show no]
            ,   viewInstr [E.stopPropagationOn "mouseup" $ E.always $ Just (DropOnInstr no) /\ true, E.onMouseDown (DragInstrNo no)] (Just no) instr
            ]


viewPointer :: {x :: Number, y :: Number} -> Instruction -> Html Msg
viewPointer {x, y} instr =
    H.div [H.class_ "ui-cursor", H.style "left" $ pc x, H.style "top" $ pc y]
        [viewInstr [] Nothing instr]


viewInstructions :: Html Msg
viewInstructions =
    H.div [] $ [Input, Output, CopyFrom (Register 0), CopyTo (Register 0), Add (Register 0), Jump (InstrNo 0)] <#> \instr -> 
        viewInstr [E.onMouseDown (DragInstr instr)] Nothing instr




viewInstr :: Array (Prop Msg) -> Maybe Int -> Instruction -> Html Msg
viewInstr props instrNo instr =
    H.div ([H.class_ "instruction-data"] <> props)
        case instr of
            Input -> [H.text "input"]
            Output -> [H.text "output"]
            CopyFrom (Register r) -> 
                [   H.text "copyfrom"
                ,   H.div 
                    [   H.class_ "instr-register"
                    ,   E.stopPropagationOn "mousedown" $ E.always (Nothing /\ true)
                    ,   E.onClick (AskRequest (maybe NoRequest RegisterRequest instrNo))
                    ]
                    [H.text $ show r]
                ]
            CopyTo (Register r) -> 
                [   H.text "copyto"
                ,   H.div
                [   H.class_ "instr-register"
                    ,   E.stopPropagationOn "mousedown" $ E.always (Nothing /\ true)
                    ,   E.onClick (AskRequest (maybe NoRequest RegisterRequest instrNo))
                    ]
                    [H.text $ show r]
                ]
            Add (Register r) -> 
                [   H.text "add"
                ,   H.div 
                    [   H.class_ "instr-register"
                    ,   E.stopPropagationOn "mousedown" $ E.always (Nothing /\ true)
                    ,   E.onClick (AskRequest (maybe NoRequest RegisterRequest instrNo))
                    ]
                    [H.text $ show r]
                ]
            Sub (Register r) -> 
                [   H.text "sub"
                ,   H.div
                    [   H.class_ "instr-register"
                    ,   E.stopPropagationOn "mousedown" $ E.always (Nothing /\ true)
                    ,   E.onClick (AskRequest (maybe NoRequest RegisterRequest instrNo))
                    ]
                    [H.text $ show r]
                ]
            Jump (InstrNo n) -> [H.text $ "jump " <> show n]
            _ -> [H.text "???"]


viewInput :: Array Int -> Html Msg
viewInput l =
    H.div [H.class_ "input"] $ l <#> \v ->
        H.div [H.class_ "value"] [H.text $ show v]

viewRegisters :: Boolean -> Array (Maybe Int) -> Html Msg
viewRegisters selectable l =
    H.div [H.class_ "registers"] $ l # mapWithIndex \i v ->
        H.div [H.class_ "register", H.class' "register-selectable" selectable, E.onClick (SelectRegister i)] [
            H.maybe v \val ->
                H.div [H.class_ "value"] [H.text $ show val]        
        ]

viewOutput :: Array Int -> Html Msg
viewOutput l =
    H.div [H.class_ "output"] $ l <#> \v ->
        H.div [H.class_ "value"] [H.text $ show v]


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