module View  (view) where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Tuple.Nested ((/\))
import Logic (Instruction(..), Register(..), InstrNo(..))
import Model (Model, Msg(..), Message(..), Request(..), Dragged(..), draggedInstr)
import Levels (levels)
import Pha.Html (Html, Prop, EventHandler)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event as Ev
import Web.HTML.HTMLElement as HE
import Web.UIEvent.MouseEvent as MouseEvent

view :: Model -> Html Msg
view model@{program,
            level,
            state: state@{instrNo: InstrNo instrNo, currentValue, input, output, registers},
            request,
            pointer,
            dragged,
            dragAt,
            message
           } =
    H.div [H.class_ "asm-main", E.onMouseUp CancelDrag, E.on "mousemove" move]
    [   levelDropdown
    ,   H.div []
        [   viewState
        ,   H.button [E.onClick Step, H.class_ "button"] [H.text "step"]
        ,   H.button 
                [   E.onClick RunProgram
                ,   H.class_ "button is-primary"
                ,   P.disabled $ Array.length program == 0
                ]
                [   H.text "run"]
        ,   H.button [E.onClick Reset, H.class_ "button is-danger"] [H.text "reset"]
        ,   H.div [H.class_ "asm-message"] [H.text $ case message of
                NoMessage -> ""
                SuccessMessage msg -> msg 
                ErrorMessage msg -> "Error: " <> msg
            ]
        ]
    ,   viewInstructions level.availableInstructions
    ,   viewProgram
    ,   H.fromMaybe $ viewPointer <$> pointer <*> draggedInstr model
    ,   H.div [H.class_ "box asm-objective"]
        [   H.text level.instructionText
        ]
    ]
    where
        move e = map (SetPointer <<< Just) <$> pointerDecoder e
        currentInstr = program !! instrNo
        areRegistersSelectable = case request of
            RegisterRequest _ -> true
            _ -> false

        levelDropdown =
            H.div [H.class_ "dropdown asm-level-dropdown is-hoverable"]  --, H.class' "is-active" isLevelDropdownActive]
            [   H.div [H.class_ "dropdown-trigger"]
                [   H.button [H.class_ "button"]
                    [   H.span [] [H.text "Level"]
                    ,   H.span [H.class_ "icon is-small"]
                        [   H.h "i" [H.class_ "fas fa-angle-down"] []]
                    ]
                ]
            ,   H.div [H.class_"dropdown-menu"]
                [   H.div [H.class_"dropdown-content"] $
                    NonEmptyArray.toArray levels <#> \{id, title} ->
                        H.a [P.href ("#" <> id), H.class_ "dropdown-item"]
                        [   H.text title]
                ]
            ]

        viewState =
            H.div [H.class_ "box asm-state"]
            [   viewInput input
            ,   H.div [H.class_ "asm-state-center"]
                [   H.maybe currentValue \{value} -> H.div [H.class_ "asm-value asm-current-value"] [H.text $ show value]
                ,   viewRegisters areRegistersSelectable registers
                ]
            ,   viewOutput output
            ]

        viewProgram =
            H.div
                [H.class_ "box asm-program", E.stopPropagationOn "mouseup" $ E.always $ Just DropOnProgram /\ true]
                [   H.div 
                    [   H.class_ "asm-program-1"] 
                    [   H.div
                        [   H.class_ "asm-instruction-arrow"
                        ,   H.style "left" "0"
                        ,   H.style "top" $ show (instrNo * 2) <> "rem"
                        ]
                        [   H.text "➤"]
                    ]
                ,   H.div
                    [   H.class_ "asm-program-2"] (
                        program # Array.mapWithIndex \i _ ->
                            H.div
                            [   H.class_ "asm-instruction-no"]
                            [   H.text $ show i]
                    )
                ,   H.div
                    [   H.class_ "asm-program-3"] $
                        program # Array.mapWithIndex \i {id, instr} -> viewInstr' i id instr
                ]

        viewInstr' idx id instr =
            viewInstr
            ([   H.class_ "asm-program-instruction"
            ,   E.stopPropagationOn "mouseup" $ E.always $ Just (DropOnInstr idx) /\ true
            ,   E.onMouseDown (DragInstrNo idx)
            ,   E.onMouseEnter (DragAt $ Just idx)
            ,   E.onMouseLeave (DragAt Nothing)
            ] <>
                if dragged == Just (DraggedInstrNo idx) then
                    [H.style "display" "none"]
                else if isJust dragged && isJust dragAt && dragAt <= Just idx then
                    [H.style "transform" "translateY(2em)"] 
                else
                    []
            )
                (Just idx) instr

viewPointer :: {x :: Number, y :: Number} -> Instruction -> Html Msg
viewPointer {x, y} instr =
    H.div [H.class_ "asm-ui-cursor", H.style "left" $ pc x, H.style "top" $ pc y]
        [viewInstr [] Nothing instr]

viewInstructions :: Array Instruction -> Html Msg
viewInstructions instrs =
    H.div [H.class_ "box"] $ 
        instrs <#> \instr -> 
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
    H.div [H.class_ "box asm-input"] $ l <#> \v ->
        H.div [H.class_ "asm-value"] [H.text $ show v]

viewRegisters :: Boolean -> Array (Maybe Int) -> Html Msg
viewRegisters selectable l =
    H.div [H.class_ "box asm-registers"] $ l # Array.mapWithIndex \i v ->
        H.div [H.class_ "asm-register", H.class' "asm-register-selectable" selectable, E.onClick (SelectRegister i)] [
            H.maybe v \val ->
                H.div [H.class_ "asm-value"] [H.text $ show val]        
        ]

viewOutput :: Array Int -> Html Msg
viewOutput l =
    H.div [H.class_ "box asm-output"] $ l <#> \v ->
        H.div [H.class_ "asm-value"] [H.text $ show v]


pointerDecoder ∷ EventHandler { x ∷ Number, y ∷ Number }
pointerDecoder ev = do
    case MouseEvent.fromEvent ev /\ Ev.currentTarget ev of
        Just mouseEv /\ Just el → do
            -- dans l'implémentation actuelle en purescript, getBoundingClientRect ne s'applique
            -- qu'à des HTMLElement et pas à des SVG Elements
            let el' = unsafeCoerce el ∷ HE.HTMLElement
            {left, top, width, height} ← HE.getBoundingClientRect el'
            pure $ Just {
                x: (toNumber(MouseEvent.clientX mouseEv) - left) / width,
                y: (toNumber(MouseEvent.clientY mouseEv) - top) / height
            }
        _ → pure Nothing