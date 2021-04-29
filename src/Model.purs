module Model where

import Prelude

import Control.Monad.Rec.Class (tailRecM, Step(..))
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Logic (Program, State, Level, Instruction, InstrNo(..), Register(..), modifyRegister, step)
import Levels (getLevelById)
import Pha.Update (Update, delay, get, modify_)
import Web.HTML (window)
import Web.HTML.Location as L
import Web.HTML.Window (location)

data Dragged = DraggedInstr Instruction | DraggedInstrNo Int
derive instance eqDragged :: Eq Dragged

data Request = RegisterRequest Int | NoRequest

data Message = NoMessage | ErrorMessage String

type Model =
    {   program :: Program
    ,   state :: State
    ,   level :: Level
    ,   dragged :: Maybe Dragged
    ,   dragAt :: Maybe Int
    ,   pointer :: Maybe { x ∷ Number, y ∷ Number }
    ,   request :: Request
    ,   id :: Int
    ,   message :: Message
    }

myProgram :: Program
myProgram = []

myState :: State
myState =
    {   instrNo: InstrNo 0
    ,   currentValue: Nothing
    ,   registers: [Nothing, Nothing, Nothing]
    ,   input: [4, 2, 5, 6]
    ,   output: []
    }

initModel :: Model
initModel = initializeLevel
    {   program: myProgram
    ,   state: myState
    ,   level: getLevelById "additions"
    ,   dragged: Nothing
    ,   dragAt: Nothing
    ,   pointer: Nothing
    ,   request: NoRequest
    ,   id: 0
    ,   message: NoMessage
    }

data Msg = Step
         | RunProgram
         | Reset
         | AskRequest Request
         | SelectRegister Int
         | DragInstr Instruction
         | DragInstrNo Int
         | DragAt (Maybe Int)
         | DropOnInstr Int
         | DropOnProgram
         | CancelDrag
         | SetPointer (Maybe { x ∷ Number, y ∷ Number })
         | HashChanged

initializeLevel :: Model -> Model
initializeLevel m@{level} = m{state = myState{input = level.input}, program = [], message = NoMessage}

draggedInstr :: Model -> Maybe Instruction
draggedInstr model@{dragged, program} = join $ dragged <#> case _ of
    DraggedInstr instr -> Just instr
    DraggedInstrNo no -> program !! no <#> _.instr

update :: Msg -> Update Model Unit
update Step = modify_ \model@{program, state} ->
    case step program state of
        Left text -> model
        Right st2 -> model{state = st2}
update RunProgram = get >>= \{state} → tailRecM go state where
        go st = do
            {program, level} <- get
            case step program st of
                Left text -> do
                    modify_ _{message = ErrorMessage text}
                    pure (Done unit)
                Right st2 ->
                    let InstrNo instrNo =  st2.instrNo in 
                    if instrNo < Array.length program && (st2.input /= [] || isJust st2.currentValue) then do
                        modify_ _{state = st2}
                        delay $ Milliseconds 1000.0
                        pure (Loop st2) 
                    else if Array.reverse st2.output == level.expectedOutput then do
                        modify_ _{ state = st2
                                 , message = ErrorMessage "Success"
                                 }
                        pure (Done unit)
                    else do
                        modify_ _{ state = st2
                                 , message = ErrorMessage $ "Expected " <> show level.expectedOutput <> " but received " <> show (Array.reverse st2.output)
                                 }
                        pure (Done unit)
update Reset = modify_ initializeLevel
update (AskRequest r) = modify_ _{request = r}
update (SelectRegister r) = modify_ \model@{request, program} ->
    case request of
        RegisterRequest i ->
            model{ program = program # Array.modifyAtIndices [i] \{instr, id} -> {id, instr: modifyRegister (Register r) instr}
                 , request = NoRequest
            }
        _ -> model

update (DragInstr instr) = modify_ _{dragged = Just (DraggedInstr instr)}
update (DragInstrNo no) = modify_ _{dragged = Just (DraggedInstrNo no)}
update (DragAt idx) = modify_ _{dragAt = idx}
update (DropOnInstr no) = modify_ \model@{program, dragged, id} ->
    case dragged of
        Nothing -> model
        Just (DraggedInstr instr) ->
                        let instr'= {instr, id} in
                        model{
                            program = Array.insertAt no instr' program # fromMaybe (Array.snoc program instr')
                        ,   dragged = Nothing
                        ,   id = id + 1
                        }
        Just (DraggedInstrNo no') -> model{
                            program = arrayMove no' no id program
                        ,   dragged = Nothing
                        ,   id = id + 1
                        }
update CancelDrag = modify_ \model@{program, dragged} ->
    case dragged of
        Just (DraggedInstrNo no) -> model {
                                    program = Array.deleteAt no program # fromMaybe program
                                ,   dragged = Nothing
                                }
        _ -> model { dragged = Nothing}
update DropOnProgram = modify_ \model@{program, dragged, id} ->
    case dragged of
        Nothing -> model
        Just (DraggedInstr instr) -> model{
                            program = Array.snoc program {id, instr}
                        ,   dragged = Nothing
                        ,   id = id + 1
                        }
        Just (DraggedInstrNo no) -> model{
                            program = arrayMove no (Array.length program - 1) id program
                        ,   dragged = Nothing
                        ,   id = id + 1
                        }
update (SetPointer p) = modify_ _{pointer = p}
update HashChanged = do
    hash <- liftEffect $ window >>= location >>= L.hash
    modify_ $ initializeLevel <<< _{level = getLevelById (String.drop 1 hash)}

arrayMove :: Int -> Int -> Int -> Program -> Program
arrayMove from to newId t = fromMaybe t do
    {instr} <- t !! from
    t2 <- Array.deleteAt from t
    Array.insertAt (if from <= to then to else to) {instr, id: newId} t2