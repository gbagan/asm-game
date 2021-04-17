module Logic where
import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Array (cons, uncons, (!!), updateAtIndices)
import Data.Tuple.Nested ((/\))

newtype InstrNo = InstrNo Int
newtype Register = Register Int

data Instruction = 
      Input 
    | Output
    | Jump InstrNo
    | JumpIfZero InstrNo
    | JumpIfNeg InstrNo
    | CopyFrom Register
    | CopyTo Register
    | Add Register
    | Sub Register
    | Increment Register
    | Decrement Register

type Program = Array Instruction

data From = FromInput | FromRegister Register | FromNothing

type State =
    {   instrNo :: InstrNo
    ,   currentValue :: Maybe {value :: Int, comesFrom :: From}
    ,   registers :: Array (Maybe Int)
    ,   input :: Array Int
    ,   output :: Array Int
    }


step :: Program -> State -> Either String State
step program state@{instrNo: InstrNo instrNo, currentValue, registers, input, output} =
    program !! instrNo # maybe (Left "fatal error") case _ of
        Input -> case uncons input of
                    Nothing -> Left "blah blah"
                    Just {head, tail} -> Right state{
                                            currentValue = Just{value: head, comesFrom: FromInput} 
                                          , input = tail
                                          , instrNo = InstrNo $ instrNo + 1
                                          }
        Output -> case currentValue of
                    Nothing -> Left "blah blah"
                    Just {value} -> Right state{
                                        currentValue = Nothing
                                      , output = cons value output
                                      , instrNo = InstrNo $ instrNo + 1
                                      }
        CopyFrom (Register r) -> case registers !! r of
                    Just (Just value) -> Right state {
                                              currentValue = Just {value, comesFrom: FromRegister (Register r)}
                                            , instrNo = InstrNo $ instrNo + 1
                                        }
                    _ -> Left "blah blah"
        CopyTo (Register r) -> case currentValue of
                    Nothing -> Left "blah blah"
                    Just {value} -> Right state {
                                        registers = registers # updateAtIndices [r /\ Just value]
                                      , instrNo = InstrNo $ instrNo + 1
                                    }

        Jump no -> Right state{instrNo = no}
        JumpIfZero no -> case currentValue of
                    Nothing -> Left "blah blah"
                    Just {value} ->
                        if value == 0 then
                            Right state{instrNo = no}
                        else
                            Right state{instrNo = InstrNo $ instrNo + 1}
        JumpIfNeg no -> case currentValue of
                    Nothing -> Left "blah blah"
                    Just {value} ->
                        if value < 0 then
                            Right state{instrNo = no}
                        else
                            Right state{instrNo = InstrNo $ instrNo + 1}
        _ -> Left "not implemented"

