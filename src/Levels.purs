module Levels where
import Prelude
import Data.Maybe (fromMaybe)
import Data.Foldable (find)
import Data.Array.NonEmpty (NonEmptyArray, cons', head)
import Logic (InstrNo(..), Instruction(..), Register(..), Level)

levels :: NonEmptyArray Level
levels = cons'
    {   id: "warmingup"
    ,   title: "Warming up"  
    ,   input: [10, 5, 8]
    ,   expectedOutput: [10, 5, 8]
    ,   availableInstructions: [Input, Output]
    ,   instructionText: "For every element in the INPUT, put it in the OUTPUT."
    }
    [
    {   id: "additions"
    ,   title: "Additions"  
    ,   input: [10, 5, 8, 14]
    ,   expectedOutput: [15, 22]
    ,   availableInstructions: [Input, Output, CopyFrom (Register 0), CopyTo (Register 0), Add (Register 0), Jump (InstrNo 0)]
    ,   instructionText: "For every pair of elements in the INPUT, do the sum and put the result in the OUTPUT."
    }
    ]

getLevelById :: String -> Level
getLevelById id' = levels
                    # find (\{id} -> id == id')
                    # fromMaybe (head levels)