module Levels where
import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (find)
import Data.Array.NonEmpty (NonEmptyArray, cons', head)
import Logic (InstrNo(..), Instruction(..), Register(..), Level)

levels :: NonEmptyArray Level
levels = cons'
    {   id: "warmingup"
    ,   title: "Warming up"  
    ,   input: [10, 5, 8]
    ,   registers: []
    ,   expectedOutput: [10, 5, 8]
    ,   availableInstructions: [Input, Output]
    ,   instructionText: "For every element in the INBOX, put it into the OUTBOX."
    }
    [
    {   id: "permutations"
    ,   title: "Permutations"  
    ,   input: [9, 6, 7, 12, 0, 15]
    ,   registers: [Nothing, Nothing, Nothing]
    ,   expectedOutput: [6, 9, 12, 7, 15, 0]
    ,   availableInstructions: [Input, Output, CopyFrom (Register 0), CopyTo (Register 0), Jump (InstrNo 0)]
    ,   instructionText: "Pick the first two values in the INBOX and put them in the OUTBOX in the reverse order. Repeat until the INBOX is empty."
    }
    ,
    {   id: "additions"
    ,   title: "Additions"
    ,   registers: [Nothing, Nothing, Nothing]
    ,   input: [10, 5, 8, 14, 5, 3]
    ,   expectedOutput: [15, 22, 8]
    ,   availableInstructions: [Input, Output, CopyFrom (Register 0), CopyTo (Register 0), Add (Register 0), Jump (InstrNo 0)]
    ,   instructionText: "For each two elements in the INBOX, add them and put the result in the OUTBOX."
    }
    ,
    {   id: "tripler"
    ,   title: "Tripler"
    ,   registers: [Nothing, Nothing, Nothing] 
    ,   input: [3, -7, 5, 0]
    ,   expectedOutput: [9, -21, 15, 0]
    ,   availableInstructions: [Input, Output, CopyFrom (Register 0), CopyTo (Register 0), Add (Register 0), Jump (InstrNo 0)]
    ,   instructionText: "For each element in the INBOX, triple it and put the result in the OUTBOX."
    }
    ,
    {   id: "octupler"
    ,   title: "Octupler"  
    ,   registers: [Nothing, Nothing, Nothing]
    ,   input: [4, -6, 0, 8]
    ,   expectedOutput: [32, -48, 0, 64]
    ,   availableInstructions: [Input, Output, CopyFrom (Register 0), CopyTo (Register 0), Add (Register 0), Jump (InstrNo 0)]
    ,   instructionText: "For each element in the INBOX, multiply it by 8 and put the result in the OUTBOX."
    }
    ]

getLevelById :: String -> Level
getLevelById id' = levels
                    # find (\{id} -> id == id')
                    # fromMaybe (head levels)