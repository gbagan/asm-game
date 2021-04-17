module Model where
import Prelude
import Pha.Update (Update)
import Logic (Program, State)

type Model =
    {   program :: Program 
    ,   state :: State
    }

type Msg = Unit

update :: Msg -> Update Model Unit
update _ = pure unit