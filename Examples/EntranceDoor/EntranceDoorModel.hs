module EntranceDoorModel where

import Protolude    
    
data Model = Model 
    { fobDetected :: Bool
    , inProx :: Bool
    , outProx :: Bool
    , doorOpen :: Bool
    } deriving (Eq, Show)

defaultModel :: Model
defaultModel = Model False False False False
    
