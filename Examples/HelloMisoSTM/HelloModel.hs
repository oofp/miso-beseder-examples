module HelloModel where

import Protolude    
    
data Model = Model 
    { clicked :: Bool
    , checked :: Bool
    } deriving (Eq, Show)

defaultModel :: Model
defaultModel = Model False False 
    
