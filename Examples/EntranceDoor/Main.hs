module Main where

import Protolude    
import Miso hiding (go)
import Miso.String
import Language.Javascript.JSaddle.Warp as JSaddle
import Control.Concurrent.STM
import Miso.STM.MisoSTM
import EntranceDoorModel
import EntranceDoorApp

testView :: BoundView Model
testView = 
    El div_ []
    [ El div_ [] 
        [ El button_ [BE onClick (\m -> m {fobDetected = True})] [Txt "Fob"] ] 
    , El div_ [] 
        (checkbox inProx (\fl m -> m {inProx = fl}) "Internarl prox")
    , El div_ [] 
        (checkbox outProx (\fl m -> m {outProx = fl}) "Outside prox")
    , El div_ [] 
        [DynTxt (\m -> if doorOpen m then "Door Open" else "DoorClose")]
    ]  

main :: IO ()
main = do
    putStrLn ("Visit http://localhost:8000/ for 'door simulation'" :: Text)
    JSaddle.run 8000 $ startApp =<< mkBoundApp (srvApp defaultModel) defaultModel testView

srvApp :: Model -> TVar Model -> IO ()
srvApp initModel tmodel = do 
    putStrLn ("started serverApp" :: Text)
    void $ async $ doorApp tmodel
    go initModel 
  where 
    go curModel = do
        newModel <- monitorModel curModel tmodel
        putStrLn (("model changed was:"::Text) <> show curModel <> " now: " <> show newModel)
        go newModel 

monitorModel :: Model -> TVar Model -> IO Model
monitorModel baseModel tmodel = atomically $ do
    curModel <- readTVar tmodel
    if curModel == baseModel
    then retry
    else return curModel
    
