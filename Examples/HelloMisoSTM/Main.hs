module Main where

import Protolude    
import Miso hiding (go)
import Miso.String
import Control.Monad.IO.Class
import Language.Javascript.JSaddle.Warp as JSaddle
import Control.Concurrent.STM
import Data.Text
import Miso.STM.MisoSTM
import HelloModel
import HelloApp

testView :: BoundView Model
testView = 
    El div_ []
    [ El div_ [] 
        [ El button_ [BE onClick (\m -> m {clicked = True})] [Txt "Click"] ] 
    , El div_ [] 
        (checkbox checked (\fl m -> m {checked = fl}) "Checked")
    , El div_ [] 
        [DynTxt (\m -> if clicked m then "On" else "Off")]
    , El div_ [] 
        [DynTxt (\m -> if checked m then "Checked" else "Unchecked")]
    ]  

main :: IO ()
main = do
    JSaddle.run 8000 $ startApp =<< mkBoundApp (srvApp defaultModel) defaultModel testView
    --newTVarIO defaultModel >>= helloApp


srvApp :: Model -> TVar Model -> IO ()
srvApp initModel tmodel = do 
    putStrLn ("started serverApp" :: Text)
    void $ async $ do
      putStrLn ("started helloApp" :: Text)
      helloApp tmodel
      putStrLn ("stop helloApp" :: Text)
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
    
