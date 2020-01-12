{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
-- {-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}


module  HelloFlow where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Beseder.Misc.Prosumers
import           Beseder.Resources.Monitor
import           Beseder.Resources.Comm
import           Control.Concurrent.STM

import           Data.String 
import           HelloModel
import           qualified Protolude 


checkProducer :: TVar Model -> TaskQ (Producer TaskQ Bool)
checkProducer tm =  stmProducer (checked <$> readTVar tm)

mkCheckMonitor :: TVar Model -> TaskQ (BinaryMonitorProdRes TaskQ)
mkCheckMonitor tm =  mkBinaryMonitorProd <$> checkProducer tm



maybeFromBool :: Bool -> Maybe Bool
maybeFromBool True = Just True
maybeFromBool False = Nothing

btnComm :: TVar Model -> STMRes Bool
btnComm tmodel 
  = CommRes $ STMComm 
              { stmRecv = (maybeFromBool . clicked) <$> readTVar tmodel
              , stmSend = (\fl -> modifyTVar tmodel (\m -> m {clicked = fl}))
              , stmAckMsg = const False 
              , stmCloseMsg = Nothing 
              }

helloFlow :: TVar Model -> STransData TaskQ NoSplitter _ _
helloFlow tm = do
  newRes #btn (btnComm tm)
  nextEv
  try @("btn" :? IsCommAlive) $ do
      op (mkCheckMonitor tm) >>= newRes #ch 
      --nextEv
      handleEvents $ do
        on @("btn" :? IsMessageReceived) $ do 
          liftIO $ putStrLn ("click detected"::Text)
          invoke #btn GetNextMsg
      --nextEv
      -- pumpEvents
  termAndClearAllResources  


helloFlow2 :: TVar Model -> STransData TaskQ NoSplitter _ _
helloFlow2 tm = do
  op (mkCheckMonitor tm) >>= newRes #ch 
  newRes #t TimerRes
  invoke #t (StartTimer 3600)
  try @("t" :? IsTimerArmed) $ do
    handleEvents $ do
      liftIO $ putStrLn ("handleEvents ...." :: Text)
      onOrElse @("ch" :? IsBinMonitorOn) 
        (liftIO $ putStrLn ("**IsOn" :: Text))
        (liftIO $ putStrLn ("**IsOff" :: Text))
  liftIO $ putStrLn ("loop is completed" :: Text)
  termAndClearAllResources  
  

helloFlow3 :: TVar Model -> STransData TaskQ NoSplitter _ _
helloFlow3 tm = do
  newRes #t TimerRes
  invoke #t (StartTimer 36)
  newRes #t1 TimerRes
  invoke #t1 (StartTimer 3)
  op (mkCheckMonitor tm) >>= newRes #ch 
  try @("t" :? IsTimerArmed) $ do
    handleEvents $ do
      liftIO $ putStrLn ("handleEvents ...." :: Text)
      on @("ch" :? IsBinMonitorOn) 
        (liftIO $ putStrLn ("**IsOn" :: Text))
      on @(Not ("ch" :? IsBinMonitorOn)) 
        (liftIO $ putStrLn ("**IsOff" :: Text))
      on @("t1" :? IsTimerTriggered) $ do 
        clear #t1
        newRes #t1 TimerRes
        invoke #t1 (StartTimer 3)
  liftIO $ putStrLn ("loop is completed" :: Text)
  termAndClearAllResources  

-- :t evalSTransData (helloFlow undefined)
