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


module  EntranceDoorInit where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Beseder.Misc.Prosumers
import           Beseder.Resources.Monitor.BinaryMonitorRes
import           Beseder.Resources.State.ImpRes 
import           Beseder.Resources.State.BinarySwitch
import           Beseder.Resources.State.DataRes 
import           Beseder.Resources.Comm
import           Control.Concurrent.STM
import           Control.Monad.Cont (ContT)

import           Data.String 
import           EntranceDoorModel
import           EntranceDoor
import           qualified Protolude 

doorConsumer :: TVar Model -> STMConsumer Bool
doorConsumer tm = STMConsumer (\fl -> modifyTVar tm (\m -> m {doorOpen = fl}))

proxProducer :: TVar Model -> (Model -> Bool) -> TaskQ (Producer TaskQ Bool)
proxProducer tm getter =  stmProducer (getter <$> readTVar tm)

fobComm :: TVar Model -> STMRes Bool
fobComm tmodel 
  = CommRes $ STMComm 
              { stmRecv = (maybeFromBool . fobDetected) <$> readTVar tmodel
              , stmSend = (\fl -> modifyTVar tmodel (\m -> m {fobDetected = fl}))
              , stmAckMsg = const False
              , stmCloseMsg = Nothing 
              }

maybeFromBool :: Bool -> Maybe Bool
maybeFromBool True = Just True
maybeFromBool False = Nothing

type InitState = 
  '[  ( CommWaitForMsg "fobReader" (STMComm Bool) Bool Bool () TaskQ ,
      ( BinSwitchOff TaskQ "door",
      ( BinMonitorOff TaskQ "inDet", BinMonitorOff TaskQ "outDet"))),()] 


initHandler :: TVar Model -> STransData TaskQ NoSplitter _ _
initHandler tm = do
  newRes #fobReader (fobComm tm)
  nextEv
  try @("fobReader" :? IsCommAlive) $ do
      op (binSwRes (doorConsumer tm)) >>= newRes #door  
      op (proxProducer tm inProx) >>= (newRes #inDet . BinaryMonitor)
      op (proxProducer tm outProx) >>= (newRes #outDet . BinaryMonitor)
      doorHandler 5
  termAndClearAllResources  

-- :t evalSTransData (initHandler undefined)
--evalAssert :: Proxy '(_,'[])
--evalAssert = evalSTransData (initHandler undefined)

--interpretInitHandler :: TVar Model -> STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ _ ()
--interpretInitHandler tm = interpret (initHandler tm) 

