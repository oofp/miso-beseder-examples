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
import           Beseder.Misc.Misc
import           Beseder.Misc.Prosumers
import           Beseder.Resources.Monitor
import           Beseder.Resources.Switch
import           Beseder.Resources.Comm
import           Control.Concurrent.STM
import           EntranceDoorModel
import           EntranceDoor

doorConsumer :: TVar Model -> STMConsumer Bool
doorConsumer tm = STMConsumer (\fl -> modifyTVar tm (\m -> m {doorOpen = fl}))

mkDoorSwitch :: TVar Model -> TaskQ (BinSwitchConsRes TaskQ)
mkDoorSwitch tm = mkBinSwitchCons <$> (consumer $ doorConsumer tm)

mkPrxMonitor :: TVar Model -> (Model -> Bool) -> TaskQ (BinaryMonitorProdRes TaskQ)
mkPrxMonitor tm getter =  mkBinaryMonitorProd <$> stmProducer (getter <$> readTVar tm)

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

initHandler :: TVar Model -> STransData TaskQ NoSplitter _ _
initHandler tm = do
  newRes #fobReader (fobComm tm)
  nextEv
  try @("fobReader" :? IsCommAlive) $ do
      op (mkDoorSwitch tm) >>= newRes #door  
      op (mkPrxMonitor tm inProx) >>= newRes #inDet 
      op (mkPrxMonitor tm outProx) >>= newRes #outDet
      doorHandler 5
  termAndClearAllResources  

-- :t evalSTransData (initHandler undefined)
--evalAssert :: Proxy '(_,'[])
--evalAssert = evalSTransData (initHandler undefined)

--interpretInitHandler :: TVar Model -> STrans (ContT Bool) TaskQ NoSplitter '[()] _ _ _ ()
--interpretInitHandler tm = interpret (initHandler tm) 

