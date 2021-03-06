{-# LANGUAGE DeriveDataTypeable,DeriveGeneric,ScopedTypeVariables #-}
import Control.Distributed.Process
import Network.Transport.TCP
import Control.Distributed.Process.Node (initRemoteTable,newLocalNode,forkProcess)
import Prelude hiding (log)
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Control.Concurrent

data Measurement = DummyMeasurement deriving (Show,Typeable,Generic)

data HOCommand' = HOCommand' deriving (Show,Typeable,Generic,Eq)

data HOReq = HOReq deriving (Show,Typeable,Generic,Eq)

data HOConnect = HOConnect deriving (Show,Typeable,Generic,Eq)

data Flush = Flush deriving (Show,Typeable,Generic,Eq)

data Activation = Activation deriving (Show,Typeable,Generic,Eq)

data LinkActReq = LinkActReq deriving (Show,Typeable,Generic,Eq)
                
data LinkEstablished =  LinkEstablished deriving (Show,Typeable,Generic,Eq)
                
data LinkActive = LinkActive ProcessId deriving (Show,Typeable,Generic,Eq)
                
data HOAck = HOAck ProcessId deriving (Show,Typeable,Generic,Eq)
                
data HOCommand = HOCommand ProcessId deriving (Show,Typeable,Generic,Eq)

instance Binary Measurement
instance Binary HOCommand'
instance Binary HOReq
instance Binary HOConnect
instance Binary Flush
instance Binary Activation
instance Binary LinkActReq
instance Binary LinkEstablished
instance Binary LinkActive
instance Binary HOAck
instance Binary HOCommand

msc :: Process ()
msc = do 
  (oldBSC,newBSC) <- expect
--  liftIO $ print oldBSC
  go oldBSC newBSC 
 where
  go oldBSC (newBSC::ProcessId) = do
  self <- getSelfPid
  liftIO $ putStrLn $ "At MSC - " ++ show (self,oldBSC,newBSC)
  receiveWait
    [ match $ \(HOReq,oldBSC) -> do        
  liftIO $ log oldBSC self HOReq
  send newBSC (HOCommand',self)
  --send newBSC ("Ack",self)
    , match $ \(HOAck newbs,newBSC) -> do 
  liftIO $ log newBSC self (HOAck newbs)
  send oldBSC (HOCommand newbs,self)
    , match $ \(HOConnect,newBSC) -> do
  liftIO $ log newBSC self HOConnect
  send oldBSC (HOConnect,self)
    , match $ \(Flush,oldBSC) -> do
  liftIO $ log oldBSC self Flush
  liftIO $ putStrLn "HandOver Complete"
  -- handover complete here
  -- oldBSC becomes newBSC and newBSC becomes oldBSC for next handoff
  return ()
    , matchUnknown $ do
  liftIO $ putStrLn $ "Matched Unknown message at " ++ show (self,oldBSC,newBSC)
    ]
  go oldBSC newBSC

bsc :: Process ()
bsc = do
  (bs,msc) <- expect
  go bs msc
 where
  go bs msc = do
  self <- getSelfPid
  liftIO $ putStrLn $ "At BSC - " ++ show (self,bs,msc)
  receiveWait
    [ match $ \(HOReq,oldBS) -> do
  liftIO $ log oldBS self HOReq 
  send msc (HOReq,self)
    , match $ \(HOCommand',msc) -> do
  liftIO $ log msc self HOCommand'
  send bs (HOCommand',self)
    , match $ \(HOCommand newbs,msc) -> do
  liftIO $ log msc self (HOCommand newbs)
  send bs (HOAck newbs,self)
    , match $ \(Activation,newbs) -> do
  liftIO $ log newbs self Activation
  send msc (HOAck newbs,self)
    , matchIf (\(HOConnect,pid) -> pid == bs) $ \(HOConnect,pid) -> do
  liftIO $ log pid self HOConnect
  send msc (HOConnect,self)
    , matchIf (\(HOConnect,pid) -> pid == msc) $ \(HOConnect,pid) -> do
  liftIO $ log pid self HOConnect
  send bs (Flush,self)
    , match $ \(Flush,oldbs) -> do
  liftIO $ log oldbs self Flush
  send msc (Flush,self)
    , matchUnknown $ do
  liftIO $ putStrLn $ "Matched Unknown message at " ++ show (self,bs,msc)
    ]
  go bs msc                                           
                                           
ms :: Measurement -> Process ()
ms measurement = do
  bs <- expect
  self <- getSelfPid
  liftIO $ threadDelay 480000
  send bs (measurement,self) 
  go bs measurement
 where
  go bs measurement = do
  self <- getSelfPid

  liftIO $ putStrLn $ "At MS - " ++ show (self,bs)

  receiveWait
    [ match $ \(LinkActive newbs,bs) -> do
  liftIO $ log bs self (LinkActive newbs)
  send newbs (LinkActReq,self)
    , match $ \(LinkEstablished,new) -> do
  liftIO $ log new self LinkEstablished
    , matchUnknown $ do
  liftIO $ putStrLn $ "Matched Unknown message at " ++ show (self,bs)
    ]  
  go bs measurement

bs :: Process () 
bs = do
  (bsc,ms) <- expect
  go bsc ms
 where 
  go bsc ms = do
  self <- getSelfPid
  liftIO $ putStrLn $ "At bs - " ++ show (self,ms,bsc)
  receiveWait
    [ match $ \(HOAck newbs,oldbsc) -> do
  liftIO $ log oldbsc self (HOAck newbs)
  send ms (LinkActive newbs,self)
    , match $ \(Flush,oldbsc) -> do
  liftIO $ log oldbsc self Flush
  send oldbsc (Flush,self)
    , match $ \(DummyMeasurement,ms) -> do
  liftIO $ log ms self HOConnect
  -- decide whether to initiate handoff 
  send bsc (HOReq,self)
    , match $ \(HOCommand',newbsc) -> do 
  liftIO $ log newbsc self HOCommand'
  send newbsc (Activation,self)
    , match $ \(LinkActReq,ms) -> do
  liftIO $ log ms self LinkActReq
  send ms (LinkEstablished,self)
  send bsc (HOConnect,self)
    , matchUnknown $ do
  liftIO $ putStrLn $ "Matched Unknown message at " ++ show (self,bsc,ms)
    ]  
  go bsc ms


log :: (Show a) => ProcessId -> ProcessId -> a -> IO ()
log from to msg = putStrLn $ show to ++ " <--- " ++ show from ++ " ---- " ++ show msg

master :: Process ()
master = do
  mscId         <- spawnLocal msc
  oldbscId      <- spawnLocal bsc
  newbscId      <- spawnLocal bsc
  oldbsId       <- spawnLocal bs
  newbsId       <- spawnLocal bs
  msId          <- spawnLocal $ ms DummyMeasurement
  
  mscref        <- monitor mscId
  oldbscref     <- monitor oldbscId
  newbscref     <- monitor newbscId
  oldbsref      <- monitor oldbsId
  newbsref      <- monitor newbsId
  msref         <- monitor msId

  send mscId (oldbscId,newbscId)
  send oldbscId (oldbsId,mscId)
  send newbscId (newbsId,mscId)
  send oldbsId (oldbscId,msId)
  send newbsId (newbscId,msId)
  send msId oldbsId
  
  go 
 where 
  go = do
  receiveWait 
    [ match $ \(ProcessMonitorNotification ref id dr) -> do 
  liftIO $ putStrLn $ show (ref,id,dr)
    , matchUnknown $ do
  liftIO $ putStrLn "Matched Unknown at master"
    ]
  go
  

main :: IO ()
main = do
  either_ <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  case either_ of Right t -> do 
                      node <- newLocalNode t initRemoteTable
                      putStrLn "Before launching processes"
                      forkProcess node master
                      threadDelay 100000000
                  Left x -> print x