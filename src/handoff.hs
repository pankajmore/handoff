import Control.Distributed.Process

data Measurement = DummyMeasurement

data HandoffMsg = HOCommand ProcessId
                | HOCommand'
                | HOReq 
                | HOAck ProcessId
                | HOConnect
                | Flush
                | Activation
                | LinkActive ProcessId
                | LinkActReq
                | LinkEstablished


msc ::  ProcessId -> ProcessId -> Process ()
msc oldBSC  newBSC = do
  self <- getSelfPid
  receiveWait
    [ match $ \(HOReq,oldBSC) -> do        
  liftIO $ log oldBSC self HOReq
  send newBSC (HOCommand',self)
  msc oldBSC newBSC
    , match $ \(HOAck newbs,newBSC) -> do 
  liftIO $ log newBSC self HOAck
  send oldBSC (HOCommand newbs,self)
  msc oldBSC newBSC
    ]

oldbsc :: ProcessId -> ProcessId -> Process ()
oldbsc oldBS msc = do
  self <- getSelfPid
  receiveWait
    [ match $ \(HOReq,oldBS) -> send msc (HOReq,self) >> oldbsc oldBS msc
    , match $ \(HOCommand newbs,msc) -> send oldBS (HOAck newbs,self) >> oldbsc oldBS msc
    ]
                                           
newbsc :: ProcessId -> ProcessId -> Process ()
newbsc newBS msc = do
  self <- getSelfPid
  receiveWait
    [ match $ \(HOCommand',msc) -> send newBS (HOCommand',self) >> newbsc newBS msc
    , match $ \(Activation,newBS) -> send msc (HOAck newBS,self) >> newbsc newBS msc
    ] 

ms :: ProcessId -> Measurement -> Process ()
ms bs measurement = do
  self <- getSelfPid
  receiveTimeout 480000
    [ match $ \(LinkActive newbs,bs) -> do
  liftIO $ log bs self (LinkActive newbs)
  send newbs (LinkActReq,self)
  ms bs measurement
    , match $ \(LinkEstablished,new) -> do
  liftIO $ log new self LinkEstablished
  ms new measurement
    ]
  send bs (measurement,self) 
  ms bs measurement

bs :: ProcessId -> ProcessId -> Process () 
bs bsc ms = do
  self <- getSelfPid
  receiveWait
    [ match $ \(HOAck newbs,oldbsc) -> do
  liftIO $ log oldbsc self (HOAck newbs)
  send ms (LinkActive newbs,self)
  bs bsc ms
    , match $ \(Flush,oldbsc) -> do
  liftIO $ log oldbsc self Flush
  send oldbsc (Flush,self)
  bs bsc ms
    , match $ \(m,ms) -> do
  liftIO $ log ms self m
  -- decide whether to initiate handoff 
  send bsc (HOReq,self)
  bs bsc ms
    , match $ \(HOCommand',newbsc) -> do 
  liftIO $ log newbsc self HOCommand'
  send newbsc (Activation,self)
  bs bsc ms
    , match $ \(LinkActReq,ms) -> do
  liftIO $ log ms self LinkActReq
  send ms (LinkEstablished,self)
  send bsc (HOConnect,self)
  bs bsc ms
    ]  

log :: ProcessId -> ProcessId -> HandoffMsg -> IO ()
log from to msg = putStrLn $ show to ++ " <--- " ++ show from ++ " ---- " ++ show HandoffMsg