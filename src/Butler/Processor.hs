module Butler.Processor (
    Processor,
    withProcessor,
    awaitProcessor,
    Process (..),
    ProcessAction (..),
    getProcesses,
    getProcess,
    startProcess,
    stopProcess,

    -- * useful re-exports
    module Butler.Process,
) where

import Butler.Clock
import Butler.Events
import Butler.Logger
import Butler.NatMap as NM
import Butler.Prelude
import Butler.Process

data Processor = Processor
    { scope :: Scope
    , processes :: NatMap Process
    }

awaitProcessor :: Processor -> STM ()
awaitProcessor processor = awaitAll processor.scope

withProcessor :: (Processor -> IO a) -> IO a
withProcessor cb = scoped \scope -> do
    processor <- atomically (newProcessor scope)
    cb processor

newProcessor :: Scope -> STM Processor
newProcessor scope = Processor scope <$> newNatMap

getProcesses :: Processor -> STM [Process]
getProcesses processor = NM.elems processor.processes

getProcess :: Processor -> Pid -> STM (Maybe Process)
getProcess processor (Pid pid) = NM.lookup processor.processes pid

newtype ProcessAction = ProcessAction (Process -> IO ())

stopProcess :: Processor -> Pid -> STM (Maybe Process)
stopProcess processor (Pid pid) = do
    processM <- NM.lookup processor.processes pid
    case processM of
        Nothing -> pure Nothing
        Just process -> do
            putTMVar process.doneVar ()
            NM.delete processor.processes pid
            pure $ Just process

startProcess ::
    Clock ->
    Logger SystemEvent ->
    Processor ->
    Maybe Process ->
    ProgramName ->
    ProcessAction ->
    IO Process
startProcess clock logger processor parent program (ProcessAction action) = do
    createdAt <- getTime clock
    doneVar <- newEmptyTMVarIO
    mthread <- newEmptyTMVarIO
    mprocess <- newEmptyTMVarIO

    let parentScope = case parent of
            Just parentProcess -> parentProcess.scope
            Nothing -> processor.scope

    let createProcess :: Scope -> ThreadId -> STM Process
        createProcess scope threadId = do
            pid <- Pid <$> newKey processor.processes
            status <- newTVar Running
            thread <- readTMVar mthread
            childs <- newTVar []

            let process = Process{..}
            NM.insert processor.processes (coerce pid) process
            case parent of
                Just parentProcess -> modifyTVar' (parentProcess.childs) (process :)
                Nothing -> pure ()
            putTMVar mprocess process
            pure process

    let processTerminated :: Time -> Maybe (Either SomeException ()) -> STM ExitReason
        processTerminated now res = do
            let exitReason = case res of
                    Nothing -> Killed
                    Just (Right ()) -> Exited
                    Just (Left e) -> Crashed e

            process <- readTMVar mprocess
            writeTVar process.status $ Stopped (now, exitReason)
            addEvent logger now EventInfo (ProcessStopped process exitReason)
            pure exitReason

    -- Create a new scope
    thread <-
        parentScope `fork` scoped \scope -> do
            now <- getTime clock
            processThread <-
                scope `forkTry` do
                    scoped \processScope -> do
                        threadId <- myThreadId
                        process <- atomically do
                            process <- createProcess processScope threadId
                            addEvent logger now EventInfo (ProcessCreated process)
                            pure process
                        action process
            res <- atomically (hitman processThread doneVar)
            atomically (processTerminated now res)

    -- Write back the new ki thread so that it is part of the Process data
    atomically $ putTMVar mthread thread

    -- Wait for child thread to be running
    atomically (readTMVar mprocess)

hitman :: Thread a -> TMVar () -> STM (Maybe a)
hitman processThread doneVar = waitDoneVar <|> waitProcess
  where
    waitDoneVar = Nothing <$ readTMVar doneVar
    waitProcess = Just <$> await processThread
