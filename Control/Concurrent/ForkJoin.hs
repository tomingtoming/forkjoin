module Control.Concurrent.ForkJoin(
  Thread, Status,
  fork, status, join
)where

import Control.Concurrent
import Control.Concurrent.STM
import Data.List (delete)

data Status = Running | Done
instance Show Status where
  show Running = "Running"
  show Done    = "Done"

data Thread = Thread ThreadId (TVar Status)
instance Show Thread where
  show (Thread id _) = "Thread(" ++ show id ++ ")"
instance Eq Thread where
  (Thread id1 _) == (Thread id2 _) = id1 == id2

fork :: IO () -> IO Thread
fork io = do
  sts <- newTVarIO Running
  tid <- forkIO $ do
    io
    atomically $ modifyTVar sts (const Done)
  return $ Thread tid sts

status :: Thread -> IO Status
status (Thread _ sts) = readTVarIO sts

join :: Thread -> IO ()
join (Thread _ sts) = atomically $ do
  status <- readTVar sts
  case status of
    Running -> retry
    Done    -> return ()
