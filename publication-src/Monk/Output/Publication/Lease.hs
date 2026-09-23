-- | Revocable scope ownership for IO actions that can escape rank-n callbacks.
module Monk.Output.Publication.Lease
  ( Lease,
    newLease,
    withLease,
    revokeLease,
  )
where

import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as Exception
import System.IO.Error (userError)

-- Closing prevents new claims before waiting for current operations to finish.
-- Keeping a claim through the entire IO action closes the check/use race.
newtype Lease = Lease (STM.TVar (Bool, Int))

newLease :: IO Lease
newLease = Lease <$> STM.newTVarIO (True, 0)

withLease :: Lease -> IO a -> IO a
withLease (Lease leaseState) = Exception.bracket_ claim release
  where
    claim = STM.atomically $ do
      (live, active) <- STM.readTVar leaseState
      unless live (STM.throwSTM (userError "publication lease expired"))
      STM.writeTVar leaseState (live, active + 1)
    release = STM.atomically $ STM.modifyTVar' leaseState (\(live, active) -> (live, active - 1))

revokeLease :: Lease -> IO ()
revokeLease (Lease leaseState) = Exception.uninterruptibleMask_ $ do
  STM.atomically $ STM.modifyTVar' leaseState (\(_, active) -> (False, active))
  -- Resource destruction must not overtake an admitted operation, even when
  -- another exception arrives during bracket cleanup. Each operation releases
  -- its claim on exceptions and cancellation through withLease's bracket.
  STM.atomically $ do
    (_, active) <- STM.readTVar leaseState
    STM.check (active == 0)
