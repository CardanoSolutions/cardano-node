{-# LANGUAGE LambdaCase #-}

import Cardano.Node.LedgerEvent (parseStakeCredential, foldEvent, filterRewards, AnchoredEvent (AnchoredEvent), LedgerEvent (LedgerNewEpochEvent), LedgerNewEpochEvent(LedgerStakeDistEvent))
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode))
import Network.Socket
import Control.Exception (bracket, bracketOnError)

-- Usage: reward-history <<<stdin LEDGER-EVENTS] <STAKE-ADDRESS>
--
-- Example:
--
--   cat ledger_events.cbor | rewards-history "e04cf4f01890215bd181d1fcd3c9589a2a4a3adbcff1a70b748080fa82"
main :: IO ()
main = do
  stakeCredential <- getArgs >>= expectStakeCredential . head
  print $ "Got stake credential: " ++ show stakeCredential

  runTCPClient "localhost" "9999" $ \sock -> do
    h <- socketToHandle sock ReadMode

    putStrLn "Getting reward history..."
    -- void $ foldEvent h mempty $ \st e -> let r = filterRewards stakeCredential st e in print r >> pure r
    -- void $ foldEvent h mempty $ \st e -> let r = filterRewards stakeCredential st e in print r >> pure r
    foldEvent h () $ \() -> \case AnchoredEvent _ _ (LedgerNewEpochEvent (LedgerStakeDistEvent e)) -> print e; _otherEvent -> pure ()
 where
  expectStakeCredential =
    maybe (error "invalid / missing stake address as 1st argument") return
    .
    parseStakeCredential

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addrInfo <- resolve
    putStrLn $ "Connecting to " <> show addrInfo
    bracket (open addrInfo) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock
