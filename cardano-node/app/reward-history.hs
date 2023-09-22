import Cardano.Node.LedgerEvent (foldEvent, filterRewards, parseStakeCredential)
import System.Environment (getArgs)
import System.IO (stdin, IOMode(ReadMode))
import Text.Pretty.Simple (pPrint)
import Network.Socket

-- Usage: rewards-history <<<stdin LEDGER-EVENTS] <STAKE-ADDRESS>
--
-- Example:
--
--   cat ledger_events.cbor | rewards-history "e04cf4f01890215bd181d1fcd3c9589a2a4a3adbcff1a70b748080fa82"
main :: IO ()
main = do
  stakeCredential <- getArgs >>= expectStakeCredential . head
  addrInfo <- resolve
  putStrLn $ "connecting to " <> show addrInfo
  sock <- openSocket addrInfo
  connect sock $ addrAddress addrInfo
  h <- socketToHandle sock ReadMode
  
  history <- foldEvent (\st -> pure . filterRewards stakeCredential st) mempty h
  pPrint history
 where
  resolve = do
        let hints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
        head <$> getAddrInfo (Just hints) (Just "localhost") (Just "9999")
  expectStakeCredential =
    maybe (error "invalid / missing stake address as 1st argument") return
    .
    parseStakeCredential
