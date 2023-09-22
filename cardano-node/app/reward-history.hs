import Cardano.Node.LedgerEvent (foldEvent, filterRewards, parseStakeCredential)
import System.Environment (getArgs)
import System.IO (stdin)
import Text.Pretty.Simple (pPrint)

-- Usage: rewards-history <<<stdin LEDGER-EVENTS] <STAKE-ADDRESS>
--
-- Example:
--
--   cat ledger_events.cbor | rewards-history "e04cf4f01890215bd181d1fcd3c9589a2a4a3adbcff1a70b748080fa82"
main :: IO ()
main = do
  stakeCredential <- getArgs >>= expectStakeCredential . head
  history <- foldEvent (\st -> pure . filterRewards stakeCredential st) mempty stdin
  pPrint history
 where
  expectStakeCredential =
    maybe (error "invalid / missing stake address as 1st argument") return
    .
    parseStakeCredential
