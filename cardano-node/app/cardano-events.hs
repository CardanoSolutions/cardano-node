import System.Environment
import Cardano.Node.LedgerEvent (foldEvent, filterRewards)
import System.IO(stdin)

main :: IO ()
main = foldEvent filterRewards mempty stdin
