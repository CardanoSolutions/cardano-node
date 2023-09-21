import System.Environment
import Cardano.Node.LedgerEvent (tailEvent)

main :: IO ()
main = getArgs >>= tailEvent . head
