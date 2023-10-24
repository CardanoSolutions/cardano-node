{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

import Cardano.Node.LedgerEvent (
    AnchoredEvent (AnchoredEvent, ledgerEvent),
    LedgerEvent (LedgerNewEpochEvent),
    LedgerNewEpochEvent (LedgerStakeDistEvent, LedgerTotalRewards),
    foldEvent, Credential, KeyRole (Staking), StandardCrypto, EpochNo, Coin
 )
import Control.Exception (bracket, bracketOnError)
import Control.Monad (void)
import Network.Socket
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Cardano.Node.LedgerEvent as Ledger
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as B8
import qualified Cardano.Ledger.Address as Ledger
import Data.Function ((&))

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
        void $ foldEvent h mempty $ \st e -> let r = filterRewards stakeCredential st e in {-print r >>-} pure r
        foldEvent h () $ \() -> \case ae@(AnchoredEvent _ _ _ _ (LedgerNewEpochEvent (LedgerStakeDistEvent _))) -> print ae; _otherEvent -> pure ()
  where
    expectStakeCredential =
        maybe (error "invalid / missing stake address as 1st argument") return
            . parseStakeCredential

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addrInfo <- resolve
    putStrLn $ "Connecting to " <> show addrInfo
    bracket (open addrInfo) close client
  where
    resolve = do
        let hints = defaultHints{addrSocketType = Stream, addrFamily = AF_INET}
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

filterRewards
  :: Credential 'Staking StandardCrypto
  -> Map EpochNo Coin
  -> AnchoredEvent
  -> Map EpochNo Coin
filterRewards credential st = \case
  AnchoredEvent{ledgerEvent = LedgerNewEpochEvent (LedgerTotalRewards epoch rewardsMap)} ->
    let update = Map.lookup credential rewardsMap
          & maybe id (Map.insert epoch . mergeRewards)
     in update st
  _otherEvent -> st
 where
   mergeRewards = Set.foldr (<>) mempty . Set.map Ledger.rewardAmount

-- | Parse a 'Credential 'Staking' from a stake address in base16.
parseStakeCredential :: String -> Maybe (Credential 'Staking StandardCrypto)
parseStakeCredential str =
   case Hex.decode (B8.pack str) of
     Right bytes -> Ledger.getRwdCred <$> Ledger.decodeRewardAcnt bytes
     Left{} -> Nothing
