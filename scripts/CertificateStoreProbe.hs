-- Isolate trust-store loading without DNS, sockets, TLS or credentials.
import Control.Exception (evaluate)
import Control.Monad (replicateM_)
import Data.X509.CertificateStore (listCertificates)
import GHC.Clock (getMonotonicTimeNSec)
import System.X509 (getSystemCertificateStore)

main :: IO ()
main = replicateM_ 3 $ do
    start <- getMonotonicTimeNSec
    store <- getSystemCertificateStore
    count <- evaluate (length (listCertificates store))
    end <- getMonotonicTimeNSec
    print (count, fromIntegral (end - start) / 1000000 :: Double)
