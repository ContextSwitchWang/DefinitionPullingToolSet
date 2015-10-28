import qualified Text.CSV(printCSV, csv)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv
import System.Environment
import Data.Char


main :: IO ()
main = do
    args <- getArgs
    xss <- mapM  readFile  args
    BL.putStr (Data.Csv.encode [xss])     
