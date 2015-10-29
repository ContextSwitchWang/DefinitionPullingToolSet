import qualified Text.CSV(printCSV, csv)
import qualified Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector
import Text.Parsec.Prim(ParsecT,Stream)
import Text.ParserCombinators.Parsec
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List
import Data.Char
import Control.Monad
import Text.Printf

baseStr = "<base href=\"https://en.wiktionary.org/\">"

addBase x = head x:[ baseStr ++ (x !! 1)]  
main = do
        args <- getArgs
        files <- mapM BL.readFile args
        case Data.Csv.decode Data.Csv.NoHeader (head files) of
            Left  _ -> putStrLn "Error"
            Right c ->
               BL.putStr  $ Data.Csv.encode $ Data.Vector.toList  $ fmap addBase c 
