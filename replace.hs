import qualified Text.CSV(printCSV, csv)
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

unquotedEscChar = '\\'
-- one after esc char is replaced
cmdList = 
        [
            ["u", "ü"]
        ,   ["o", "ö"]
        ,   ["a", "ä"]
        ,   ["U", "Ü"]
        ,   ["O", "Ö"]
        ,   ["A", "Ä"]
        ,   ["s", "ß"]

        ,   ["1", "⸚"]
        ,   ["\\","\\"]
        ,   ["#", "#"]
        ]

parseCmd ::String -> Parser String
parseCmd escStr= try(
           do
           string escStr
           cmd <- choice (fmap (string . head) cmdList)
           return (processCmd cmd)
           ) <?> "parseCmd"

processCmd c = last (concat filList)
            where filList = filter (\x -> head x == c ) cmdList

field :: Parser String
field = do xs <- many ( parseContent [[unquotedEscChar]] <|> parseCmd [unquotedEscChar] <?> "An unquoted Field")
           return $ concat xs

parseContent not= many1 $ notString not

notString s =
        do
        notFollowedBy $ choice (fmap (try . string) s )
        anyChar

parseString f c = 
        case parse field f c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r ->  putStr r


main = do
        args <- getArgs
        mapM (parseString "" ) args 
