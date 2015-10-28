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

commentStr = ["#"]
unquotedEscChar = '\\'
fdi = ["\\f"]
redi =  ["\n", "\r","\n\r"]
quotedEscChar = '\\'
quoteStartStr = ["\\Q"]
quoteDirectStartStr = ["\\q"]
quoteEndStr = ["\\d"]
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


csv :: Parser [[String]]
csv = do many (recordDeli <|> cmment)
         x <- record `sepEndBy` many1 (recordDeli <|> cmment)
         eof
         return x

record :: Parser [String]
record = (quotedField <|> field <?> "Field") `sepBy` fDeli

cmment = choice ( fmap (try . string) commentStr)
        >> manyTill anyChar (recordDeli <|> eof ) >> return () <?> "Comment"

chooseFrom f list =  choice (fmap f list)

recordDeli = void ( choice $ fmap (try . string) redi) <?> "Record Delimiter"
fDeli = void (choice $ fmap (try . string) fdi) 
        <|> lookAhead quoteDirectStart <?> "Field Delimiter"
quoteStart = void (choice $ fmap (try . (\x -> do spaces;x) . string) quoteStartStr) <?> "Quote Start Delimiter"
quoteEnd = void (choice $ fmap (try . string) quoteEndStr) 
            <|> lookAhead fDeli 
            <|> lookAhead quoteDirectStart <?> "Quote End Delimiter"
quoteDirectStart = void (choice $ fmap (try . string) quoteDirectStartStr) <?> "Quote Direct Start Delimiter"

field :: Parser String
field = do xs <- many ( parseContent ([unquotedEscChar]:redi ++ fdi) <|> parseCmd [unquotedEscChar] <?> "An unquoted Field")
           return $ concat xs

quotedField :: Parser String
--quotedField = between quoteStart quoteEnd $
--              many $ noneOf "\\" <|> try (string "\\\\" >> return '\\')

quotedField = between (quoteStart <|> quoteDirectStart) quoteEnd $
              do
                 xs <- many 
                      (parseContent 
                         ([quotedEscChar]:quoteEndStr ++ quoteDirectStartStr)
                      <|> parseCmd [quotedEscChar] <?> "An quoted Field")
                 return $ concat xs

data Options = Options  { optVerbose    :: Bool
                        , optInputArg   :: String
                        , optAltInput  :: IO String
                        , optInput      :: IO String
                        , optOutput     :: String -> IO ()
                        , optOutputPro1 :: [[String]] -> String -> [[String]]
                        , optOutputPro  :: [[String]] -> String
                        , optInputFormat:: Parser [[String]]
                        }
startOptions :: Options
startOptions = Options  { optVerbose    = False
                        , optInputArg   = []
                        , optInput      = getContents
                        , optAltInput  = return ""
                        , optOutput     = putStr
                        , optOutputPro1 = const
                        , optOutputPro  = printCSV
                        , optInputFormat= csv
}

parseContent not= many1 $ notString not

notString s =
        do
        notFollowedBy $ choice (fmap (try . string) s )
        anyChar

parseCmd ::String -> Parser String
parseCmd escStr= try(
           do
           string escStr
           cmd <- choice (fmap (string . head) cmdList)
           return (processCmd cmd)
           ) <?> "parseCmd"

processCmd c = last (concat filList)
            where filList = filter (\x -> head x == c ) cmdList

compProcess r = fmap (\x -> x ++ replicate (len - length x) "" ) r
        where len = maximum lens
              lens = fmap length r

removeTail::[[String]] -> [[String]]
removeTail r = if null ( intercalate "" (last r)) then init r else r

usingPro arg opt
            |arg == "csv" = return opt { optInputFormat = Text.CSV.csv }
            |otherwise = exitFailure

usingCmd arg opt
          |arg == "nr"  = return opt { optOutputPro = show . length }
          |arg == "src" = return opt { optOutputPro = printCSVsrc }
          |arg == "comp" = return opt { optOutputPro = optOutputPro opt . compProcess }
          |arg == "notail" = return opt { optOutputPro = optOutputPro opt . removeTail }
          |take 2 arg == "rf" = return opt { optOutputPro = getField (read (drop 2 arg)) }
          |take 1 arg == "f" = let n = read (drop 1 arg) 
                                   in return opt { optOutputPro =  optOutputPro opt . fmap (\x -> [x !! n]) }
          |take 2 arg == "up" = return opt { optOutputPro1 = changeField (read (drop 2 arg))}
          |otherwise = exitFailure

changeField list csv s = 
                    if len <= f
                         then
                            pre ++ ( rc ++ replicate (f - length rc) "" ++ [s] ):post
                         else 
                            let (front, ele:back) = splitAt f rc
                                in pre ++ (front ++ s:back):post
                    where r = head list
                          f = list !! 1
                          (pre, rc:post) = splitAt r csv
                          len = length rc

getField list csv = (csv !! r) !! f
                    where r = head list
                          f = list !! 1

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { optInputArg = arg, optInput = readFile arg })
            "FILE")
        "Input file"
 
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = writeFile arg })
            "FILE")
        "Output file, stdout by default"

    , Option "I" ["a-input"]
        (ReqArg
            (\arg opt -> return opt { optAltInput = readFile arg})
            "FILE")
        "Another Input file"
    , Option "s" ["string"]
        (ReqArg
            (\arg opt -> return opt { optInput = return arg })
            "FILE")
        "Input string"

    , Option "S" ["a-string"]
        (ReqArg
            (\arg opt -> return opt { optAltInput = return arg })
            "FILE")
        "Another Input string"
    , Option "c" ["cmd"]
        (ReqArg
             usingCmd 
            "CMD")
        "Using Command to change output"

    , Option "p" ["process"]
        (ReqArg
             usingPro 
            "PRO")
        "Using Differet processing"

    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"
 
    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitSuccess))
        "Print version"
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitSuccess))
        "Show help"
    ]
escape esc x
        | x == esc = replicate 2 esc
        | otherwise = [x]

printCSVsrc records = unlines (printRecord `map` records)
    where printRecord = intercalate  (head fdi) . map printField
          printField f = head quoteStartStr ++ concatMap (escape quotedEscChar ) f ++ head quoteEndStr 
          unlines = intercalate "\n"

printCSV records = unlines (printRecord `map` records)
    where printRecord = intercalate "," . map printField
          printField f = "\"" ++ concatMap (escape '"') f ++ "\""
          unlines = intercalate (head redi)

main =
    do
       args <- getArgs
       let (actions, nonOptions, errors) = getOpt RequireOrder options args

       -- Here we thread startOptions through all supplied option actions
       opts <- foldl (>>=) (return startOptions) actions
 
       let Options { optVerbose = verbose
                   , optInput = input
                   , optAltInput = inputAlt
                   , optInputArg = filename
                   , optOutput = output 
                   , optOutputPro = outputPro
                   , optOutputPro1 = outputPro1
                   , optInputFormat = inputFor} = opts
 
       when verbose (hPutStrLn stderr "Verbose mode")
 
       --input >>= output
       c <- input
       case parse inputFor filename c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> do when verbose (mapM_ print r)
                          altc <- inputAlt
                          output $ outputPro  $ outputPro1 r altc
