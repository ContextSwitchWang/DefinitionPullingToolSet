
import Text.HTML.TagSoup

import System.Environment
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
--import System.Cmd
import System.Directory
import System.Exit
import System.IO
import Control.Applicative

import Text.HTML.TagSoup.Entity
import Text.StringLike
import Text.HTML.TagSoup.Match

openItem :: String -> IO String
openItem  = readFile

--renderOptions :: StringLike str => RenderOptions str
--renderOptions = RenderOptions escapeHTML (\x -> toString x == "br") (\x -> toString x == "script")

parse :: String -> IO ()
parse x = do
        tags <- fmap parseTags ( openItem x )
        putStr . renderTags . pro3 . pro2 . pro $ tags
    where
        pro = filter (not . tagComment ( const True))
        pro2 = dropWhile (~/= "<h3>") . dropWhile (~/= "<span id=German>" )
        pro3 = takeWhile (\x -> (x ~/=  "<hr>") && (x ~/= "<noscript>" ))

-- the standard intersperse has a strictness bug which sucks!
intersperseNotBroken :: a -> [a] -> [a]
intersperseNotBroken _ [] = []
intersperseNotBroken sep (x:xs) = x : is xs
    where
        is [] = []
        is (y:ys) = sep : y : is ys


helpMsg :: IO ()
helpMsg = putStr $ unlines $
    ["Extract From Wiktionary"
    ,""
    ] ++ map f res
    where
        width = maximum $ map (length . fst) res
        res = map g actions

        g (nam,msg,Left  _) = (nam,msg)
        g (nam,msg,Right _) = (nam ++ " <url>",msg)

        f (lhs,rhs) = "  " ++ lhs ++ replicate (4 + width - length lhs) ' ' ++ rhs
            

actions :: [(String, String, Either (IO ()) (String -> IO ()))]
actions = [("parse","Parse a web page",Right parse)
          ,("help","This help message",Left helpMsg)
          ]
main :: IO ()
main = do
    args <- getArgs
    case (args, lookup (map toLower $ head args) $ map (\(a,_,c) -> (a,c)) actions) of
        ([],_) -> helpMsg
        (x:_,Nothing) -> putStrLn ("Error: unknown command " ++ x) >> helpMsg
        ([_],Just (Left a)) -> a
        (x:xs,Just (Left a)) -> do
            putStrLn $ "Warning: expected no arguments to " ++ x ++ " but got: " ++ unwords xs
            a
        ([_,y],Just (Right a)) -> a y
        (x:xs,Just (Right _)) -> do
            putStrLn $ "Error: expected exactly one argument to " ++ x ++ " but got: " ++ unwords xs
            helpMsg
