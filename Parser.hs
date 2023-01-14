import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Tuple


-- implement pretty-printing, Eq
data Regex
    = Eps
    | Lit  (Char -> Bool)
    | Or   Regex Regex
    | Cat  Regex Regex
    | Star Regex

main = return ()

parse :: String -> Maybe Regex
parse s = case auxparse True Eps s of
    Nothing      -> Nothing
    Just (re, _) -> Just re

-- fold-analogous
auxparse :: Bool -> Regex -> String -> Maybe (Regex, String)
auxparse _ acc [] = Just (acc, [])
auxparse failhard acc s = case multiplicity $ alphanum s <|> group s of
    Nothing       -> if failhard then Nothing else Just (acc, s)
    Just (re, s') -> auxparse failhard (Cat acc re) s'

group :: String -> Maybe (Regex, String)
group ('(':s) = case auxparse False Eps s of
    Just (re, (')':s')) -> Just (re, s')
    _  -> Nothing
group _ = Nothing

alphanum :: String -> Maybe (Regex, String)
alphanum (c:s) | isAlphaNum c = Just (Lit (== c), s)
               | otherwise    = Nothing
alphanum _ = Nothing

multiplicity :: Maybe (Regex, String) -> Maybe (Regex, String)
multiplicity (Just (re, ('*':s))) = Just (Star re, s)
multiplicity (Just (re, ('+':s))) = Just (Cat re $ Star re, s)
multiplicity (Just (re, ('?':s))) = Just (Or Eps re, s)
multiplicity x = x
