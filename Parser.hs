import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Tuple


data Regex
    = Eps
    | Lit  String (Char -> Bool) -- String only for development purposes
    | Or   Regex Regex
    | Cat  Regex Regex
    | Star Regex

instance Eq Regex where
    Eps        == Eps        = True
    Lit  s  _  == Lit  t  _  =  s == t
    Or   ll rl == Or   lr rr = ll == lr && rl == rr
    Cat  ll rl == Cat  lr rr = ll == lr && rl == rr
    Star re    == Star sf    = re == sf
    _          == _          = False

instance Show Regex where
    show Eps        = "Eps"
    show (Lit  s _) = "Lit " ++ s ++ " <pred>"
    show (Or   l r) = "Or ("  ++ show l ++ ") (" ++ show r ++ ")"
    show (Cat  l r) = "Cat (" ++ show l ++ ") (" ++ show r ++ ")"
    show (Star re)  = "Star (" ++ show re ++ ")"

main = putStrLn . show $ parse "abcd"

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
    Just (re, ')':s') -> Just (re, s')
    _  -> Nothing
group _ = Nothing

alphanum :: String -> Maybe (Regex, String)
alphanum (c:s) | isAlphaNum c = Just (Lit [c] (== c), s)
               | otherwise    = Nothing
alphanum _ = Nothing

multiplicity :: Maybe (Regex, String) -> Maybe (Regex, String)
multiplicity (Just (re, '*':s)) = Just (Star re, s)
multiplicity (Just (re, '+':s)) = Just (Cat re $ Star re, s)
multiplicity (Just (re, '?':s)) = Just (Or Eps re, s)
multiplicity x = x
