import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Tuple


data Regex
    = Eps
    | Lit   String (Char -> Bool) -- String only for development purposes
    | Or    Regex Regex
    | Cat   Regex Regex
    | Star  Regex
    | Group Int (Maybe String) Regex -- to-be Nat instead of Int

instance Eq Regex where
    Eps            == Eps            = True
    Lit   s  _     == Lit   t  _     =  s == t
    Or    ll rl    == Or    lr rr    = ll == lr && rl == rr
    Cat   ll rl    == Cat   lr rr    = ll == lr && rl == rr
    Star  re       == Star  rf       = re == rf
    Group n  lb re == Group m  lc rf = lb == lc && re == rf
    _              == _              = False

instance Show Regex where
    show Eps          = "Eps"
    show (Lit   s  _) = "Lit \"" ++ s ++ "\" <pred>"
    show (Or    l  r) = "Or ("  ++ show l ++ ") (" ++ show r ++ ")"
    show (Cat   l  r) = "Cat (" ++ show l ++ ") (" ++ show r ++ ")"
    show (Star  re)   = "Star (" ++ show re ++ ")"
    show (Group n  Nothing  re) = "Group " ++ show n ++ " <nameless> (" ++ show re ++ ")"
    show (Group n (Just lb) re) = "Group " ++ show n ++ " \"" ++ lb ++ "\" (" ++ show re ++ ")"

main = putStrLn . show $ parse "a*(bc)?d+"

parse :: String -> Maybe Regex
parse s = case auxparse True Eps s of
    Nothing      -> Nothing
    Just (re, _) -> Just re

-- fold-analogous
auxparse :: Bool -> Regex -> String -> Maybe (Regex, String)
auxparse _        acc [] = Just (acc, [])
auxparse failhard acc s  = case subparse s of
    Nothing       -> if failhard then Nothing else Just (acc, s)
    Just (re, s') -> auxparse failhard (Cat acc re) s'

subparse :: String -> Maybe (Regex, String)
subparse [] = Just (Eps, [])
subparse s  = multiplicity . foldr (<|>) Nothing $ ($ s) <$> [alphanum, group]

group :: String -> Maybe (Regex, String)
group ('(':s) = case auxparse False Eps s of
    Just (re, ')':s') -> Just (Group 0 Nothing re, s')
    _                 -> Nothing
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
