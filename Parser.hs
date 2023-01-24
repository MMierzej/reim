import Control.Applicative
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Tuple


data Regex
    = Eps
    | Lit   String (Char -> Bool) -- String only for development purposes
    | Or    Regex  Regex
    | Cat   Regex  Regex
    | Star  Regex
    | Group (Maybe String) Regex

instance Eq Regex where
    Eps         == Eps         = True
    Lit   s  _  == Lit   t  _  =  s == t
    Or    ll rl == Or    lr rr = ll == lr && rl == rr
    Cat   ll rl == Cat   lr rr = ll == lr && rl == rr
    Star  re    == Star  rf    = re == rf
    Group lb re == Group lc rf = lb == lc && re == rf
    _           == _           = False

instance Show Regex where
    -- show = showStruct
    show = stringify
        where
            showStruct Eps          = "Eps"
            showStruct (Lit   s  _) = "Lit \"" ++ s ++ "\" <pred>"
            showStruct (Or    l  r) = "Or ("  ++ show l ++ ") (" ++ show r ++ ")"
            showStruct (Cat   l  r) = "Cat (" ++ show l ++ ") (" ++ show r ++ ")"
            showStruct (Star  re)   = "Star (" ++ show re ++ ")"
            showStruct (Group Nothing   re) = "Group " ++ " <nameless> ("       ++ show re ++ ")"
            showStruct (Group (Just lb) re) = "Group " ++ " \"" ++ lb ++ "\" (" ++ show re ++ ")"

            stringify Eps           = ""
            stringify (Lit  s   _)  = s
            stringify (Or   re Eps) = show $ Or Eps re
            stringify (Or   Eps re) = case re of
                Group {} ->        show re ++  "?"
                _        -> "(" ++ show re ++ ")?"
            stringify (Or   l   r)  = show l ++ "|" ++ show r
            stringify (Cat  re  (Star rf)) | re == rf = show re ++ "+"
            stringify (Cat  l   r)  = show l ++ show r
            stringify (Star re)     = show re ++ "*"
            stringify (Group  Nothing  re) =               "(" ++ show re ++ ")"
            stringify (Group (Just lb) re) = "`" ++ lb ++ "`(" ++ show re ++ ")"


-- main = print $ parse "abcdefghi"
-- main = print $ parse "a|b"
-- main = print $ parse "x|"
-- main = print $ parse "|d"
-- main = print $ parse "x|d"
main = print $ parse "a*|(|b(x|d|p)c)?d+()|e*"
-- main = print $ parse "a*(b(x)c)?d+()"

simpl :: Regex -> Regex
simpl (Or    re  rf)  = Or   (simpl re) (simpl rf)
simpl (Cat   Eps re)  = simpl re
simpl (Cat   re  Eps) = simpl re
simpl (Cat   re  rf)  = Cat  (simpl re) (simpl rf)
simpl (Star  re)      = Star (simpl re)
simpl (Group lb  re)  = Group lb (simpl re)
simpl re              = re

parse :: String -> Maybe Regex
parse s = case auxparse True s of
    Just (re, glue, _) -> Just . simpl $ Eps `glue` re
    Nothing      -> Nothing

auxparse :: Bool -> String -> Maybe (Regex, Regex -> Regex -> Regex, String)
auxparse failhard [] = Just (Eps, Cat, [])
auxparse failhard s  = case parseAtom failhard s of
    Just (re, glue, s') -> (\(re', glue', s'') -> (re `glue'` re', glue, s'')) <$> auxparse failhard s'
    Nothing -> if failhard then Nothing else Just (Eps, Cat, s)

parseAtom :: Bool -> String -> Maybe (Regex, Regex -> Regex -> Regex, String)
parseAtom failhard s = multiplicity . asum $ ($ s) <$> [alphanum, group, alt failhard]

multiplicity :: Maybe (Regex, Regex -> Regex -> Regex, String) -> Maybe (Regex, Regex -> Regex -> Regex, String)
multiplicity (Just (re, f, s)) = case s of
    '*':s' -> Just (Star re, f, s')
    '+':s' -> Just (Cat re (Star re), f, s')
    '?':s' -> Just (Or Eps re, f, s')
    s'     -> Just (re, f, s')
multiplicity Nothing = Nothing

alt :: Bool -> String -> Maybe (Regex, Regex -> Regex -> Regex, String)
alt failhard ('|':s) = case auxparse failhard s of 
    Just (re, glue, s') -> Just (Eps `glue` re, Or, s')
    Nothing -> Nothing
alt _ _      = Nothing

alphanum :: String -> Maybe (Regex, Regex -> Regex -> Regex, String)
alphanum (c:s) | isAlphaNum c = Just (Lit [c] (== c), Cat, s)
               | otherwise    = Nothing
alphanum _ = Nothing

group :: String -> Maybe (Regex, Regex -> Regex -> Regex, String)
group ('(':s) = case auxparse False s of
    Just (re, glue, ')':s') -> Just (Eps `glue` Group Nothing re, Cat, s')
    _  -> Nothing
group _ = Nothing
