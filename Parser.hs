import Control.Applicative
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Tuple
import qualified Data.Map as Map


data Regex
    = Eps
    | Lit   String (Char -> Bool) -- String only for development purposes
    | Or    Regex  Regex
    | Cat   Regex  Regex
    | Star  Regex
    | Group String Regex

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
            showStruct Eps           = "Eps"
            showStruct (Lit   s  _)  = "Lit \"" ++ s ++ "\" <pred>"
            showStruct (Or    l  r)  = "Or ("  ++ show l ++ ") (" ++ show r ++ ")"
            showStruct (Cat   l  r)  = "Cat (" ++ show l ++ ") (" ++ show r ++ ")"
            showStruct (Star  re)    = "Star (" ++ show re ++ ")"
            showStruct (Group lb re) = "Group \"" ++ lb ++ "\" (" ++ show re ++ ")"

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
            stringify (Group [] re) = "("               ++ show re ++ ")"
            stringify (Group lb re) = "(<" ++ lb ++ ">" ++ show re ++ ")"


-- main = print $ parse "abcdefghi"
-- main = print $ parse "a|b"
-- main = print $ parse "x|"
-- main = print $ parse "|d"
-- main = print $ parse "x|d"
-- main = print $ parse "a*|(|b(x|d|p\\w)c\\d)?d+()|e*"
main = print $ parse "a*|(<NAMEK>|b(x|d|p\\w)c\\d)?d+()|e*"
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
    Nothing -> Nothing

auxparse :: Bool -> String -> Maybe (Regex, Regex -> Regex -> Regex, String)
auxparse failhard [] = Just (Eps, Cat, [])
auxparse failhard s  = case parseAtom failhard s of
    Just (re, glue, s') -> do
        (re', glue', s'') <- auxparse failhard s'
        return (re `glue'` re', glue, s'')
    Nothing -> if failhard then Nothing else Just (Eps, Cat, s)

parseAtom :: Bool -> String -> Maybe (Regex, Regex -> Regex -> Regex, String)
parseAtom failhard = quantity . asum . ([alphanum, group, escaped, disjunction failhard] <&>) . flip ($)

quantity :: Maybe (Regex, Regex -> Regex -> Regex, String) -> Maybe (Regex, Regex -> Regex -> Regex, String)
quantity (Just (re, f, s)) = case s of
    '*':s' -> Just (Star re, f, s')
    '+':s' -> Just (Cat re (Star re), f, s')
    '?':s' -> Just (Or Eps re, f, s')
    s'     -> Just (re, f, s')
quantity Nothing = Nothing

disjunction :: Bool -> String -> Maybe (Regex, Regex -> Regex -> Regex, String)
disjunction failhard ('|':s) = case auxparse failhard s of 
    Just (re, glue, s') -> Just (Eps `glue` re, Or, s')
    Nothing    -> Nothing
disjunction _ _ = Nothing

predFromChar :: Bool -> Char -> Maybe (Char -> Bool)
predFromChar escaped c = do
        c2p  <- Map.lookup escaped table
        Map.lookup c c2p <|> (if not escaped then Just (== c) else Nothing)
    where
        table = Map.fromList . zip [True, False] $ [yescd, nescd] <&> Map.fromList
        yescd = ([
                ('w', isLetter),
                ('d', isDigit),
                ('s', isSpace)
            ] >>= \(c, p) -> [
                (c, p), (toUpper c, not . p)
            ]) ++ [
                ('n', (== '\n')),
                ('t', (== '\t'))
            ] ++ [
                (c, (== c)) | c <- [ '(', ')', '[', ']', '{', '}', '|', '*', '+', '?', '\\', '.' ]
            ]
        nescd = [('.', const True)]

escaped :: String -> Maybe (Regex, Regex -> Regex -> Regex, String)
escaped ('\\':c:s) = case maybePred of
    Just pred -> Just (Lit ('\\':[c]) (neg . pred), Cat, s)
    Nothing   -> Nothing
    where
        -- could be prettier
        maybePred = case toLower c of
            '\\' -> Just (== c)
            '*'  -> Just (== c)
            '?'  -> Just (== c)
            -- ...
            'w'  -> Just isLetter
            'd'  -> Just isDigit
            's'  -> Just isSpace
            _    -> Nothing
        neg | isUpper c = not
            | otherwise = id
escaped _ = Nothing

alphanum :: String -> Maybe (Regex, Regex -> Regex -> Regex, String)
alphanum (c:s) | isAlphaNum c = Just (Lit [c] (== c), Cat, s)
               | otherwise    = Nothing
alphanum _ = Nothing

group :: String -> Maybe (Regex, Regex -> Regex -> Regex, String)
group ('(':s) = case auxparse False s' of
    Just (re, glue, ')':s'') -> Just (Eps `glue` Group name re, Cat, s'')
    _ -> Nothing
    where
        (name, s') = case parseName s of
            Just (name', s'') -> (name', s'')
            Nothing           -> ("",    s)

        parseName ('<':s) = auxParseName s
        parseName _       = Nothing

        auxParseName ('>':s) = Just ("", s)
        auxParseName (c:s)   = case auxParseName s of
            Just (name, s') -> Just (c:name, s')
            Nothing         -> Nothing
        auxParseName []      = Nothing
group _ = Nothing
