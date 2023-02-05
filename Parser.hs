import Control.Applicative
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple


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
    show = showStruct
    -- show = stringify
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

simpl :: Regex -> Regex
simpl (Or    re  rf)  = Or   (simpl re) (simpl rf)
simpl (Cat   Eps re)  = simpl re
simpl (Cat   re  Eps) = simpl re
simpl (Cat   re  rf)  = Cat  (simpl re) (simpl rf)
simpl (Star  re)      = Star (simpl re)
simpl (Group lb  re)  = Group lb (simpl re)
simpl re              = re


type GroupEnv = Map String Regex

grpEnvAdd :: GroupEnv -> String -> Regex -> Maybe GroupEnv
grpEnvAdd env name g@(Group {}) = Just (Map.insert name g env)
grpEnvAdd _ _ _ = Nothing

grpEnvGet :: GroupEnv -> String -> Maybe Regex
grpEnvGet = flip Map.lookup

grpEnvEmpty :: GroupEnv
grpEnvEmpty = Map.empty


-- main = print $ parse "abcdefghi"
-- main = print $ parse "a|b"
-- main = print $ parse "x|"
-- main = print $ parse "|d"
-- main = print $ parse "x|d"
-- main = print $ parse "a*|(|b(x|d|p\\w)c\\d)?d+()|e*"
main = print $ parse "a*|\\((<NAMEK>|b(x|d|p\\w)c\\d)?d+()|e*"
-- main = print $ parse "a*(b(x)c)?d+()"


specials :: [Char]
specials =  ['(', ')', '[', ']', '{', '}', '|', '*', '+', '?', '\\', '.']

setOfSpecials :: Set Char
setOfSpecials =  Set.fromList specials

parse :: String -> Maybe Regex
parse s = case auxParse True grpEnvEmpty s of
    Just (re, glue, _, _) -> Just . simpl $ Eps `glue` re
    Nothing -> Nothing

auxParse :: Bool -> GroupEnv -> String -> Maybe (Regex, Regex -> Regex -> Regex, String, GroupEnv)
auxParse failhard env "" = Just (Eps, Cat, [], env)
auxParse failhard env s  = case parseAtom failhard env s of
    Just (re, glue, s', env') -> do
        (re', glue', s'', env'') <- auxParse failhard env' s'
        return (re `glue'` re', glue, s'', env'')
    Nothing -> if failhard then Nothing else Just (Eps, Cat, s, env)

parseAtom :: Bool -> GroupEnv -> String -> Maybe (Regex, Regex -> Regex -> Regex, String, GroupEnv)
parseAtom failhard env = quantity . asum . ([group, plainOrEscd, disjunction failhard] <&> ($ env) <&>) . flip ($)

quantity :: Maybe (Regex, Regex -> Regex -> Regex, String, GroupEnv) -> Maybe (Regex, Regex -> Regex -> Regex, String, GroupEnv)
quantity (Just (re, glue, s, env)) = case s of
    '*':s' -> Just (Star re,           glue, s', env)
    '+':s' -> Just (Cat  re (Star re), glue, s', env)
    '?':s' -> Just (Or   Eps re,       glue, s', env)
    s'     -> Just (re,                glue, s', env)
quantity Nothing = Nothing

disjunction :: Bool -> GroupEnv -> String -> Maybe (Regex, Regex -> Regex -> Regex, String, GroupEnv)
disjunction failhard env ('|':s) = case auxParse failhard env s of 
    Just (re, glue, s', env') -> Just (Eps `glue` re, Or, s', env')
    Nothing    -> Nothing
disjunction _ _ _ = Nothing

predFromChar :: Bool -> Char -> Maybe (Char -> Bool)
predFromChar escaped c = do
        c2p <- Map.lookup escaped table
        Map.lookup c c2p <|> if not escaped && Set.notMember c setOfSpecials
                                then Just (== c)
                             else Nothing
    where
        table = Map.fromList . zip [True, False] $ [yesc'd, nesc'd] <&> Map.fromList
        yesc'd = ([('w', isLetter),
                   ('d', isDigit),
                   ('s', isSpace)] >>= \(c, p) -> [(c, p), (toUpper c, not . p)])
                ++ [('n', (== '\n')), ('t', (== '\t'))]
                ++ [(c, (== c)) | c <- specials]
        nesc'd = [('.', const True)]

plainOrEscd :: GroupEnv -> String -> Maybe (Regex, Regex -> Regex -> Regex, String, GroupEnv)
plainOrEscd env ('\\':c:s) = case predFromChar True c of
    Just pred  -> Just (Lit ('\\':[c]) pred, Cat, s, env)
    Nothing    -> Nothing
plainOrEscd env (c:s) = case predFromChar False c of
    Just pred  -> Just (Lit [c] pred, Cat, s, env)
    Nothing    -> Nothing
plainOrEscd _ _ = Nothing

group :: GroupEnv -> String -> Maybe (Regex, Regex -> Regex -> Regex, String, GroupEnv)
group env ('(':s) = case name of
    "" -> case auxParse False env s' of
        Just (re, glue, ')':s'', env') -> Just (Eps `glue` Group "" re, Cat, s'', env')
        _ -> Nothing
    _  -> case s' of
        (')':s'') -> do
            gr <- grpEnvGet env name
            Just (gr, Cat, s'', env)
        _ -> case auxParse False env s' of
            Just (re, glue, ')':s'', env') -> case grpEnvGet env' name of
                Nothing -> let gr = Group name re in Just (Eps `glue` gr, Cat, s'', fromJust $ grpEnvAdd env' name gr)
                Just _  -> Nothing
            _ -> Nothing
    where
        (name, s') = case parseName s of
            Just (name', s'') -> (name', s'')
            Nothing           -> ("",    s)

        parseName ('<':s)     = auxParseName s
        parseName _           = Nothing

        auxParseName ('>':s) = Just ("", s)
        auxParseName (c:s)   = case auxParseName s of
            Just (name, s') -> Just (c:name, s')
            Nothing         -> Nothing
        auxParseName []      = Nothing
group _ _ = Nothing
