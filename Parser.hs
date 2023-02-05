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


data Ctx = Root | L Ctx

restore :: Regex -> Ctx -> Regex -> Regex
restore _ Root re = re
restore (Or  l r) (L path) re = Or  (restore l path re) r
restore (Cat l r) (L path) re = Cat (restore l path re) r
restore _ _ _ = Eps -- shouldn't take place


-- main = print $ parse "abcdefghi"
-- main = print $ parse "a|b"
-- main = print $ parse "x|"
-- main = print $ parse "|d"
-- main = print $ parse "x|d"
-- main = print $ parse "a*|(|b(x|d|p\\w)c\\d)?d+()|e*"
main = print $ parse "a*|\\((<NAMEK>|b(<G>x|d|p\\w)c\\d)?d+(<NAMEK>)(<G>)?|e*"
-- main = print $ parse "a*(b(x)c)?d+()"
-- main = print $ parse "aa|bbb|cc|dd"


type IntermResult = (Regex, String, GroupEnv, Regex, Ctx)

specials :: [Char]
specials =  ['(', ')', '[', ']', '{', '}', '|', '*', '+', '?', '\\', '.']

setOfSpecials :: Set Char
setOfSpecials =  Set.fromList specials

parse :: String -> Maybe Regex
parse s = case auxParse True grpEnvEmpty s of
    Just (re, _, _, root, ctx) -> return . simpl $ restore root ctx re
    Nothing -> empty

auxParse :: Bool -> GroupEnv -> String -> Maybe IntermResult
auxParse failhard env "" = Just (Eps, "", env, Eps, Root)
auxParse failhard env s  = case parseAtom failhard env s of
    Just (re, s', env', _, Root) -> do
        (re', s'', env'', root', ctx') <- auxParse failhard env' s'
        return (Cat re re', s'', env'', root', ctx')
    Just (re, s', env', root, ctx@(L _)) -> do
        (re', s'', env'', root', ctx') <- auxParse failhard env' s'
        let re''   = restore root' ctx' re'
        let root'' = Cat root re''
        return (re, s'', env'', root'', L ctx)
    Nothing -> if failhard then empty else return (Eps, s, env, Eps, Root)

parseAtom :: Bool -> GroupEnv -> String -> Maybe IntermResult
parseAtom failhard env = quantity . asum . ([plainOrEscd, group, disjunction failhard] <&> ($ env) <&>) . flip ($)

quantity :: Maybe IntermResult -> Maybe IntermResult
quantity (Just (re, s, env, root, ctx)) = return (re', s', env, root, ctx)
    where
        (re', s') = case s of
            '*':s' -> (Star re,           s')
            '+':s' -> (Cat  re (Star re), s')
            '?':s' -> (Or   Eps re,       s')
            _      -> (re,                s)
quantity Nothing = empty

disjunction :: Bool -> GroupEnv -> String -> Maybe IntermResult
disjunction failhard env ('|':s) = do
    (re, s', env', root, ctx) <- auxParse failhard env s
    return (Eps, s', env', Or Eps (restore root ctx re), L Root)
disjunction _ _ _ = empty

plainOrEscd :: GroupEnv -> String -> Maybe IntermResult
plainOrEscd env ('\\':c:s) = do
    pred <- predFromChar True c
    let re = Lit ('\\':[c]) pred
    return (re, s, env, re, Root)
plainOrEscd env (c:s) = do
    pred <- predFromChar False c
    let re = Lit [c] pred
    return (re, s, env, re, Root)
plainOrEscd _ _ = empty

predFromChar :: Bool -> Char -> Maybe (Char -> Bool)
predFromChar escaped c = do
        c2p <- Map.lookup escaped table
        Map.lookup c c2p <|> if not escaped && Set.notMember c setOfSpecials
                                then return (== c)
                             else empty
    where
        table = Map.fromList . zip [True, False] $ [yesc'd, nesc'd] <&> Map.fromList
        yesc'd = ([('w', isLetter),
                   ('d', isDigit),
                   ('s', isSpace)] >>= \(c, p) -> [(c, p), (toUpper c, not . p)])
                ++ [('n', (== '\n')), ('t', (== '\t'))]
                ++ [(c, (== c)) | c <- specials]
        nesc'd = [('.', const True)]

group :: GroupEnv -> String -> Maybe IntermResult
group env ('(':s) = case name of
    "" -> case auxParse False env s' of
        Just (re, ')':s'', env', root, ctx) -> let gr = Group "" (restore root ctx re) in
            return (gr, s'', env', gr, Root)
        _ -> empty
    _  -> case s' of
        (')':s'') -> do
            gr <- grpEnvGet env name
            return (gr, s'', env, gr, Root)
        _ -> case auxParse False env s' of
            Just (re, ')':s'', env', root, ctx) -> case grpEnvGet env' name of
                Nothing -> let gr = Group name (restore root ctx re) in
                    return (gr, s'', fromJust $ grpEnvAdd env' name gr, gr, Root)
                Just _  -> empty
            _ -> empty
    where
        (name, s') = case parseName s of
            Just (name', s'') -> (name', s'')
            Nothing           -> ("",    s)

        parseName ('<':s)     = auxParseName s
        parseName _           = empty

        auxParseName ('>':s) = return ("", s)
        auxParseName (c:s)   = case auxParseName s of
            Just (name, s') -> return (c:name, s')
            Nothing         -> empty
        auxParseName []      = empty
group _ _ = empty
