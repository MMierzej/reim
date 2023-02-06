{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- module Parser where


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
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote


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
grpEnvAdd env name g@(Group {}) = return (Map.insert name g env)
grpEnvAdd _ _ _ = empty

grpEnvGet :: GroupEnv -> String -> Maybe Regex
grpEnvGet = flip Map.lookup

grpEnvEmpty :: GroupEnv
grpEnvEmpty = Map.empty


data Path
    = Root
    | L    Regex Path
    | R    Regex Path

instance Semigroup Path where
    Root        <> tail = tail
    (L re path) <> tail = L re (path <> tail)
    (R re path) <> tail = R re (path <> tail)


type Ctx = (Regex, Path)

restoRe :: Ctx -> Regex
restoRe (re, Root) = re
restoRe (re, (L parent path')) = case parent of
    (Or  _ r) -> restoRe (Or  re r, path')
    (Cat _ r) -> restoRe (Cat re r, path')
restoRe (re, (R parent path')) = case parent of
    (Or  l _) -> restoRe (Or  l re, path')
    (Cat l _) -> restoRe (Cat l re, path')

-- main = print $ parse "abcdefghi"
-- main = print $ parse "a|b"
-- main = print $ parse "x|"
-- main = print $ parse "|d"
-- main = print $ parse "x|d"
-- main = print $ parse "a*|(|b(x|d|p\\w)c\\d)?d+()|e*"
main = print $ parse "a*|\\((<NAMEK>|b(<G>x|d(<XD>DDD)d|p\\w)c\\d(<XD>))?d+((<XD>)(<G>))*(<NAMEK>)(<G>)?|e*"
-- main = print $ parse "a*(b(x)c)?d+()"
-- main = print $ parse "aa|bbb|cc|dd"


type IntermResult = (Ctx, String, GroupEnv)

specials :: [Char]
specials =  ['(', ')', '[', ']', '{', '}', '|', '*', '+', '?', '\\', '.']

setOfSpecials :: Set Char
setOfSpecials =  Set.fromList specials

parse :: String -> Maybe Regex
parse s = case auxParse True grpEnvEmpty s of
    Just (ctx, _, _) -> return . simpl . restoRe $ ctx
    Nothing          -> empty

auxParse :: Bool -> GroupEnv -> String -> Maybe IntermResult
auxParse failhard env "" = return ((Eps, Root), "", env)
auxParse failhard env s  = case parseAtom failhard env s of
    Just ((re, Root), s', env') -> do
        ((re', path), s'', env'') <- auxParse failhard env' s'
        return ((Cat re re', path), s'', env'')
    Just ((re, path), s', env') -> do
        ((re', path'), s'', env'') <- auxParse failhard env' s'
        return ((re, path <> L (Cat Eps re') path'), s'', env'')
    Nothing -> if failhard then empty else return ((Eps, Root), s, env)

parseAtom :: Bool -> GroupEnv -> String -> Maybe IntermResult
parseAtom failhard env = quantity . asum . ([plainOrEscd, group, disjunction failhard] <&> ($ env) <&>) . flip ($)

quantity :: Maybe IntermResult -> Maybe IntermResult
quantity (Just ((re, path), s, env)) = return ((re', path), s', env)
    where
        (re', s') = case s of
            '*':s' -> (Star re,           s')
            '+':s' -> (Cat  re (Star re), s')
            '?':s' -> (Or   Eps re,       s')
            _      -> (re,                s)
quantity Nothing = empty

disjunction :: Bool -> GroupEnv -> String -> Maybe IntermResult
disjunction failhard env ('|':s) = do
    (ctx, s', env') <- auxParse failhard env s
    return ((Eps, L (Or Eps (restoRe ctx)) Root), s', env')
disjunction _ _ _ = empty

plainOrEscd :: GroupEnv -> String -> Maybe IntermResult
plainOrEscd env ('\\':c:s) = do
    pred <- predFromChar True c
    return ((Lit ('\\':[c]) pred, Root), s, env)
plainOrEscd env (c:s) = do
    pred <- predFromChar False c
    return ((Lit [c]        pred, Root), s, env)
plainOrEscd _ _ = empty

predFromChar :: Bool -> Char -> Maybe (Char -> Bool)
predFromChar escaped c = do
        c2p <- Map.lookup escaped table
        Map.lookup c c2p <|> if (not escaped) && isNotSpecial
                                then return (== c)
                             else empty
    where
        isNotSpecial = Set.notMember c setOfSpecials
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
        Just (ctx, ')':s'', env') -> return ((Group "" (restoRe ctx), Root), s'', env')
        _                         -> empty
    _  -> case s' of
        (')':s'') -> do
            gr <- grpEnvGet env name
            return ((gr, Root), s'', env)
        _         -> case auxParse False env s' of
            Just (ctx, ')':s'', env') -> case grpEnvGet env' name of
                Nothing -> let gr = Group name (restoRe ctx) in
                    return ((gr, Root), s'', fromJust $ grpEnvAdd env' name gr)
                Just _  -> empty
            _                         -> empty
    where
        (name, s') = case parseName s of
            Just (name', s'') -> (name', s'')
            Nothing           -> ("",    s)

        parseName ('<':s) = auxParseName s
        parseName _       = empty

        auxParseName ('>':s) = return ("", s)
        auxParseName (c:s)   = case auxParseName s of
            Just (name, s') -> return (c:name, s')
            Nothing         -> empty
        auxParseName []      = empty
group _ _ = empty


re :: QuasiQuoter
re =  QuasiQuoter
    { quoteExp  = parseRegex
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
    where
        parseRegex s = case parse s of
            Nothing -> fail $ "Unable to parse the given regex string: " ++ s
            Just re -> interp re
        interp Eps             = [| Eps |]
        interp (Lit   lit  p)  = [| Lit   $(stringE lit)  $(mkPred lit) |]
        interp (Or    l    r)  = [| Or    $(interp l)     $(interp r)   |]
        interp (Cat   l    r)  = [| Cat   $(interp l)     $(interp r)   |]
        interp (Star  re)      = [| Star  $(interp re)                  |]
        interp (Group name gr) = [| Group $(stringE name) $(interp gr)  |]

        mkPred ('\\':c:s) = [| \x -> (fromJust . predFromChar True  $ c) x |]
        mkPred (c:s)      = [| \x -> (fromJust . predFromChar False $ c) x |]
