
import Data.List
import Data.List.Split
import Data.Text hiding (last, foldl', drop, take, any, head, toLower, tail, words, splitOn, concat, intercalate, lines, map, length, foldl, init, all)
import Prelude hiding (head, words, lex)
import Control.Monad
import Data.Char
import qualified Data.Text as Text
import qualified Data.Text.IO
import Control.Applicative
import System.IO
import Data.Maybe
import System.Directory
import DATA
import Data.Binary
import System.IO.Unsafe



-- argStuff tokens i val
--    | i <= val = do
--       let argSplitPoint = findEndHelper tokens i val 0 ","
--       if i == argSplitPoint 
--          then do
--             print " Missing variable in quantifier."
--             return ()
--          else do
--             let toAppend = parse $ getSubList tokens i argSplitPoint
--             appendFile "args.txt" "toAppend"
--             argStuff tokens (argSplitPoint+1) val
            --appendFile "args.txt" $ (parse (getSubList tokens i argSplitPoint)) ++ "\n"
            

-- parse tokens
--     | tokens == [] = error "no inup"
--     | tokens!!0 == "forall" = do
--       let period = findChar (tail tokens) "." 0
--       --print (isJust period)
--       if isJust period 
--          then do
--             let val = fromJust period
--             removeFile "args.txt"
--             writeFile "args.txt" ""
--             argStuff tokens 1 val
--             (if (length tokens) == val+1
--                then error "Missing formula in Forall quantifier."
                  
--                else
--                   let formula = parse $ getSubList tokens (val+1) (length tokens) in formula
--                   --"formula"
              
--                   )
--          else error "Missing . in forall quantifier."
            
--     | tokens!!0 == "exists" = do
--       let period = findChar (tail tokens) "." 0
--       if isJust period
--          then do
--             let val = fromJust period
--             removeFile "args.txt"
--             writeFile "args.txt" ""
--             argStuff tokens 1 val
--             (if (length tokens) == val+1
--                then error "Missing formula in exists quantifier."
                
--                else let formula = parse $ getSubList tokens (val+1) (length tokens) in formula
--                   --"formula"
                 
--                   )
--          else error "Missing . in exists quantifier."
            
--     | otherwise = do
      -- -- -- -- --
   -- -- -- OR -- -- --
     -- -- -- -- -- -- 
      --let or_ind = findImpHelper tokens 0 (length tokens) 0 "or" in or_ind

      -- when (isJust or_ind) $ do
      --       let or_val = fromJust or_ind
      --       let on_left = findDoubleHelper tokens 0 or_val 0 "forall" "exists"
      --       when (not on_left) $ do
      --          if or_val == 0 || or_val == ((length tokens)-1)
      --             then error "Missing formula in OR connective."
                    
      --             else do
      --                let first_half = init $ getSubList tokens 0 or_val
      --                let second_half = init $ getSubList tokens (or_val+1) (length tokens)

      --                let left_parse = parse first_half
      --                let right_parse = parse second_half
               
      --                let formula = Or left_parse right_parse in formula
                     

      -- -- -- -- -- --
      -- -- NOT -- -- --
      -- -- -- -- -- --4
      -- when (tokens!!0 == "not") $ do
      --       if (length tokens) < 2
      --           then error "Missing formula in Not connective"
      --           else let formula = Not $ parse (tail tokens) in formula


      -- -- -- -- -- 
      -- PREDICATE --
      -- -- -- -- --
      --when (isAlphaNum (tokens!!0) && not ((toLower (tokens!!0)) `elem` keywords) && (length tokens) == 1 && not (all isLower tokens!!0)) $ return $ Predicate (tokens!!0) []
     
           



      --print "Unable to Parse!!!!!!!!"
     



      -- implies stuff, doesn't have to be in front
      -- let implies = findImpHelper tokens 0 (length tokens) 0 "implies"
      -- if isJust implies
      --    then do
      --       let val = fromJust implies
      --       let lft = findDoubleHelper tokens 0 val 0 "forall" "exists"
      --       (if (not lft) 
      --          then do
      --             (if val == 0 || val == ((length tokens) -1)
      --                then do
      --                   print "Missing formula in implies connective."
      --                   doStuff
      --                else do
      --                   print "Do parsing"
      --                   doStuff)
      --          else
      --             doStuff

      --             )
      --        --doStuff
      --    else 
      --       doStuff

-- finImplies tokens
--     | i < val && tokens!!i == "(" = findImpHelper tokens (i+1) val (paren+1) toFind
--     | i < val && tokens!!i == ")" = findImpHelper tokens (i+1) val (paren-1) toFind
--     | paren == 0 && i < val && (tokens!!i) == toFind = Just i
--     | i < val = findImpHelper tokens (i+1) val paren toFind
--     | otherwise = Nothing



  -- for finding two things
findDoubleHelper tokens i val paren toFind1 toFind2
    | i < val && tokens!!i == "(" = findDoubleHelper tokens (i+1) val (paren+1) toFind1 toFind2
    | i < val && tokens!!i == ")" = findDoubleHelper tokens (i+1) val (paren-1) toFind1 toFind2
    | paren == 0 && i < val && ((tokens!!i) == toFind1 || (tokens!!1) == toFind2) = True
    | i < val = findDoubleHelper tokens (i+1) val paren toFind1 toFind2
    | otherwise = False


lowerCase :: String -> String
lowerCase [] = []
lowerCase (x:xs) = (toLower x):(lowerCase xs)

testFuncPredVar :: [String] -> Int
testFuncPredVar tokens 
    | isAlphaNum ((tokens!!0)!!0) 
    && not ((lowerCase (tokens!!0)) `elem` keywords)
    && (length tokens) > 1
    && all isLower (tokens!!0)
    && tokens!!1 == "("                                = 0
    | isAlphaNum ((tokens!!0)!!0) 
    && not ((lowerCase (tokens!!0)) `elem` keywords)
    && (length tokens) == 1
    && not (all isLower (tokens!!0))                   = 1
    | isAlphaNum ((tokens!!0)!!0) 
    && not ((lowerCase (tokens!!0)) `elem` keywords)
    && (length tokens) == 1
    && all isLower (tokens!!0)                         = 2
    | otherwise = 3


findMiddle :: [String] -> String -> Int -> Bool
findMiddle [] _ _ = False
findMiddle (x:xs) target pCount
    | x == "(" = findMiddle xs target (pCount+1)
    | x == ")" = findMiddle xs target (pCount-1)
    | pCount == 0 && x == target = True
    | otherwise = findMiddle xs target pCount

--findEndHelper :: [String] -> Int -> Int -> Int
findEndHelper tokens i val paren toFind
    | i < val && tokens!!i == "(" = findEndHelper tokens (i+1) val (paren+1) toFind
    | i < val && tokens!!i == ")" = findEndHelper tokens (i+1) val (paren-1) toFind
    | paren == 0 && i < val && (tokens!!i) == toFind = Just i
    | i < val = findEndHelper tokens (i+1) val paren toFind
    | otherwise = Nothing

getSubList :: [String] -> Int -> Int -> [String]
getSubList tokens start end 
   | start < end = [(tokens!!start)] ++ (getSubList tokens (start+1) end)
   | otherwise = [[]]



findImpHelper tokens i val paren toFind
    | i < val && tokens!!i == "(" = findImpHelper tokens (i+1) val (paren+1) toFind
    | i < val && tokens!!i == ")" = findImpHelper tokens (i+1) val (paren-1) toFind
    | paren == 0 && i < val && (tokens!!i) == toFind = Just i
    | i < val = findImpHelper tokens (i+1) val paren toFind
    | otherwise = Nothing



--lexHelper2 :: String -> Int -> Int -> String -> (String, Int)
lexHelper2 :: String -> String
lexHelper2 [] = []
lexHelper2 (x:xs)
   | isAlphaNum x = x : (lexHelper2 xs)
   | otherwise = []

lexCount :: String -> Int
lexCount [] = 0
lexCount (x:xs)
   | isAlphaNum x = 1 + (lexCount xs)
   | otherwise = 0
  
lexIter [] _ _ = []
lexIter inp n mv
   | n < mv = lexIter (tail inp) (n+1) mv
   | otherwise = inp

--lexHelper :: String -> Bool
lexHelper [] = return ()
lexHelper inp 
   | (head inp) == ' ' = lexHelper (tail inp) 
   | otherwise = do
      let identifier = lexHelper2 inp 
      let mv = lexCount inp
      let yinp = lexIter inp 0 mv
      if (length identifier) > 0
         then do
            appendFile "lex.txt" $ identifier ++ "\n"
            lexHelper yinp 
         else if yinp == []
            then return ()
            else do 
               appendFile "lex.txt" $ [(head yinp)] ++ "\n"
               lexHelper (tail yinp)

whileHelper :: [String] -> Int -> Int -> Int
whileHelper tokens i period_val 
    | i <= period_val = 1
        --let end = period_val
        --let loc = findEndHelper tokens i period_val 0 "," in loc
        -- if isJust loc 
        --     then let end = fromJust loc in end
        --     else let end = period_val in end

        -- when (end == i) $ error "Missing variable in Forall quantifier"
        -- --let toAppend = parse $ getSubList tokens i end
        -- appendFile "args.txt" "toAppend"
        -- --argStuff tokens (argSplitPoint+1) val
        -- --appendFile "args.txt" $ (parse (getSubList tokens i argSplitPoint)) ++ "\n"
        -- whileHelper tokens (end+1) period_val
    | otherwise = 0

findChar :: [String] -> String -> Int -> Int
findChar [] _ _  = (-1)
findChar (x:xs) p n
    | x == p = n
    | otherwise = findChar xs p (n+1)

forAllParser :: [String] -> String
forAllParser tokens = "FORALL"
    --let period = Nothing
    --let prd = findChar (tail tokens) "." 1 in prd

    -- when (isJust prd) $ let prd_val = fromJust prd in prd_val
    -- let period_val = fromJust period

    -- print period_val

    -- --removeFile "args.txt"
    -- writeFile "args.txt" ""

   --let strg = whileHelper tokens 1 (period_val::Int)



notParser :: [String] -> String
notParser tokens = 
    --let formula = Not (predicateParser (tail tokens))
    "NOT("++(parser (tail tokens))++")"

predicateParser :: [String] -> String
predicateParser tokens = do
    --let formula = Predicate (tokens!!0) []
    "PREDICATE("++(head tokens)++",[])"

lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' (const . drop 1) xs (drop n xs)

-- lexIter [] _ _ = []
-- lexIter inp n mv
--    | n < mv = lexIter (tail inp) (n+1) mv
--    | otherwise = inp

orParser :: [String] -> String
orParser tokens = do
    let or_ind = findImpHelper tokens 0 (length tokens) 0 "or"
    "OR("++(parser (slice 0 ((fromJust or_ind)-1) tokens))++","++(parser (lexIter tokens 0 ((fromJust or_ind)+1)))++")"

    -- when (isJust or_ind) $ do
    --     let or_val = fromJust or_ind
    --     let on_left = findDoubleHelper tokens 0 or_val 0 "forall" "exists"
    --     when (not on_left) $ do
    --        if or_val == 0 || or_val == ((length tokens)-1)
    --           then "Missing formula in OR connective."
                
    --           else do
    --              let first_half = init $ getSubList tokens 0 or_val
    --              let second_half = init $ getSubList tokens (or_val+1) (length tokens)

    --              let nxt = parseForName first_half
    --              let scnd = parseForName second_half in scnd
    --              "OR"


    --     "OR"

                 --let formula = Or (predicateParser first_half) (notParser second_half) in formula

                 --check_formula(formula)
                 --result = proveFormula()

    -- "OR"




                 --let left_parse = parse first_half
                 --let right_parse = parse second_half
           
                 --let formula = Or left_parse right_parse in formula

existsParser tokens = "EXISTS"

slice from to xs = take (to - from + 1) (drop from xs)

impliesParser tokens = do
  let implies_ind = findImpHelper tokens 0 (length tokens) 0 "implies" 
  "IMPLIES("++(parser (slice 0 ((fromJust implies_ind)-1) tokens))++","++(parser (lexIter tokens 0 ((fromJust implies_ind)+1)))++")"

andParser tokens = do
  let and_ind = findImpHelper tokens 0 (length tokens) 0 "and"
  "AND("++(parser (slice 0 ((fromJust and_ind)-1) tokens))++","++(parser (lexIter tokens 0 ((fromJust and_ind)+1)))++")"


functionParser tokens = "FUNCTION"

variableParser tokens = "VARIABLE("++(show (head tokens))++")"

groupThings tokens = "GROUP"

functionCaller :: String -> [String] -> String
functionCaller str tokens
    | str == "forall" = forAllParser tokens
    | str == "not" = notParser tokens
    | str == "predicate" = predicateParser tokens
    | str == "or" = orParser tokens
    | str == "exists" = existsParser tokens
    | str == "implies" = impliesParser tokens
    | str == "and" = andParser tokens
    | str == "function" = functionParser tokens
    | str == "variable" = variableParser tokens
    | otherwise = "invalid name"






parseForName :: [String] -> String
parseForName tokens 
    | tokens!!0 == "forall"             = "forall"
    | tokens!!0 == "exists"             = "exists"
    | tokens!!0 == "not"                = "not"
    | (findMiddle tokens "implies" 0)   = "implies"
    | (findMiddle tokens "or" 0)        = "or"
    | (findMiddle tokens "and" 0)       = "and"
    | testFuncPredVar tokens == 0       = "function"
    | testFuncPredVar tokens == 1       = "predicate"
    | testFuncPredVar tokens == 2       = "Variable"
    | otherwise                         = "Nothing"


contains [] _ = False
contains (x:xs) lst2 
    | x `elem` lst2 = True
    | otherwise = False

writeToFile file str =
  appendFile file str

parser :: [String] -> String
parser tokens = do
  
  let name = parseForName tokens
  let fdf = writeToFile "axioms.txt" name
  functionCaller name tokens
  --print formula
    --parse for formula
    --check it
    --prove it
   

helperMain = do
    putStrLn "Please enter command: "
    inp <- getLine
    --print inp

     
    removeFile "lex.txt"
    writeFile "lex.txt" ""

    
    lexHelper inp
    content <- readFile "lex.txt"
    let tokens = lines content 

    when(contains tokens keywords) $ do
        print "Unexpected Keyword."
        helperMain

    --when(not(contains tokens validCommands)) $ 

    let str = parser tokens
    print str

    let len = (length tokens)

   
    when (tokens!!0 == "axioms" && len > 0) $ print . lines =<< readFile ((tokens!!0)++".txt")
    when (tokens!!0 == "axioms" && len > 1) $ do
        print "Unexpected arguemts"
        helperMain
    when (tokens!!0 == "lemmas" && len > 0) $ print . lines =<< readFile ((tokens!!0)++".txt")
    when (tokens!!0 == "lemmas" && len > 1) $ do
        print "Unexpected arguments"
        helperMain
    when (tokens!!0 == "axiom" && len > 0) $ do
        --let name = parseForName (tail tokens)
         -- formula -> parse (tail tokens)
         -- check
        print tokens
        appendFile "axioms.txt" $ (intercalate " " (tail tokens)) ++ "\n"
    when (tokens!!0 == "lemma" && len > 0) $ do
         -- formula -> parse (tail tokens)
         -- check
        appendFile "lemmas.txt" $ (intercalate " " (tail tokens)) ++ "\n" 
    when (tokens!!0 == "reset" && len > 0) $ do 
        removeFile "axioms.txt"
        writeFile "axioms.txt" ""
        removeFile "lemmas.txt"
        writeFile "lemmas.txt" ""




    helperMain




validCommands :: [String]
validCommands = ["axiom", "lemma", "axioms", "lemmas", "remove", "reset"]

keywords :: [String]
keywords = ["not", "implies", "and", "or", "forall", "exists"]

main = do
   putStrLn "Terms:"
   putStrLn ""
   putStrLn "  x                   (variable)"
   putStrLn "  f(term, ...)        (function)"
   putStrLn ""
   putStrLn "Formulae:"
   putStrLn ""
   putStrLn "  P(term)             (predicate)"
   putStrLn "  not P               (complement)"
   putStrLn "  P or Q              (disjunction)"
   putStrLn "  P and Q             (conjunction)"
   putStrLn "  P implies Q         (implication)"
   putStrLn "  forall x. P         (universal quantification)"
   putStrLn ""
   putStrLn "  exists x. P         (existential quantification)"
   putStrLn "Enter formulae at the prompt. The following commands are also available for manipulating axioms:"
   putStrLn ""
   putStrLn "  axioms              (list axioms)"
   putStrLn "  lemmas              (list lemmas)"
   putStrLn "  axiom <formula>     (add an axiom)"
   putStrLn "  lemma <formula>     (prove and add a lemma)"
   putStrLn "  remove <formula>    (remove an axiom or lemma)"
   putStrLn "  reset               (remove all axioms and lemmas)"
   putStrLn ""


   -- removeFile "axioms.txt"
   -- removeFile "lemmas.txt"

   --initialize files to store axioms and lemmas
   writeFile "axioms.txt" ""
   writeFile "lemmas.txt" ""
   

   

   helperMain














