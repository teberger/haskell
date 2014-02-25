{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Text.ParserCombinators.Parsec
import System.Environment
import System.IO

main :: IO ()
main = do 
  program:_ <- getArgs
  contents <- hGetContents =<< openFile program ReadMode
  
  return ()

ws :: Parser ()
ws = space >> return ()

--newline :: Parser ()
--newline = (try (string "\n\r") <|> try (string "\n")) >> return ()

whitespace :: Parser ()
whitespace = many ws >> return ()

remark :: Parser String
remark = do
  string "REM"
  char ' '
  (manyTill anyChar newline) >>= return

type ID = String
bid :: Parser ID
bid = many1 letter

type Lines a = [(Int, Statements a)]
basiclines :: Parser (Lines a)
--basiclines = bl1 <||> bl2 <?> "Could not parse a Basic line: Integer Statement Newline Lines or Integer Statement Newline"
basiclines = basicList bl newline
  
bl :: Parser (Int, Statements a)
bl = do
  i <- int
  s <- statements
  newline
  return (i,s)
  
type Statements a = [Statement a]
statements :: Parser [Statement a]
--statements = s1 <||> s2 <?> "could not parse a statement: Statement ':' Statements or Statement"
statements = basicList s (char ':') 

s :: Parser (Statement a)
s = do
  s <- statement
  return s

data Statement a = forall a .  
                     SData [Constant a] 
                   | Dim ID [Constant Int] 
                   | END 
                   | For ID (Expression a) (Expression a) 
                   | ForStep ID (Expression a) (Expression a) (Constant Int) 
                   | Goto (Expression a) 
                   | Gosub (Expression a) 
                   | If (Expression a) (Statement a) 
                   | InputList [ID] 
                   | Input (Constant Int) [ID] 
                   | Let ID (Expression a) 
                   | Next [ID] 
                   | Open (Value a) Access (Constant Int) 
                   | Poke [Value a] 
                   | Print [Expression a] 
                   | PrintAt (Constant Int) [Expression a] 
                   | Read [ID] 
                   | RETURN 
                   | RESTORE 
                   | RUN 
                   | STOP 
                   | Sys (Value a) 
                   | Wait (Value a) 
                   | Remark String
                     
instance (Show a) => Show (Statement a) where                     
  show (SData a)            = "DATA " ++ (show a)
  show (Dim id as)          = "DIM " ++ id ++ " " ++ (show as)
  show (For id e1 e2)       = "FOR " ++ id ++ " = " ++ (show e1) ++ " TO " ++ (show e2) 
  show (ForStep id e1 e2 c) = "FOR " ++ id ++ " = " ++ (show e1) ++ " TO " ++ (show e2) ++ " STEP " ++ (show c)
  show (Goto e)             = "GOTO " ++ (show e)
  show (Gosub e)            = "GOSUB " ++ (show e)
  show (If e1 s)            = "IF " ++ (show e1) ++ " THEN " ++ (show s)
  show (InputList ids)      = "INPUT " ++ (show ids)
  show (Input c ids)        = "INPUT #" ++ (show c) ++ " " ++ (show ids)
  show (Let id e1)          = "LET " ++ id ++ " = " ++ (show e1)
  show (Next ids)           = "NEXT " ++ (show ids)
  show (Open v a c)         = "OPEN " ++ (show v) ++ " FOR " ++ (show a) ++ " AS #" ++ (show c)
  show (Poke vs)            = "POKE " ++ (show vs)
  show (Print as)           = "PRINT " ++ (show as)
  show (PrintAt c as)       = "PRINT #" ++ (show c) ++ " " ++ (show as)
  show (Read ids)           = "READ " ++ (show ids)
  show RETURN               = "RETURN"
  show RESTORE              = "RESTORE"
  show RUN                  = "RUN"
  show STOP                 = "STOP"
  show (Sys v)              = "SYS " ++ (show v)
  show (Wait v)             = "WAIT " ++ (show v)
  show (Remark s)           = "REM" ++ s
  show END                  = "END"

string' = tokens' . string

expression :: Parser (Expression a)
expression = undefined

--TODO FINISH EXPRESSION LIST PARSER
value :: Parser (Value a)
value = (constant >>= return . Value) <||> ((between (char '(') (char ')') expression) >>= return . Value) <||> (bid >>= return . Value) <||> undefined --expressionList

basicList :: Parser a -> Parser sep -> Parser [a]
basicList itemParser sep = (b1 itemParser sep) <||> try (b2 itemParser) <?> "Failed parsing a list"

b1 :: Parser a -> Parser sep -> Parser [a]
b1 a sep = do
  a' <- a
  tokens' sep
  as <- basicList a sep
  return (a':as)

b2 :: Parser a -> Parser [a]
b2 a = a >>= \x -> return [x]

idlist :: Parser [ID]
idlist = basicList bid (char ',')

valueList :: Parser [Value a]
valueList = basicList value (char ',')

printList :: Parser [Expression a]
printList = basicList expression (char ';') <|> return []

access :: Parser Access
access = ((string' "INPUT") >> return AInput)
         <||> (try (string' "OUTPUT") >> return AOutput)

(<||>) f s = try f <|> s

statement :: Parser (Statement a)
statement = tokens' $ (dim <||>  dataStatement <||>  end <||>  for <||>  forstep <||>  goto <||>  gosub <||>  ifStatement <||>  inputls <||>  input <||>  letstatement <||>  next <||>  open <||>  poke <||>  printStatement <||>  printAt <||>  returnStatement <||>  restore <||>  run <||>  stop <||>  sys <||>  wait <||>  try remStatement <?> "Failed attempting to parse a statement")

stop :: Parser (Statement a)
stop = string' "STOP" >> return STOP

sys :: Parser (Statement a)
sys = string' "SYS" >> value >>= return . Sys

wait :: Parser (Statement a)
wait = string' "WAIT" >> value >>= return . Wait

remStatement :: Parser (Statement a)
remStatement = do
  r <- remark
  return (Remark r)

returnStatement :: Parser (Statement a)
returnStatement = string' "RETURN" >> return RETURN

restore :: Parser (Statement a)
restore = string' "RESTORE" >> return RESTORE

run :: Parser (Statement a)
run = string' "RUN" >> return RUN

readStatement :: Parser (Statement a)
readStatement = do
  string' "READ"
  ls <- idlist
  return (Read ls)

printAt :: Parser (Statement a)
printAt = do
  string' "PRINT #"
  i <- int
  ls <- printList
  return (PrintAt (Constant i) ls)

printStatement :: Parser (Statement a)
printStatement = do
  string' "PRINT" 
  ls <- printList
  return (Print ls)

poke :: Parser (Statement a)
poke = do
  string' "POKE"
  vs <- valueList
  return (Poke vs)

open :: Parser (Statement a)
open = do
  string' "OPEN"
  vs <- value
  a <- access
  i <- int
  return (Open vs a (Constant i))

next :: Parser (Statement a)
next = do
  string' "NEXT"
  ls <- idlist
  return (Next ls)

letstatement :: Parser (Statement a)
letstatement = do
  string' "LET"
  i <- bid
  tokens' (char '=')
  e <- expression
  return (Let i e)

input :: Parser (Statement a)
input = do
  string' "INPUT #"
  i <- int
  ls <- idlist
  return (Input (Constant i) ls)

inputls :: Parser (Statement a)
inputls = do
  string' "INPUT"
  ids <- idlist
  return (InputList ids)

ifStatement :: Parser (Statement a)
ifStatement = do
  string' "IF"
  e <- expression
  string' "THEN"
  s <- statement
  return (If e s)

gosub :: Parser (Statement a)
gosub = do
  string' "GOSUB"
  e <- expression
  return (Gosub e)

goto :: Parser (Statement a)
goto = do
  string' "GOTO"
  e <- expression
  return (Goto e)

forstep :: Parser (Statement a)
forstep = do
  string' "FOR"
  i <- bid
  tokens' (char '=')
  e1 <- expression 
  string' "TO"
  e2 <- expression
  string' "STEP"
  s <- constant
  return (ForStep i e1 e2 s)

for :: Parser (Statement a)
for = do
  string' "FOR"
  i <- bid
  tokens' (char '=')
  e1 <- expression 
  string' "TO"
  e2 <- expression
  return (For i e1 e2)

end :: Parser (Statement a)
end = string' "END" >> return END

dim :: Parser (Statement a)
dim = do
  string' "DIM"
  spaces
  i <- bid
  ls <- constantList
  return (Dim i ls)

dataStatement :: Parser (Statement a)
dataStatement = do
  string' "DATA"
  x <- constantList :: Parser [Constant a]
  return (SData x) 

--Constants parser
data Constant a = forall a . (Show a) => Constant { val :: a } 
instance Show (Constant a) where
  show (Constant a) = show a

constantList :: Parser [Constant a]
constantList = basicList constant (char ',')

intList :: Parser [Int]
intList = basicList int (char ',')

constant :: Parser (Constant a)
constant = (basicReal >>= return . Constant) <||> (int >>= return . Constant) <||> (basicString >>= return . Constant) <?> "could not parse a constant"

basicString :: Parser String
basicString = tokens' $ do
  c <- (char '"' <|> char '\'')
  manyTill anyChar (char c) >>= return
  
int :: Parser Int  
int = tokens' (many1 digit >>= return . read)

basicReal :: Parser Double
basicReal = tokens' $ do
  i <- many1 digit
  char '.'
  i2 <- many1 digit
  return (read (i ++ "." ++ i2))


{-constantList = tokens' $ (cls <||> (try constant) <?> "Could not parse: Data {Constant list}")

cls :: Parser [Constant a]
cls = do 
  [c] <- constant
  tokens' (char ',')
  cs  <- constantList
  return (c:cs)
-}

data Access = AInput | AOutput 
instance Show Access where
  show AInput = "INPUT"
  show AOutput = "OUTPUT"

data Expression a = forall a . (Show a) => Expression a
instance Show (Expression a) where
  show (Expression a ) = show a


--Value parsers
data Value a = forall a . (Show a) => Value a
instance (Show a) => Show (Value a) where 
  show (Value a) = show a
  
tokens' :: Parser a -> Parser a
tokens' p = do
  p' <- p
  many ws
  return p'


