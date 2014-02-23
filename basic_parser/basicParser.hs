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
bid :: Parser String
bid = many1 letter

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


type Lines a = [(Int, Statements a)]
basiclines :: Parser (Lines a)
basiclines = bl1 <|> bl2 <?> "Could not parse a Basic line: Integer Statement Newline Lines or Integer Statement Newline"
  
bl1 = do
  i <- int :: Parser Int
  s <- statements :: Parser (Statements a)
  newline
  ls <- basiclines
  return ((i, s):ls)
  
bl2 = do
  i <- int
  s <- statements
  return [(i,s)]
  
                  
type Statements a = [Statement a]
statements :: Parser (Statements a)
statements = s1 <|> s2 <?> "could not parse a statement: Statement ':' Statements or Statement"
             
s1 = do
  s <- statement :: Parser (Statement a)
  tokens' (char ':')
  ls <- statements
  return (s:ls) 
  
s2 = do
  s <- statement
  return [s]

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
                   | Print [a] 
                   | PrintAt (Constant Int) [a] 
                   | Read [ID] 
                   | RETURN 
                   | RESTORE 
                   | RUN 
                   | STOP 
                   | Sys (Value a) 
                   | Wait (Value a) 
                   | Remark
                     
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
  show (Open vs a c)        = "OPEN " ++ (show vs) ++ " FOR " ++ (show a) ++ " AS #" ++ (show c)
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
  show Remark               = "REM"
  show END                  = "END"
  
string' = tokens' . string

expression = undefined

statement :: Parser (Statement a)
statement = tokens' $ ((try dim) <|> (try dataStatement) <|> (try end) <|> (try for) <|> (try forstep) <|> (try goto) <|> try gosub <?> "Failed attempting to parse a statement")


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
  [s] <- constant
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

--ds1 = do
  
constantList = tokens' $ ((try cls) <|> (try constant) <?> "Could not parse: Data {Constant list}")

cls :: Parser [Constant a]
cls = do 
  [c] <- constant
  cs  <- constantList
  return (c:cs)

tokens' :: Parser a -> Parser a
tokens' p = do
  p' <- p
  many ws
  return p'


data Access = AInput | AOutput 
instance Show Access where
  show AInput = "INPUT"
  show AOutput = "OUTPUT"

data Expression a = forall a . (Show a) => Expression a
instance Show (Expression a) where
  show (Expression a ) = show a


--Value parsers
data Value a = forall a . (Show a) => Ve (Expression a) | Vi a | Vil [a] | V (Constant a) 
instance (Show a) => Show (Value a) where 
  show (Ve a) = show a
  show (Vi a) = show a
  show (Vil a) = concatMap show a
  show (V a) = show a
  
  
--Constants parser
data Constant a = forall a . (Show a) => Constant { value :: a } 
instance Show (Constant a) where
  show (Constant a) = show a

constant :: Parser [Constant a]
constant = do
  x <- (try real >>= return . Constant) 
         <|> (try int >>= return . Constant) 
         <|> (try basicString >>= return . Constant) 
         <?> "could not parse a constant"
  return [Constant x]

--basicString :: Parser (Constant a)
--basicString = (tokens' $ do 
--                  char '"'
--                  letters <- many1 letter 
--                  char '"'
--                  return  (Constant letters)) <?> "No BasicString value found"

--int :: Parser (Constant a)
--int = (tokens' (many1 digit)) >>= return . Constant . (read :: String -> Int) <?> "could not parse integer"

real :: Parser Double
real = (tokens' $ do 
  n1 <- many1 digit 
  char '.'
  n2 <- many1 digit 
  return $ (read (n1 ++ ['.'] ++ n2) :: Double) ) <?> "could not parse Real value number"


