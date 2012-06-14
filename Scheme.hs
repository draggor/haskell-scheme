module Scheme (readExpr) where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad
import Numeric

data LispVal = Atom String
	     | List [LispVal]
	     | DottedList [LispVal] LispVal
	     | Number Integer
	     | String String
	     | Bool Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

character :: Parser Char
character = symbol <|> letter <|> digit

escapable :: Parser Char
escapable = oneOf "\\\"" <|> character

escape :: Parser String
escape = do char '\\'
	    second <- escapable
	    str <- stringRecurse
	    return $ ['\\', second] ++ str

spaces :: Parser ()
spaces = skipMany1 space

{-
parseString :: Parser LispVal
parseString = do char '"'
		 x <- many (noneOf "\"")
		 char '"'
		 return $ String x
-}

stringRecurse :: Parser String
stringRecurse = do x <- many character
		   y <- liftM concat (many escape)
		   return $ x ++ y

parseString :: Parser LispVal
parseString = do char '"'
		 --x <- many character <|> escape --many (noneOf "\"") <|> escape
		 x <- stringRecurse
		 char '"'
		 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
	       rest <- many (letter <|> digit <|> symbol)
	       return $ Atom ([first] ++ rest)

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

extractFromRead :: [(a, b)] -> a
extractFromRead i = fst (i !! 0)

--These parsers require backtracking to work,
--which is not necessary for this grammar.
{-
parseDec :: Parser LispVal
parseDec = do first <- char '#' <|> digit
	      second <- char 'd' <|> digit
	      rest <- many digit
	      return $ (Number . extractFromRead . readDec) (case first of
	      						 	  '#' -> rest
								  otherwise -> [first] ++ [second] ++ rest)

parseOct :: Parser LispVal
parseOct = do string "#o"
	      rest <- many (oneOf ['0'..'7'])
	      return $ (Number. extractFromRead . readOct) rest

parseHex :: Parser LispVal
parseHex = do string "#h"
	      rest <- many (oneOf ['0'..'9'] <|> oneOf ['a'..'f'] <|> oneOf ['A'..'F'])
	      return $ (Number . extractFromRead . readHex) rest

parseNumber :: Parser LispVal
parseNumber = try parseHex <|> try parseOct <|> parseDec
-}

--The following line works, but is too verbose, and we shouldn't have to do these imports (Text.Parsec.Prim, Control.Monad.Identity)
--parsePrefixedNum :: Char -> (String -> [(Integer , String)]) -> ParsecT String () Identity Char -> Parser LispVal
parsePrefixedNum :: Char -> (String -> [(Integer , String)]) -> Parser Char -> Parser LispVal
parsePrefixedNum c readBase charParser =
	char c >>= \_ -> liftM (Number . extractFromRead . readBase) $ many1 charParser

parseHex :: Parser LispVal
parseHex = parsePrefixedNum 'x' readHex digitHex

parseDec :: Parser LispVal
parseDec = parsePrefixedNum 'd' readDec digitDec

parseOct :: Parser LispVal
parseOct = parsePrefixedNum 'o' readOct digitOct

parseBin :: Parser LispVal
parseBin = parsePrefixedNum 'b' readBin digitBin

{-
parseHex :: Parser LispVal
parseHex = char 'x' >>= \_ -> (liftM (Number . extractFromRead . readHex) $ many1 digitHex)

parseHex :: Parser LispVal
parseHex = do char 'x'
	      liftM (Number . extractFromRead . readHex) $ many1 digitHex

parseDec :: Parser LispVal
parseDec = do char 'd'
	      liftM (Number . extractFromRead . readDec) $ many1 digitDec

parseOct :: Parser LispVal
parseOct = do char 'o'
	      liftM (Number . extractFromRead . readOct) $ many1 digitOct

--This would hide 't' and 'f' in the list of expected values if something went wrong
parseTorF :: Parser LispVal
parseTorF = do torf <- oneOf "tf"
	       return $ case torf of
	   		     't' -> Bool True
			     'f' -> Bool False
-}

parseTrue :: Parser LispVal
parseTrue = do char 't'
	       return $ Bool True

parseFalse :: Parser LispVal
parseFalse = do char 'f'
		return $ Bool False

readBin :: String -> [(Integer, String)]
readBin s = [(sum $ zipWith (*) [2^x|x <- [0..]] (map ((+)(-48) . toInteger . fromEnum) $ reverse s), "")]
--readBin s = [(fst $ foldr (\digit (sum, count) -> (digit ^ count + sum, count+1)) (0, 0) $ map (\c -> case c of '0' -> 0; '1' -> 2) s, "")]

digitBin :: Parser Char
digitBin = oneOf "01"

digitDec :: Parser Char
digitDec = digit

digitOct :: Parser Char
digitOct = oneOf ['0'..'7']

digitHex :: Parser Char
digitHex = digit <|> oneOf ['a'..'f'] <|> oneOf ['A'..'F']

parsePound :: Parser LispVal
parsePound = do char '#'
		parseHex <|> parseDec <|> parseOct <|> parseBin <|> parseTrue <|> parseFalse

{-
parseNumber :: Parser LispVal
parseNumber = do number <- many1 digit
		 return $ Number (read number)
-}

{-
parseNumber :: Parser LispVal
parseNumber = many1 digit >>= f
	      where f result = return $ Number (read result)

parseNumber = many1 digit >>= \s -> return $ Number (read s)
-}

parseExpr :: Parser LispVal
parseExpr = parseAtom
	<|> parseString
	<|> parseNumber
	<|> parsePound

showLVal :: LispVal -> String
showLVal (String contents) = "String: " ++ contents
showLVal (Atom name) = "Atom: " ++ name
showLVal (Number contents) = "Number: " ++ (show contents)
showLVal (Bool True) = "Bool: True"
showLVal (Bool False) = "Bool: False"

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
	Left err -> "No match: " ++ show err
	Right val -> showLVal val
