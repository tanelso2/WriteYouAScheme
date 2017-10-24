import Control.Monad
import Control.Monad.Error
import Data.Char (digitToInt)
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Character Char
             | String String
             | Bool Bool
             deriving (Eq, Ord)

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character contents) = show contents
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ "; found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError
instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many $ letter <|> digit <|> symbol
              let atom = first:rest
              return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _ -> Atom atom

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ parseEscapeSequence <|> (noneOf "\"")
                char '"'
                return $ String x

parseEscapeSequence :: Parser Char
parseEscapeSequence = do
                        char '\\'
                        x <- oneOf "n\"tr\\"
                        return $ case x of
                                  'n' -> '\n'
                                  '"' -> '"'
                                  't' -> '\t'
                                  'r' -> '\r'
                                  '\\' -> '\\'

parseCharacter :: Parser LispVal
parseCharacter = do
                   string "#\\"
                   character <- many1 anyChar
                   return $ Character $ case length character of
                                         0 -> error "Not a character"
                                         1 -> character !! 0
                                         _ -> case character of
                                               "space" -> ' '
                                               "newline" -> '\n'
                                               "tab" -> '\t'


parseNumber :: Parser LispVal
parseNumber = liftM Number $ try parseDecimalNumber <|> try parseHexNumber <|> try parseOctalNumber <|> try parseBinaryNumber

parseDecimalNumber :: Parser Integer
parseDecimalNumber = do
                       numString <- many1 digit
                       return $ read numString


parseBinaryNumber :: Parser Integer
parseBinaryNumber = do
                     string "#b"
                     num <- many $ oneOf "01"
                     return $ case readInt 2 (`elem` "01") digitToInt num of
                               (num, ""):[] -> num
                               _ -> error "What the fuck number"

parseOctalNumber :: Parser Integer
parseOctalNumber = do
                     string "#o"
                     num <- many $ oneOf "01234567"
                     return $ case readOct num of
                               (num, ""):[] -> num
                               _ -> error "What the fuck number"

parseHexNumber :: Parser Integer
parseHexNumber = do
                     string "#x"
                     num <- many $ oneOf "0123456789abcdef"
                     return $ case readHex num of
                               (num, ""):[] -> num
                               _ -> error "What the fuck number"

parseFloat :: Parser LispVal
parseFloat = do
               firstPartStr <- many1 digit
               char '.'
               secondPartStr <- many1 digit
               let numString = firstPartStr ++ "." ++ secondPartStr in
                   return $ case readFloat numString of
                             (num, ""):[] -> Float num
                             _ -> error "What the fuck number"

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
                    head <- endBy parseExpr spaces
                    tail <- char '.' >> spaces >> parseExpr
                    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
                char '\''
                x <- parseExpr
                return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = try parseFloat
        <|> parseNumber
        <|> try parseCharacter
        <|> parseString
        <|> parseAtom
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

eval :: LispVal -> ThrowsError LispVal
eval val@(Character _) = return val
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, trueExp, falseExp]) =
    do result <- eval pred
       case result of
         Bool True -> eval trueExp
         Bool False -> eval falseExp
         notBool -> throwError $ TypeMismatch "bool" notBool
eval (List (Atom func : args)) = do
                                    evaledArgs <- mapM eval args
                                    apply func evaledArgs
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("boolean?", monOp (return . Bool . isBool)),
              ("symbol?", monOp (return . Bool . isAtom)),
              ("list?", monOp (return . Bool . isList)),
              ("char?", monOp (return . Bool . isCharacter)),
              ("string?", monOp (return . Bool . isString)),
              ("number?", monOp (return . Bool . isNumber)),
              ("float?", monOp (return . Bool . isFloat)),
              ("symbol->string", monOp symbolToString),
              ("string->symbol", monOp stringToSymbol),
              ("=", numBoolBinop (==)),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))
              ]

monOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
monOp fun [x] = fun x
monOp fun params = throwError $ NumArgs 1 params

isBool :: LispVal -> Bool
isBool (Bool _) = True
isBool _ = False

isAtom :: LispVal -> Bool
isAtom (Atom _) = True
isAtom _ = False

isList :: LispVal -> Bool
isList (List _) = True
isList _ = False

isCharacter :: LispVal -> Bool
isCharacter (Character _) = True
isCharacter _ = False

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _ = False

isFloat :: LispVal -> Bool
isFloat (Float _) = True
isFloat _ = False

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String s) = return $ Atom s
stringToSymbol val = throwError $ TypeMismatch "Expected string" val

symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom a) = return $ String a
symbolToString val = throwError $ TypeMismatch "Expected atom" val

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = do
                            unpackedNums <- mapM unpackNum params
                            return $ Number $ foldl1 op unpackedNums

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op (left:right:[]) = do
                                          left <- unpacker $ left
                                          right <- unpacker $ right
                                          return $ Bool $ left `op` right
boolBinop unpacker op args = throwError $ NumArgs 2 args

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = let parsed = reads s in
                            if null parsed
                                then throwError $ TypeMismatch "number" $ String s
                                else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

main :: IO ()
main = do
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
        putStrLn $ extractValue $ trapError evaled
