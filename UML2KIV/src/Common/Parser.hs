module Common.Parser where

import Text.ParserCombinators.Parsec
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

import Common.Types

formulaForbiddenChars = ":]"
expressionForbiddenChars = ",;)}"

parseFormula :: GenParser Char st Formula
parseFormula = trimFormula <$> many (noneOf formulaForbiddenChars)
    where trimFormula s = OpaqueFormula $ dropWhileEnd isSpace s

parseExpression :: GenParser Char st Expression
parseExpression = trimExpr <$> many (noneOf expressionForbiddenChars)
    where trimExpr s = OpaqueExpression $ dropWhileEnd isSpace s

parseIdentifier :: GenParser Char st String
parseIdentifier = (:) <$> letter <*> many alphaNum

parseVar :: GenParser Char st Variable
parseVar = Variable <$> parseIdentifier

parseAssumptionCommitmentFormula :: GenParser Char st AssumptionCommitmentFormula
parseAssumptionCommitmentFormula = between (char '[') (char ']')
    (AssumptionCommitmentFormula <$> (spaces *> parseFormula) <*> (asSep ":" *> parseFormula <* spaces))
    <*> (asSep ":" *> between (char '[') (char ']') (spaces *> parseFormula <* spaces))
    <?> "[<formula> : <formula>] : [<formula>]"

-- parse helpers
asSep :: String -> GenParser Char st String
asSep s = spaces *> string s <* spaces
