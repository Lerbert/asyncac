module UML.Common.Parser (module UML.Common.Parser, module Common.Parser) where

import Text.ParserCombinators.Parsec

import Common.Parser
import UML.Common.Types

parseMachineName :: GenParser Char st MachineName
parseMachineName = MachineName <$> parseIdentifier

parsePort :: GenParser Char st Port
parsePort = Port <$> parseIdentifier

parseEventName :: GenParser Char st EventName
parseEventName = EventName <$> parseIdentifier

-- parse helpers

keyword :: String -> GenParser Char st String
keyword s = string s <* notFollowedBy alphaNum
