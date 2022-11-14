module UML.CompositeStructure.Parser (parseCompositeStructure, ParseCompositeStructureError, CompositeStructureItem(..)) where

import Text.ParserCombinators.Parsec

import UML.Common.Types
import UML.Common.Parser
import UML.CompositeStructure.Types

type ParseCompositeStructureError = ParseError

data CompositeStructureItem = NameItem StructureName
    | ComponentItem Component
    | ConnectionItem Connection
    deriving (Show)

parseCompositeStructure :: String -> Either ParseError [CompositeStructureItem]
parseCompositeStructure = parse parseCompositeStructureH "Failed to parse model"

parseCompositeStructureH :: GenParser Char st [CompositeStructureItem]
parseCompositeStructureH = between (keyword "system") (keyword "end system") $
    spaces *> (parseCompositeStructureItem `endBy` (spaces *> char ';' <* spaces)) <* spaces

parseCompositeStructureItem :: GenParser Char st CompositeStructureItem
parseCompositeStructureItem = NameItem <$> parseName
    <|> ComponentItem <$> try parseComponent
    <|> ConnectionItem <$> parseConnection

parseName :: GenParser Char st StructureName
parseName = try (keyword "name") *> spaces *> (StructureName <$> parseIdentifier) <?> "structure name"

parseComponent :: GenParser Char st Component
parseComponent = try (keyword "comp") *> spaces *> (Component <$> parseComponentName <*> (asSep ":" *> parseMachineName)) <?> "component"

parseConnection :: GenParser Char st Connection
parseConnection = try (keyword "conn") *> spaces *> (Connection <$> parseComponentPort <*> (asSep "--" *> parseComponentPort)) <?> "connection"

parseComponentPort :: GenParser Char st ComponentPort
parseComponentPort = ComponentPort <$> (parseComponentName <* char '.') <*> parsePort <?> "<name>.<port>"

parseComponentName :: GenParser Char st ComponentName
parseComponentName = ComponentName <$> parseIdentifier
