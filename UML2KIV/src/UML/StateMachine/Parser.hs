module UML.StateMachine.Parser (parseMachine, ParseMachineError, MachineItem(..)) where

import Text.ParserCombinators.Parsec

import UML.Common.Types
import UML.Common.Parser
import UML.StateMachine.Types

type ParseMachineError = ParseError

data MachineItem = NameItem MachineName
    | AcpItem AssumptionCommitmentFormula
    | VariableItem Variable
    | StateItem MachineState Assertion
    | InputItem EventDecl
    | OutputItem EventDecl
    | InitItem InitialState
    | TransItem Trans
    deriving (Show)

parseMachine :: String -> Either ParseError [MachineItem]
parseMachine = parse parseMachineH "Failed to parse model"

parseMachineH :: GenParser Char st [MachineItem]
parseMachineH = between (keyword "machine") (keyword "end machine") $
    spaces *> (concat <$> (parseMachineItem `endBy` (spaces *> char ';' <* spaces))) <* spaces

parseMachineItem :: GenParser Char st [MachineItem]
parseMachineItem = return <$> parseName
    <|> return <$> parseAcp
    <|> parseVars
    <|> parseInputs
    <|> parseOutputs
    <|> parseStates
    <|> return <$> parseInitialState
    <|> return <$> parseTrans

parseName :: GenParser Char st MachineItem
parseName = try (keyword "name") *> spaces *> (NameItem <$> parseMachineName) <?> "machine name"

parseAcp :: GenParser Char st MachineItem
parseAcp = try (keyword "acp") *> spaces *> (AcpItem <$> parseAssumptionCommitmentFormula) <?> "assumption commitment formula"

parseVars :: GenParser Char st [MachineItem]
parseVars = try (keyword "vars") *> commaList varItem <?> "variables"
    where varItem = VariableItem <$> parseVar

parseInputs :: GenParser Char st [MachineItem]
parseInputs = try (keyword "inputs") *> commaList eventDecl <?> "input declarations"
    where eventDecl = InputItem . mkDecl <$> parsePortEvent parseIdentifier
          mkDecl (a, b, c) = EventDecl a b (length c)

parseOutputs :: GenParser Char st [MachineItem]
parseOutputs = try (keyword "outputs") *> commaList eventDecl <?> "output declarations"
    where eventDecl = OutputItem . mkDecl <$> parsePortEvent parseIdentifier
          mkDecl (a, b, c) = EventDecl a b (length c)

parseStates :: GenParser Char st [MachineItem]
parseStates = try (keyword "states") *> commaList (stateItem <$> parseStateWithAssertion) <?> "states"
    where stateItem assertion@(state, _) = StateItem state assertion

parseInitialState :: GenParser Char st MachineItem
parseInitialState = try (keyword "init") *> spaces *> (mkInit <$> parseState <*> optionMaybe (asSep ":" *> initPred)) <?> "initial state"
    where initPred = between (char '[') (char ']') (spaces *> parseFormula <* spaces) <?> "[<formula>]"
          mkInit s p = InitItem $ InitialState s p

parseTrans :: GenParser Char st MachineItem
parseTrans = try (keyword "trans") *> spaces *> (mkTrans <$> parseState <*> (asSep "-->" *> parseState) <*> (asSep ":" *> parseTrigger) <*> (spaces *> parseGuard) <*> parseActions) <?> "transition"
    where mkTrans s d t g a = TransItem $ Trans s t g d a

parseTrigger :: GenParser Char st (Maybe Trigger)
parseTrigger = optionMaybe $ uncurry2 Trigger <$> parsePortEvent parseVar

parseGuard :: GenParser Char st (Maybe Formula)
parseGuard = optionMaybe (between (char '[') (char ']') (spaces *> parseFormula <* spaces) <?> "[<formula>]")

parseActions :: GenParser Char st [Action]
parseActions = option [] (asSep "/" *> between (char '{') (char '}') (spaces *> (parseAction `sepBy` char ';') <* spaces) <?> "/ { <action1>; <action2>; <...> }")

parseAction :: GenParser Char st Action
parseAction = spaces *>
    (try (AssignAction <$> parseAssignment) <|> (uncurry2 OutputAction <$> parsePortEvent parseExpression))
    <* spaces

parsePortEvent :: GenParser Char st a -> GenParser Char st (Port, EventName, [a])
parsePortEvent param = mkTriple <$> (parsePort <* char '.') <*> parseEventName <*> option [] (paramList param) <?> "<port>.<event>(<arg1>, <arg2>, <...>)"
    where mkTriple a b c = (a, b, c)

-- parsing for machine types

parseState :: GenParser Char st MachineState
parseState = MachineState <$> parseIdentifier

parseStateWithAssertion :: GenParser Char st Assertion
parseStateWithAssertion = (,) <$> parseState <*> optionMaybe (asSep ":" *> assertion)
    where assertion = between (char '[') (char ']') (spaces *> parseFormula <* spaces) <?> "[<formula>]"

parseAssignment :: GenParser Char st Assignment
parseAssignment = Assignment <$> parseVar <*> (spaces *> string ":=" *> spaces *> parseExpression <* spaces)

-- parse helpers

commaList :: GenParser Char st a -> GenParser Char st [a]
commaList p = (spaces *> p <* spaces) `sepBy` char ','

paramList :: GenParser Char st a -> GenParser Char st [a]
paramList p = between (char '(') (char ')') $ commaList p

uncurry2 f (a, b, c) = f a b c
