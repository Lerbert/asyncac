module Main where

import Data.Bifunctor (Bifunctor(second))
import qualified Data.Text as T
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), FilePath, takeDirectory)

import AsyncAC.Program.Types ( Program, withStateOutputs )
import KIV.Types ( KIVFile )
import UML.CompositeStructure.Semantic ( readCompositeStructure )
import UML.StateMachine.Semantic ( readStateMachine )
import UML.System.Types ( UMLSystem )
import UML.System.Semantic ( makeSystem )
import qualified Translation.UML2AsyncAC as UML2AC
import qualified Translation.AsyncAC2KIV as AC2KIV
import qualified Translation.KIV2File as KIV2File
import qualified Translation.AsyncAC2Graphviz as AC2Graphviz

programDescription :: String
programDescription = "Translate a system of communicating UML state machines to a KIV project for verification with the asynchronous assumption-commitment calculus.\
    \To visualize the resulting transition systems, they can be exported in graphviz format."

data Target = Graphviz | KIV

targetOpts :: Parser Target
targetOpts = flag' KIV (long "kiv" <> help "Export program as KIV project (default)") <|> flag' Graphviz (long "gviz" <> help "Export program in graphviz format") <|> pure KIV

data CLIOptions = CLIOptions
    {
        target :: Target,
        addStateOutput :: Bool,
        outputDir :: FilePath,
        compFile :: FilePath,
        machineFiles :: [FilePath]
    }

opts :: Parser CLIOptions
opts = CLIOptions <$>
    targetOpts
    <*> switch (short 's' <> long "with-state" <> help "Modify transition systems to output the previous state on a special channel with every transition")
    <*> strOption (short 'o' <> metavar "OUTPUT" <> help "Path to the output directory")
    <*> strOption (long "comp" <> short 'c' <> metavar "COMP" <> help "Path to composite structure specification")
    <*> many (argument str (metavar "MACHINES..." <> help "Paths to state machine specifiactions"))

desc :: ParserInfo CLIOptions
desc = info (opts <**> helper) (fullDesc <> progDesc programDescription)

main :: IO ()
main = do
    cliOpts <- execParser desc
    machinesD <- mapM readFile $ machineFiles cliOpts
    compD <- readFile $ compFile cliOpts
    let machines = readStateMachine <$> machinesD
    let comp = readCompositeStructure compD
    let sys = makeSystem comp machines
    printErrOr sys (systemToFiles cliOpts)

systemToFiles :: CLIOptions -> UMLSystem -> IO ()
systemToFiles cliOpts sys = do
    let stageUML = id
    let stageAC = if addStateOutput cliOpts then withStateOutputs else id
    let stageKIV = id
    let files = case target cliOpts of
            Graphviz -> [("system" <.> "gv", systemToGraphviz stageUML stageAC sys)]
            KIV -> systemToKIV stageUML stageAC stageKIV sys
    mapM_ (writeFileToBase $ outputDir cliOpts) files

writeFileToBase :: FilePath -> (FilePath, String) -> IO ()
writeFileToBase base (path, content) = do
    let p = base </> path
    createDirectoryIfMissing True $ takeDirectory p
    writeFile p content

systemToKIV :: (UMLSystem -> UMLSystem) -> (Program -> Program) -> ([KIVFile] -> [KIVFile]) -> UMLSystem -> [(FilePath, String)]
systemToKIV stageUML stageAC stageKIV = map (second T.unpack . KIV2File.translateKIVFile) . stageKIV . AC2KIV.translateProgram . stageAC . UML2AC.translateSystem . stageUML

systemToGraphviz :: (UMLSystem -> UMLSystem) -> (Program -> Program) -> UMLSystem -> String
systemToGraphviz stageUML stageAC = T.unpack . AC2Graphviz.translateProgram . stageAC . UML2AC.translateSystem . stageUML

printErrOr :: Show a => Either a b -> (b -> IO ()) -> IO ()
printErrOr (Left a) _ = print a
printErrOr (Right b) f = f b
