module Translation.AsyncAC2Graphviz where

import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified AsyncAC.Types as AC
import Translation.Common2Text
import AsyncAC.Program.Types (realiseInstances)

translateProgram :: AC.Program -> T.Text
translateProgram (AC.Program _ _ _ instances) = T.intercalate (T.singleton '\n') (translateTransitionSystem <$> systems)
    where systems = concat $ catMaybes $ realiseInstances <$> instances

translateTransitionSystem :: AC.TransitionSystem -> T.Text
translateTransitionSystem (AC.TransitionSystem name ctrlStates _ _ _ transitions initCtrl initPred _ _) = T.pack "digraph " <> nameT <> T.pack " {" <> ctrlStatesT <> semi <> transitionsT <> semi <> initT <> T.pack ";}"
    where nameT = translateTransitionSystemName name
          ctrlStatesT = translateSet ";" translateControlState ctrlStates
          transitionsT = translateSet ";" translateTrans transitions
          initT = T.pack "§init§[style=invis];§init§ -> " <> translateControlState initCtrl <> T.pack " [label=\"" <> translateGuard initPred <> T.pack "\"]"
          semi = T.singleton ';'

translateTransitionSystemName :: AC.TransitionSystemName -> T.Text
translateTransitionSystemName (AC.TransitionSystemName n) = T.pack n

translateControlState :: AC.ControlState -> T.Text
translateControlState (AC.ControlState n) = T.pack n

translateChannel :: AC.Channel -> T.Text
translateChannel (AC.Channel c) = T.pack c

translateEventName :: AC.EventName -> T.Text
translateEventName (AC.EventName e) = T.pack e

translateAssignment :: AC.Assignment -> T.Text
translateAssignment (AC.Assignment var expr) = varT <> T.pack " := " <> exprT
    where varT = translateVariable var
          exprT = translateExpression expr

translateReceiveDecl :: AC.ReceiveDecl -> T.Text
translateReceiveDecl (AC.ReceiveDecl channel evName params) = channelT <> T.singleton '?' <> evNameT <> paramsT
    where channelT = translateChannel channel
          evNameT = translateEventName evName
          paramsT = translateParams $ map translateVariable params

translateSendDecl :: AC.SendDecl -> T.Text
translateSendDecl (AC.SendDecl channel evName params) = channelT <> T.singleton '!' <> evNameT <> paramsT
    where channelT = translateChannel channel
          evNameT = translateEventName evName
          paramsT = translateParams $ map translateExpression params

translateTrans :: AC.Trans -> T.Text
translateTrans (AC.Trans src receives guard sends assigns dst) = srcT <> T.pack " -> " <> dstT <> T.pack " [label=\"" <> label <> T.pack "\"]"
    where srcT = translateControlState src
          dstT = translateControlState dst
          label = receivesT <> guardT <> actionsT
          receivesT = translateSet ", " translateReceiveDecl receives
          guardT = translateGuard guard
          actionsT = let sep1 = if Set.null sends && null assigns then T.empty else T.pack " / " in
            let sep2 = if Set.null sends || null assigns then T.empty else T.pack "; " in
                sep1 <> sendsT <> sep2 <> assignsT
          sendsT = translateSet ", " translateSendDecl sends
          assignsT = translateSet ", " translateAssignment assigns

translateGuard :: Maybe AC.Formula -> T.Text
translateGuard = maybe T.empty mkGuardT
    where mkGuardT g = T.pack "[" <> translateFormula g <> T.pack "]"

translateSet sep trans = T.intercalate (T.pack sep) . foldr ((:) . trans) []

translateParams [] = T.empty
translateParams p = T.singleton '(' <> T.intercalate (T.pack ", ") p <> T.singleton ')'
