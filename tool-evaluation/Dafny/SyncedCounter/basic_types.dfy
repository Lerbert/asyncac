module BasicTypes {
    datatype Event = start | inc | ack
    datatype Port = userCom | xCom | yCom
    datatype Message = msg(portMsg: Port, evtMsg: Event)
    datatype XCtrl = Stable | WaitAck
    datatype XConf = xConf(ctrl: XCtrl, xCnt: nat)
    datatype YCtrl = Counting
    datatype YConf = yConf(ctrl: YCtrl, yCnt: nat)
}
