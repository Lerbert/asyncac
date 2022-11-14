module BasicTypes {
    newtype CardId = int
    newtype Pin = int
    datatype Event = card(id: CardId)
        | PIN(pin: Pin)
        | verify(card: CardId, pin: Pin)
        | reenterPIN
        | keepCard
        | verified
        | ejectCard
        | PINEnteredCompl
        | VerifiedCompl
        | VerifyingCompl
        | VeriSuccessCompl
        | VeriFailCompl
    datatype Port = userCom | bankCom | atmCom | atmCompl | bankCompl
    datatype Message = msg(port: Port, evt: Event)
    datatype AtmCtrl = Idle | CardEntered | PINEntered | Verifying | Verified
    datatype AtmConf = atmConf(ctrl: AtmCtrl, cardId: CardId, pin: Pin, trialsNum: nat)
    datatype BankCtrl = Idle | Verifying | VeriSuccess | VeriFail
    datatype BankConf = bankConf(ctrl: BankCtrl, wasVerified: nat)
}
