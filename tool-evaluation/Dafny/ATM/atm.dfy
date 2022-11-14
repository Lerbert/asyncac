include "../util.dfy"
include "basic_types.dfy"

import opened Util
import opened BasicTypes

datatype Conf = conf(ca: AtmConf, qa: List<Message>, cb: BankConf, qb: List<Message>)

predicate atm_init(ca: AtmConf) {
    ca.ctrl == AtmCtrl.Idle
}

predicate bank_init(cb: BankConf) {
    cb.ctrl == BankCtrl.Idle
}

predicate init(g: Conf) {
    atm_init(g.ca) && bank_init(g.cb) && g.qa == Nil && g.qb == Nil
}

predicate atm_trans(ca1: AtmConf, input: Message, output: List<Message>, ca2: AtmConf) {
    (exists c : CardId :: ca1.ctrl == AtmCtrl.Idle && input == msg(userCom, card(c)) && output == Nil && ca2.ctrl == CardEntered && ca2.cardId == c && ca2.pin == ca1.pin && ca2.trialsNum == ca1.trialsNum)
    || (exists p : Pin :: ca1.ctrl == CardEntered && input == msg(userCom, PIN(p)) && output == oel(msg(atmCompl, PINEnteredCompl)) && ca2.pin == p && ca2.ctrl == PINEntered && ca2.cardId == ca1.cardId && ca2.trialsNum == ca1.trialsNum)
    || (ca1.ctrl == PINEntered && input == msg(atmCompl, PINEnteredCompl) && output == oel(msg(bankCom, verify(ca1.cardId, ca1.pin))) && ca2.ctrl == AtmCtrl.Verifying && ca2.cardId == ca1.cardId && ca2.pin == ca1.pin && ca2.trialsNum == ca1.trialsNum)
    || (ca1.ctrl == AtmCtrl.Verifying && input == msg(bankCom, reenterPIN) && ca1.trialsNum < 3 && output == Nil && ca2.ctrl == CardEntered && ca2.cardId == ca1.cardId && ca2.pin == ca1.pin && ca2.trialsNum == ca1.trialsNum + 1)
    || (ca1.ctrl == AtmCtrl.Verifying && input == msg(bankCom, reenterPIN) && ca1.trialsNum >= 3 && output == oel(msg(userCom, keepCard)) && ca2.ctrl == AtmCtrl.Idle && ca2.cardId == ca1.cardId && ca2.pin == ca1.pin && ca2.trialsNum == 0)
    || (ca1.ctrl == AtmCtrl.Verifying && input == msg(bankCom, verified) && output == oel(msg(atmCompl, VerifiedCompl)) && ca2.ctrl == Verified && ca2.cardId == ca1.cardId && ca2.pin == ca1.pin && ca2.trialsNum == ca1.trialsNum)
    || (ca1.ctrl == Verified && input == msg(atmCompl, VerifiedCompl) && output == oel(msg(userCom, ejectCard)) && ca2.ctrl == AtmCtrl.Idle && ca2.cardId == ca1.cardId && ca2.pin == ca1.pin && ca2.trialsNum == 0)
}

predicate bank_trans(cb1: BankConf, input: Message, output: List<Message>, cb2: BankConf) {
    (exists c : CardId, p : Pin :: cb1.ctrl == BankCtrl.Idle && input == msg(atmCom, verify(c, p)) && output == oel(msg(bankCompl, VerifyingCompl)) && cb2.ctrl == BankCtrl.Verifying && cb2.wasVerified == 0)
    || (cb1.ctrl == BankCtrl.Verifying && input == msg(bankCompl, VerifyingCompl) && output == oel(msg(bankCompl, VeriSuccessCompl)) && cb2.ctrl == BankCtrl.VeriSuccess && cb2.wasVerified == 1)
    || (cb1.ctrl == BankCtrl.Verifying && input == msg(bankCompl, VerifyingCompl) && output == oel(msg(bankCompl, VeriFailCompl)) && cb2.ctrl == BankCtrl.VeriFail && cb2.wasVerified == cb1.wasVerified)
    || (cb1.ctrl == BankCtrl.VeriSuccess && input == msg(bankCompl, VeriSuccessCompl) && output == oel(msg(atmCom, verified)) && cb2.ctrl == BankCtrl.Idle && cb2.wasVerified == cb1.wasVerified)
    || (cb1.ctrl == BankCtrl.VeriFail && input == msg(bankCompl, VeriFailCompl) && output == oel(msg(atmCom, reenterPIN)) && cb2.ctrl == BankCtrl.Idle && cb2.wasVerified == cb1.wasVerified)
}

predicate dist_post(to_dist: List<Message>, qa1: List<Message>, qa2: List<Message>, qb1: List<Message>, qb2: List<Message>) {
    match to_dist {
        case Nil => qa2 == qa1 && qb2 == qb1
        case Cons(msg(userCom, e), output) => dist_post(output, qa1, qa2, qb1, qb2)
        case Cons(msg(bankCom, e), output) => dist_post(output, qa1, qa2, enqueue(msg(atmCom, e), qb1), qb2)
        case Cons(msg(atmCom, e), output) => dist_post(output, enqueue(msg(bankCom, e), qa1), qa2, qb1, qb2)
        case Cons(msg(bankCompl, e), output) => dist_post(output, qa1, qa2, enqueue(msg(bankCompl, e), qb1), qb2)
        case Cons(msg(atmCompl, e), output) => dist_post(output, enqueue(msg(atmCompl, e), qa1), qa2, qb1, qb2)
    }
}

function dist(to_dist: List<Message>, qa: List<Message>, qb: List<Message>): (t: (List<Message>, List<Message>))
    ensures dist_post(to_dist, qa, t.0, qb, t.1)
{
    match to_dist {
        case Nil => (qa, qb)
        case Cons(msg(userCom, e), output) => dist(output, qa, qb)
        case Cons(msg(bankCom, e), output) => dist(output, qa, enqueue(msg(atmCom, e), qb))
        case Cons(msg(atmCom, e), output) => dist(output, enqueue(msg(bankCom, e), qa), qb)
        case Cons(msg(bankCompl, e), output) => dist(output, qa, enqueue(msg(bankCompl, e), qb))
        case Cons(msg(atmCompl, e), output) => dist(output, enqueue(msg(atmCompl, e), qa), qb)
    }
}

datatype ConsumeResult = Atm | Bank | External
function consume(input: Message, qa: List<Message>, qb: List<Message>): Either<(), ConsumeResult> {
    match input {
        case msg(userCom, e) => Right(External)
        case msg(bankCom, e) =>
            match qa {
                case Cons(m, _) => if m == input then Right(Atm) else Left(())
                case _ => Left(())
            }
        case msg(atmCom, e) =>
            match qb {
                case Cons(m, _) => if m == input then Right(Bank) else Left(())
                case _ => Left(())
            }
        case msg(atmCompl, e) =>
            match qa {
                case Cons(m, _) => if m == input then Right(Atm) else Left(())
                case _ => Left(())
            }
        case msg(bankCompl, e) =>
            match qb {
                case Cons(m, _) => if m == input then Right(Bank) else Left(())
                case _ => Left(())
            }
    }
}

predicate trans(g1: Conf, input: Message, output: List<Message>, g2: Conf) {
    var conf(ca1, qa1, cb1, qb1) := g1;
    var conf(ca2, qa2, cb2, qb2) := g2;
    var consumed := consume(input, qa1, qb1);
    match consumed {
        case Left(_) => false
        case Right(res) =>
        var (qa, qb) := match res {
            case External => (qa1, qb1)
            case Atm => (qa1.tail, qb1)
            case Bank => (qa1, qb1.tail)
        };
        dist(output, qa, qb) == (qa2, qb2)
        && (
            (atm_trans(ca1, input, output, ca2) && cb2 == cb1)
            || (bank_trans(cb1, input, output, cb2) && ca2 == ca1)
        )
    }
}

predicate safe(g: Conf) {
    g.ca.ctrl == AtmCtrl.Verified ==> g.cb.wasVerified == 1
}

predicate invar(g: Conf) {
    var conf(ca, qa, cb, qb) := g;
    (ca.ctrl == AtmCtrl.Idle && cb.ctrl == BankCtrl.Idle && qa == Nil && qb == Nil)
    || (ca.ctrl == CardEntered && cb.ctrl == BankCtrl.Idle && qa == Nil && qb == Nil)
    || (ca.ctrl == PINEntered && cb.ctrl == BankCtrl.Idle && length(qa) == 1 && qb == Nil)
    || (ca.ctrl == AtmCtrl.Verifying && cb.ctrl == BankCtrl.Idle && qa == Nil && length(qb) == 1)
    || (ca.ctrl == AtmCtrl.Verifying && cb.ctrl == BankCtrl.Verifying && qa == Nil && length(qb) == 1)
    || (ca.ctrl == AtmCtrl.Verifying && cb.ctrl == BankCtrl.VeriSuccess && qa == Nil && length(qb) == 1 && cb.wasVerified == 1)
    || (ca.ctrl == AtmCtrl.Verifying && cb.ctrl == BankCtrl.VeriFail && qa == Nil && length(qb) == 1)
    || (ca.ctrl == AtmCtrl.Verifying && cb.ctrl == BankCtrl.Idle && qa == enqueue(msg(bankCom, reenterPIN), Nil) && qb == Nil)
    || (ca.ctrl == AtmCtrl.Verifying && cb.ctrl == BankCtrl.Idle && qa == enqueue(msg(bankCom, verified), Nil) && qb == Nil && cb.wasVerified == 1)
    || (ca.ctrl == AtmCtrl.Verified && cb.ctrl == BankCtrl.Idle && length(qa) == 1 && qb == Nil && cb.wasVerified == 1)
}
