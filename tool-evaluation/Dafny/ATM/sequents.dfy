include "atm.dfy"

lemma invar_safe(g: Conf)
    requires invar(g)
    ensures safe(g)
{}

lemma invar_init(g: Conf)
    requires init(g)
    ensures invar(g)
{}

lemma invar_trans(g1: Conf, input: Message, output: List<Message>, g2: Conf)
    requires invar(g1) && trans(g1, input, output, g2)
    ensures invar(g2)
{}

lemma trans1(g1: Conf, c: CardId, g2: Conf)
    requires
        var conf(ca, qa, cb, qb) := g1;
        invar(g1)
        && ca.ctrl == AtmCtrl.Idle
        && g2 == conf(atmConf(CardEntered, c, ca.pin, ca.trialsNum), qa, cb, qb)
    ensures invar(g2)
{}

lemma trans2(g1: Conf, p: Pin, g2: Conf)
    requires
        var conf(ca, qa, cb, qb) := g1;
        invar(g1)
        && ca.ctrl == AtmCtrl.CardEntered
        && g2 == conf(atmConf(PINEntered, ca.cardId, p, ca.trialsNum), enqueue(msg(atmCompl, PINEnteredCompl), qa), cb, qb)
    ensures invar(g2)
{}

lemma trans3(g1: Conf, g2: Conf)
    requires
        var conf(ca, qa, cb, qb) := g1;
        invar(g1)
        && ca.ctrl == AtmCtrl.PINEntered
        && !is_empty(qa) && qa.head == msg(atmCompl, PINEnteredCompl)
        && g2 == conf(atmConf(AtmCtrl.Verifying, ca.cardId, ca.pin, ca.trialsNum), qa.tail, cb, enqueue(msg(atmCom, verify(ca.cardId, ca.pin)), qb))
    ensures invar(g2)
{}

lemma trans4(g1: Conf, g2: Conf)
    requires
        var conf(ca, qa, cb, qb) := g1;
        invar(g1)
        && ca.ctrl == AtmCtrl.Verifying
        && !is_empty(qa) && qa.head == msg(bankCom, reenterPIN)
        && ca.trialsNum < 3
        && g2 == conf(atmConf(AtmCtrl.CardEntered, ca.cardId, ca.pin, ca.trialsNum + 1), qa.tail, cb, qb)
    ensures invar(g2)
{}

lemma trans5(g1: Conf, g2: Conf)
    requires
        var conf(ca, qa, cb, qb) := g1;
        invar(g1)
        && ca.ctrl == AtmCtrl.Verifying
        && !is_empty(qa) && qa.head == msg(bankCom, reenterPIN)
        && ca.trialsNum >= 3
        && g2 == conf(atmConf(AtmCtrl.Idle, ca.cardId, ca.pin, 0), qa.tail, cb, qb)
    ensures invar(g2)
{}

lemma trans6(g1: Conf, g2: Conf)
    requires
        var conf(ca, qa, cb, qb) := g1;
        invar(g1)
        && ca.ctrl == AtmCtrl.Verifying
        && !is_empty(qa) && qa.head == msg(atmCompl, VerifyingCompl)
        && g2 == conf(atmConf(AtmCtrl.Verified, ca.cardId, ca.pin, ca.trialsNum), qa.tail, cb, qb)
    ensures invar(g2)
{}

lemma trans7(g1: Conf, g2: Conf)
    requires
        var conf(ca, qa, cb, qb) := g1;
        invar(g1)
        && ca.ctrl == AtmCtrl.Verified
        && !is_empty(qa) && qa.head == msg(atmCompl, VerifiedCompl)
        && g2 == conf(atmConf(AtmCtrl.Idle, ca.cardId, ca.pin, 0), qa.tail, cb, qb)
    ensures invar(g2)
{}

lemma trans8(g1: Conf, c: CardId, p: Pin, g2: Conf)
    requires
        var conf(ca, qa, cb, qb) := g1;
        invar(g1)
        && cb.ctrl == BankCtrl.Idle
        && !is_empty(qb) && qb.head == msg(atmCom, verify(c, p))
        && g2 == conf(ca, qa, bankConf(BankCtrl.Verifying, 0), enqueue(msg(bankCompl, VerifyingCompl), qb.tail))
    ensures invar(g2)
{}


lemma trans9(g1: Conf, g2: Conf)
    requires
        var conf(ca, qa, cb, qb) := g1;
        invar(g1)
        && cb.ctrl == BankCtrl.Verifying
        && !is_empty(qb) && qb.head == msg(bankCompl, VerifyingCompl)
        && g2 == conf(ca, qa, bankConf(BankCtrl.VeriSuccess, 1), enqueue(msg(bankCompl, VeriSuccessCompl), qb.tail))
    ensures invar(g2)
{}

lemma trans10(g1: Conf, g2: Conf)
    requires
        var conf(ca, qa, cb, qb) := g1;
        invar(g1)
        && cb.ctrl == BankCtrl.Verifying
        && !is_empty(qb) && qb.head == msg(bankCompl, VerifyingCompl)
        && g2 == conf(ca, qa, bankConf(BankCtrl.VeriFail, cb.wasVerified), enqueue(msg(bankCompl, VeriFailCompl), qb.tail))
    ensures invar(g2)
{}

lemma trans11(g1: Conf, g2: Conf)
    requires
        var conf(ca, qa, cb, qb) := g1;
        invar(g1)
        && cb.ctrl == BankCtrl.VeriSuccess
        && !is_empty(qb) && qb.head == msg(bankCompl, VeriSuccessCompl)
        && g2 == conf(ca, enqueue(msg(bankCom, verified), qa), bankConf(BankCtrl.Idle, cb.wasVerified), qb.tail)
    ensures invar(g2)
{}

lemma trans12(g1: Conf, g2: Conf)
    requires
        var conf(ca, qa, cb, qb) := g1;
        invar(g1)
        && cb.ctrl == BankCtrl.VeriFail
        && !is_empty(qb) && qb.head == msg(bankCompl, VeriFailCompl)
        && g2 == conf(ca, enqueue(msg(bankCom, reenterPIN), qa), bankConf(BankCtrl.Idle, cb.wasVerified), qb.tail)
    ensures invar(g2)
{}

lemma trans_ex1(c: CardId, p: Pin, qa: List<Message>, cb: BankConf, qb: List<Message>)
    ensures 
        var m := msg(bankCom, reenterPIN);
        var ca1 := atmConf(AtmCtrl.Verifying, c, p, 3);
        var ca2 := atmConf(AtmCtrl.Idle, c, p, 0);
        trans(conf(ca1, Cons(m, qa), cb, qb), m, oel(msg(userCom, keepCard)), conf(ca2, qa, cb, qb))
{}

lemma trans_ex2(c: CardId, p: Pin, t: nat, qa: List<Message>, cb: BankConf, qb: List<Message>)
    ensures
        var i := msg(bankCom, verified);
        var o := msg(atmCompl, VerifiedCompl);
        var ca1 := atmConf(AtmCtrl.Verifying, c, p, t);
        var ca2 := atmConf(AtmCtrl.Verified, c, p, t);
        trans(conf(ca1, Cons(i, qa), cb, qb), i, oel(o), conf(ca2, enqueue(o, qa), cb, qb))
{}

lemma trans_ex3(c: CardId, p: Pin, t: nat, qa: List<Message>, cb: BankConf, qb: List<Message>)
    ensures
        var i := msg(atmCompl, PINEnteredCompl);
        var o_ev := verify(c, p);
        var ca1 := atmConf(AtmCtrl.PINEntered, c, p, t);
        var ca2 := atmConf(AtmCtrl.Verifying, c, p, t);
        trans(conf(ca1, Cons(i, qa), cb, qb), i, oel(msg(bankCom, o_ev)), conf(ca2, qa, cb, enqueue(msg(atmCom, o_ev), qb)))
{
    var i := msg(atmCompl, PINEnteredCompl);
    var o_ev := verify(c, p);
    var ca1 := atmConf(AtmCtrl.PINEntered, c, p, t);
    var ca2 := atmConf(AtmCtrl.Verifying, c, p, t);
    assert atm_trans(ca1, i, oel(msg(bankCom, o_ev)), ca2);
}

lemma trans_ex4(c: CardId, p: Pin, w: nat, ca: AtmConf, qa: List<Message>, qb: List<Message>)
    ensures
        var i := msg(atmCom, verify(c, p));
        var o := msg(bankCompl, VerifyingCompl);
        var cb1 := bankConf(BankCtrl.Idle, w);
        var cb2 := bankConf(BankCtrl.Verifying, 0);
        trans(conf(ca, qa, cb1, Cons(i, qb)), i, oel(o), conf(ca, qa, cb2, enqueue(o, qb)))
{}

lemma trans_ex5(w: nat, ca: AtmConf, qa: List<Message>, qb: List<Message>)
    ensures
        var i := msg(bankCompl, VerifyingCompl);
        var o := msg(bankCompl, VeriSuccessCompl);
        var cb1 := bankConf(BankCtrl.Verifying, w);
        var cb2 := bankConf(BankCtrl.VeriSuccess, 1);
        trans(conf(ca, qa, cb1, Cons(i, qb)), i, oel(o), conf(ca, qa, cb2, enqueue(o, qb)))
{}

lemma trans_ex6(w: nat, ca: AtmConf, qa: List<Message>, qb: List<Message>)
    ensures
        var i := msg(bankCompl, VerifyingCompl);
        var o := msg(bankCompl, VeriFailCompl);
        var cb1 := bankConf(BankCtrl.Verifying, w);
        var cb2 := bankConf(BankCtrl.VeriFail, w);
        trans(conf(ca, qa, cb1, Cons(i, qb)), i, oel(o), conf(ca, qa, cb2, enqueue(o, qb)))
{}
