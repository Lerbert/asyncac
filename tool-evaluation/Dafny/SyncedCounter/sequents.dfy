include "synced_counter.dfy"

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

lemma trans1(g1: Conf, g2: Conf)
    requires
        var conf(cx, qx, cy, qy) := g1;
        invar(g1)
        && g1.cxConf.ctrl == Stable
        && g2 == conf(xConf(WaitAck, cx.xCnt + 1), qx, cy, enqueue(msg(xCom, inc), qy))
    ensures invar(g2)
{}

lemma trans2(g1: Conf, g2: Conf)
    requires 
        var conf(cx, qx, cy, qy) := g1;
        invar(g1)
        && cx.ctrl == WaitAck
        && !is_empty(qx) && qx.head == msg(yCom, ack)
        && g2 == conf(xConf(Stable, cx.xCnt), qx.tail, cy, qy)
    ensures invar(g2)
{}

lemma trans3(g1: Conf, g2: Conf)
    requires
        var conf(cx, qx, cy, qy) := g1;
        invar(g1)
        && cy.ctrl == Counting
        && !is_empty(qy) && qy.head == msg(xCom, inc)
        && g2 == conf(cx, enqueue(msg(yCom, ack), qx), yConf(Counting, cy.yCnt + 1), qy.tail)
    ensures invar(g2)
{}

lemma trans_ex1(cy: YConf)
    ensures trans(conf(xConf(Stable,  2), Nil, cy, Nil), msg(userCom, start), oel(msg(yCom, inc)), conf(xConf(WaitAck,  3), Nil, cy, enqueue(msg(xCom, inc), Nil)))
{}

lemma trans_ex2(cy: YConf)
    ensures trans(conf(xConf(WaitAck, 2), enqueue(msg(yCom, ack), Nil), cy, Nil), msg(yCom, ack), Nil, conf(xConf(Stable, 2), Nil, cy, Nil))
{}

lemma trans_ex3(cy: YConf)
    ensures !trans(conf(xConf(WaitAck, 2), enqueue(msg(yCom, inc), Nil), cy, Nil), msg(yCom, inc), Nil, conf(xConf(Stable, 2), Nil, cy, Nil))
{}

lemma trans_ex4(cy: YConf)
    ensures !trans(conf(xConf(WaitAck, 2), Nil, cy, Nil), msg(yCom, ack), Nil, conf(xConf(Stable, 2), Nil, cy, Nil))
{}
