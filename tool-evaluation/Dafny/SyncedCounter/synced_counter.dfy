include "../util.dfy"
include "basic_types.dfy"

import opened Util
import opened BasicTypes

datatype Conf = conf(cxConf: XConf, qxConf: List<Message>, cyConf: YConf, qyConf: List<Message>)

predicate x_init(cx: XConf) {
    cx.ctrl == Stable && cx.xCnt == 0
}

predicate y_init(cy: YConf) {
    cy.ctrl == Counting && cy.yCnt == 0
}

predicate init(g: Conf) {
    x_init(g.cxConf) && y_init(g.cyConf) && g.qxConf == Nil && g.qyConf == Nil
}

predicate x_trans(cx1: XConf, input: Message, output: List<Message>, cx2: XConf) {
    (cx1.ctrl == Stable && input == msg(userCom, start) && output == oel(msg(yCom, inc)) && cx2.xCnt == cx1.xCnt + 1 && cx2.ctrl == WaitAck)
    || (cx1.ctrl == WaitAck && input == msg(yCom, ack) && output == Nil && cx2.xCnt == cx1.xCnt && cx2.ctrl == Stable)
}

predicate y_trans(cy1: YConf, input: Message, output: List<Message>, cy2: YConf) {
    cy1.ctrl == Counting && input == msg(xCom, inc) && output == oel(msg(xCom, ack)) && cy2.yCnt == cy1.yCnt + 1 && cy2.ctrl == Counting
}

predicate dist_post(to_dist: List<Message>, qx1: List<Message>, qx2: List<Message>, qy1: List<Message>, qy2: List<Message>) {
    match to_dist {
        case Nil => qx2 == qx1 && qy2 == qy1
        case Cons(msg(userCom, e), output) => dist_post(output, qx1, qx2, qy1, qy2)
        case Cons(msg(yCom, e), output) => dist_post(output, qx1, qx2, enqueue(msg(xCom, e), qy1), qy2)
        case Cons(msg(xCom, e), output) => dist_post(output, enqueue(msg(yCom, e), qx1), qx2, qy1, qy2)
    }
}

function dist(to_dist: List<Message>, qx: List<Message>, qy: List<Message>): (t: (List<Message>, List<Message>))
    ensures dist_post(to_dist, qx, t.0, qy, t.1)
{
    match to_dist {
        case Nil => (qx, qy)
        case Cons(msg(userCom, e), output) => dist(output, qx, qy)
        case Cons(msg(yCom, e), output) => dist(output, qx, enqueue(msg(xCom, e), qy))
        case Cons(msg(xCom, e), output) => dist(output, enqueue(msg(yCom, e), qx), qy)
    }
}

datatype ConsumeResult = X | Y | External
function consume(input: Message, qx: List<Message>, qy: List<Message>): Either<(), ConsumeResult> {
    match input {
        case msg(userCom, e) => Right(External)
        case msg(yCom, e) =>
            match qx {
                case Cons(m, _) => if m == input then Right(X) else Left(())
                case _ => Left(())
            }
        case msg(xCom, e) =>
            match qy {
                case Cons(m, _) => if m == input then Right(Y) else Left(())
                case _ => Left(())
            }
    }
}

predicate trans(g1: Conf, input: Message, output: List<Message>, g2: Conf) {
    var conf(cx1, qx1, cy1, qy1) := g1;
    var conf(cx2, qx2, cy2, qy2) := g2;
    var consumed := consume(input, qx1, qy1);
    match consumed {
        case Left(_) => false
        case Right(res) =>
        var (qx, qy) := match res {
            case External => (qx1, qy1)
            case X => (qx1.tail, qy1)
            case Y => (qx1, qy1.tail)
        };
        dist(output, qx, qy) == (qx2, qy2)
        && (
            (x_trans(cx1, input, output, cx2) && cy2 == cy1)
            || (y_trans(cy1, input, output, cy2) && cx2 == cx1)
        )
    }
}

predicate safe(g: Conf) {
    g.cxConf.ctrl == Stable ==> g.cxConf.xCnt == g.cyConf.yCnt
}

predicate invar(g: Conf) {
    (g.cxConf.ctrl == Stable && g.cyConf.ctrl == Counting && g.qxConf == Nil && g.qyConf == Nil && g.cxConf.xCnt == g.cyConf.yCnt)
    || (g.cxConf.ctrl == WaitAck && g.cyConf.ctrl == Counting && g.qxConf == Nil && length(g.qyConf) == 1 && g.cxConf.xCnt == g.cyConf.yCnt + 1)
    || (g.cxConf.ctrl == WaitAck && g.cyConf.ctrl == Counting && length(g.qxConf) == 1 && g.qyConf == Nil && g.cxConf.xCnt == g.cyConf.yCnt)
}
