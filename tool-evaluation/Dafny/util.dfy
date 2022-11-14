module Util {
    datatype Either<a, b> = Left(a) | Right(b)

    datatype List<a> = Nil | Cons(head: a, tail: List<a>)

    function oel<a>(x: a): List<a> {
        Cons(x, Nil)
    }

    function length<a>(l: List<a>): (len: nat)
        ensures if is_empty(l) then len == 0 else len > 0
    {
        match l {
            case Nil => 0
            case Cons(_, t) => 1 + length(t)
        }
    }

    function is_empty<a>(l: List<a>): bool {
        match l {
            case Nil => true
            case _ => false
        }
    }

    function contains<a>(l: List<a>, x: a): bool {
        match l {
            case Nil => false
            case Cons(y, t) => x == y || contains(t, x)
        }
    }

    function enqueue<a>(x: a, l: List<a>): List<a> {
        match l {
            case Nil => oel(x)
            case Cons(head, tail) => Cons(head, enqueue(x, tail))
        }
    }
}
