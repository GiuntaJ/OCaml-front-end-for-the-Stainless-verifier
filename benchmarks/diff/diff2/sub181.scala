import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub181 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  val diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        val _2 = {
          def diff_list: (Aexp, String) => List[Aexp] = {
            case (e, x) =>
              {
                e match {
                  case Sum(Cons(Const(a), Nil())) => { List(Const(0)) }
                  case Sum(Cons(Var(x), tl)) => { List(Const(1)) }
                  case Sum(Cons(Times(Cons(Const(a), Cons(Var(x), Nil()))), tl)) => {
                    List(Const(a))
                  }
                  case Sum(Cons(Power(x, 1), tl)) => { List(Const(1)) }
                  case Sum(Cons(Times(Cons(Const(a), Cons(Power(x, 1), Nil()))), tl)) => {
                    List(Const(a))
                  }
                  case Sum(Cons(Power(x, 2), tl)) => {
                    Times(List(Const(2), Var(x))) :: diff_list(Sum(tl), x)
                  }
                  case Sum(Cons(Times(Cons(Const(a), Cons(Power(x, 2), Nil()))), tl)) => {
                    Times(List(Const(a * 2), Var(x))) :: diff_list(Sum(tl), x)
                  }
                  case Sum(Cons(Power(x, n), tl)) => {
                    Times(List(Const(n), Power(x, n - 1))) ::
                    diff_list(Sum(tl), x)
                  }
                  case Sum(Cons(Times(Cons(Const(a), Cons(Power(x, n), Nil()))), tl)) => {
                    Times(List(Const(a * n), Power(x, n - 1))) ::
                    diff_list(Sum(tl), x)
                  }
                  case _ => { assert(false, "Failure with Error") }
                }
            }
          }
          Sum(diff_list(e, x))
        }
    }
  }
}