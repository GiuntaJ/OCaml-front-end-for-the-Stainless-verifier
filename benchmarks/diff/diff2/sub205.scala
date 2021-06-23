import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub205 {
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
          def df(((e, x))) = {
            e match {
              case Sum(Cons(hd, tl)) => { Sum(List(df(hd, x), df(Sum(tl), x))) }
              case Sum(_) => { Const(0) }
              case Times(Cons(hd, tl)) => {
                
                  if (
                    hd == Var(x)
                  ) {
                    Times(tl ++ List(Const(1))) 
                  } else {
                    Times(List(hd, df(Times(tl), x)))
                  }
              }
              case Times(_) => { Times(List(Const(0))) }
              case Power(s, i) => {
                if (s == x) Times(List(Const(i), Var(s))) else Const(0)
              }
              case Const(i) => { Const(0) }
              case Var(s) => { if (s == x) Const(1) else Const(0) }
            }
          }
          df(e, x)
        }
    }
  }
}