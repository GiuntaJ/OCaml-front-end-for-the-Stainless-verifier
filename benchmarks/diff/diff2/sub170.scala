import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub170 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        e match {
          case Const(_) => { Const(0) }
          case Var(y) => { if (x == y) Const(1) else Const(0) }
          case Power(y, z) => {
            
              if (
                not(x == y)
              ) {
                Const(0) 
              } else {
                Times(List(Const(z), Power(y, z - 1)))
              }
          }
          case Times(Nil()) => { Const(0) }
          case Times(Cons(hd, tl)) => {
            Sum(
              List(Times(diff(hd, x) :: tl),
               Times(List(hd, diff(Times(tl), x)))))
          }
          case Sum(aexps) => { Sum(aexps.map(( (ae) => { diff(ae, x) } ))) }
        }
    }
  }
}