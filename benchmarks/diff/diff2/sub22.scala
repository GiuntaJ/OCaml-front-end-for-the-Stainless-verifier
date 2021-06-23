import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub22 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Sum(list1) => {
            list1 match {
              case Cons(hd, tl) => { hd }
            }
          }
          case Const(x) => { Const(x) }
          case Var(y) => { Var(y) }
          case Power(z1, z2) => {
            
              if (
                z2 == 2
              ) {
                Times(List(Const(z2), Var(z1))) 
              } else {
                Times(List(Const(z2), Power(z1, z2 - 1)))
              }
          }
          case Times(list2) => { Const(2) }
        }
    }
  }
}
