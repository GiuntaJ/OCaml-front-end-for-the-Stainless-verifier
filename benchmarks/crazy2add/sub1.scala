import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub1 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  sealed case class Error(param0: String) extends Exception {}
  def crazy2add(((x: Crazy2, y: Crazy2))) = {
    
      if (
        x == NIL
      ) {
        assert(false, "Error with Invalid input : NIL ") 
      } else if (
        y == NIL
      ) {
        assert(false, "Error with Invalid input : NIL ") 
      } else {
        val _0 = {
          def tempadd(((x, y))) = {
            (x, y) match {
              case (NIL, NIL) => { NIL }
              case (NIL, ZERO(b)) => { ZERO(tempadd(NIL, b)) }
              case (NIL, ONE(b)) => { ONE(tempadd(NIL, b)) }
              case (NIL, MONE(b)) => { MONE(tempadd(NIL, b)) }
              case (ZERO(a), ZERO(c)) => { ZERO(tempadd(a, c)) }
              case (ZERO(a), ONE(c)) => { ONE(tempadd(a, c)) }
              case (ZERO(a), MONE(c)) => { MONE(tempadd(a, c)) }
              case (ONE(a), ONE(b)) => { ZERO(tempadd(tempadd(a, b), ONE(NIL)))
              }
              case (ONE(a), MONE(b)) => { ZERO(tempadd(a, b)) }
              case (MONE(a), MONE(b)) => {
                ZERO(tempadd(tempadd(a, b), MONE(NIL)))
              }
              case _ => { tempadd(y, x) }
            }
          }
          val _1 = {
            def checkZERO(x) = {
              x match {
                case ZERO(NIL) => { ZERO(NIL) }
                case ZERO(a) => {
                  if (checkZERO(a) == ZERO(NIL)) ZERO(NIL) else ZERO(a)
                }
                case ONE(a) => { ONE(a) }
                case MONE(a) => { MONE(a) }
                case NIL => { NIL }
              }
            }
            checkZERO(tempadd(x, y))
          }
        }
      }
  }
}