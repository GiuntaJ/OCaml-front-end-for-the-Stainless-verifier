import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub146 {
  /* 컴퓨터공학과/2017-34165/김성국/2-3 */
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((x, y))) = {
    val _2 = {
      val one = ONE(NIL)
      val _3 = {
        val mone = MONE(NIL)
        (x, y) match {
          case (NIL, y) => { y }
          case (x, NIL) => { x }
          case (ZERO(xx), ZERO(yy)) => { ZERO(crazy2add(xx, yy)) }
          case (ZERO(xx), ONE(yy)) => { ONE(crazy2add(xx, yy)) }
          case (ZERO(xx), MONE(yy)) => { MONE(crazy2add(xx, yy)) }
          case (ONE(xx), ZERO(yy)) => { ONE(crazy2add(xx, yy)) }
          case (ONE(xx), ONE(yy)) => { ZERO(crazy2add(one, crazy2add(xx, yy))) }
          case (ONE(xx), MONE(yy)) => { ZERO(crazy2add(xx, yy)) }
          case (MONE(xx), ZERO(yy)) => { MONE(crazy2add(xx, yy)) }
          case (MONE(xx), ONE(yy)) => { ZERO(crazy2add(xx, yy)) }
          case (MONE(xx), MONE(yy)) => {
            ZERO(crazy2add(mone, crazy2add(xx, yy)))
          }
        }
      }
    }
  }
}