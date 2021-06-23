import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub112 {
  /*Computer Science Engineering 2015-12683 Kim Jaein*/
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((x: Crazy2, y: Crazy2))) = {
    x match {
      case NIL => {
        y match {
          case NIL => { NIL }
          case _ => { y }
        }
      }
      case ZERO(xnext) => {
        y match {
          case NIL => { x }
          case ZERO(ynext) => { ZERO(crazy2add(xnext, ynext)) }
          case ONE(ynext) => { ONE(crazy2add(xnext, ynext)) }
          case MONE(ynext) => { MONE(crazy2add(xnext, ynext)) }
        }
      }
      case ONE(xnext) => {
        y match {
          case NIL => { x }
          case ZERO(ynext) => { ONE(crazy2add(xnext, ynext)) }
          case ONE(ynext) => {
            ZERO(crazy2add(crazy2add(ONE(NIL), xnext), ynext))
          }
          case MONE(ynext) => { ZERO(crazy2add(xnext, ynext)) }
        }
      }
      case MONE(xnext) => {
        y match {
          case NIL => { x }
          case ZERO(ynext) => { MONE(crazy2add(xnext, ynext)) }
          case ONE(ynext) => { ZERO(crazy2add(xnext, ynext)) }
          case MONE(ynext) => {
            ZERO(crazy2add(crazy2add(MONE(NIL), xnext), ynext))
          }
        }
      }
    }
  }
}
