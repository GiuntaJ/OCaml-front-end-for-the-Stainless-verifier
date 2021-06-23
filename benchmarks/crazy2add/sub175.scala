import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub175 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((x: Crazy2, y: Crazy2))): Crazy2 = {
    (x, y) match {
      case (NIL, y) => { y }
      case (x, NIL) => { x }
      case (ONE(xtl), ZERO(ytl)) | (ZERO(xtl), ONE(ytl)) => {
        ONE(crazy2add(xtl, ytl))
      }
      case (MONE(xtl), ZERO(ytl)) | (ZERO(xtl), MONE(ytl)) => {
        MONE(crazy2add(xtl, ytl))
      }
      case (ZERO(xtl), ZERO(ytl)) | (MONE(xtl), ONE(ytl)) |
      (ONE(xtl), MONE(ytl)) => {
        ZERO(crazy2add(xtl, ytl))
      }
      case (MONE(xtl), MONE(ytl)) => {
        ZERO(crazy2add(crazy2add(xtl, ytl), MONE(NIL)))
      }
      case (ONE(xtl), ONE(ytl)) => {
        ZERO(crazy2add(crazy2add(xtl, ytl), ONE(NIL)))
      }
    }
  }
}