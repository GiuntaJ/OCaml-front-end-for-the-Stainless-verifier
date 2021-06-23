import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub185 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        a match {
          case NIL => { b }
          case ZERO(aa) => {
            b match {
              case NIL => { a }
              case ZERO(bb) => { ZERO(crazy2add(aa, bb)) }
              case ONE(bb) => { ONE(crazy2add(aa, bb)) }
              case MONE(bb) => { MONE(crazy2add(aa, bb)) }
            }
          }
          case ONE(aa) => {
            b match {
              case NIL => { a }
              case ZERO(bb) => { ONE(crazy2add(aa, bb)) }
              case ONE(bb) => { ZERO(crazy2add(aa, crazy2add(ONE(NIL), bb))) }
              case MONE(bb) => { ZERO(crazy2add(aa, bb)) }
            }
          }
          case MONE(aa) => {
            b match {
              case NIL => { a }
              case ZERO(bb) => { MONE(crazy2add(aa, bb)) }
              case ONE(bb) => { ZERO(crazy2add(aa, bb)) }
              case MONE(bb) => { ZERO(crazy2add(aa, crazy2add(MONE(NIL), bb))) }
            }
          }
        }
    }
  }
}
