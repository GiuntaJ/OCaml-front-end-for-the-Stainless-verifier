import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub44 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) =>
      {
        c1 match {
          case NIL => { c2 }
          case ZERO(c1in) => {
            c2 match {
              case NIL => { c1 }
              case ZERO(c2in) => { ZERO(crazy2add(c1in, c2in)) }
              case ONE(c2in) => { ONE(crazy2add(c1in, c2in)) }
              case MONE(c2in) => { MONE(crazy2add(c1in, c2in)) }
            }
          }
          case ONE(c1in) => {
            c2 match {
              case NIL => { c1 }
              case ZERO(c2in) => { ONE(crazy2add(c1in, c2in)) }
              case ONE(c2in) => {
                ZERO(crazy2add(crazy2add(c1in, c2in), ONE(NIL)))
              }
              case MONE(c2in) => { ZERO(crazy2add(c1in, c2in)) }
            }
          }
          case MONE(c1in) => {
            c2 match {
              case NIL => { c1 }
              case ZERO(c2in) => { MONE(crazy2add(c1in, c2in)) }
              case ONE(c2in) => { ZERO(crazy2add(c1in, c2in)) }
              case MONE(c2in) => {
                ZERO(crazy2add(crazy2add(c1in, c2in), MONE(NIL)))
              }
            }
          }
        }
    }
  }
}
