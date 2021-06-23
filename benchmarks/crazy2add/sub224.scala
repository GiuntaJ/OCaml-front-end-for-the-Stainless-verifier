import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub224 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        (a, b) match {
          case (NIL, _) => { b }
          case (_, NIL) => { a }
          case (ZERO(_a), ZERO(_b)) => { ZERO(crazy2add(_a, _b)) }
          case (ZERO(_a), ONE(_b)) => { ONE(crazy2add(_a, _b)) }
          case (ZERO(_a), MONE(_b)) => { MONE(crazy2add(_a, _b)) }
          case (ONE(_a), ZERO(_b)) => { ONE(crazy2add(_a, _b)) }
          case (ONE(_a), ONE(_b)) => {
            ZERO(crazy2add(crazy2add(_a, _b), ONE(NIL)))
          }
          case (ONE(_a), MONE(_b)) => { ZERO(crazy2add(_a, _b)) }
          case (MONE(_a), ZERO(_b)) => { MONE(crazy2add(_a, _b)) }
          case (MONE(_a), ONE(_b)) => { ZERO(crazy2add(_a, _b)) }
          case (MONE(_a), MONE(_b)) => {
            ZERO(crazy2add(crazy2add(_a, _b), MONE(NIL)))
          }
        }
    }
  }
      
  
     
}