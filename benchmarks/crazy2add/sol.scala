import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sol {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) =>
      {
        val _2 = {
          def crazy2add_sub: (Crazy2, Crazy2) => Crazy2 = {
            case (c1, c2) =>
              {
                c1 match {
                  case NIL => { c2 }
                  case ZERO(c1_0) => {
                    c2 match {
                      case NIL => { c1 }
                      case ZERO(c2_0) => { ZERO(crazy2add_sub(c1_0, c2_0)) }
                      case ONE(c2_0) => { ONE(crazy2add_sub(c1_0, c2_0)) }
                      case MONE(c2_0) => { MONE(crazy2add_sub(c1_0, c2_0)) }
                    }
                  }
                  case ONE(c1_0) => {
                    c2 match {
                      case NIL => { c1 }
                      case ZERO(c2_0) => { ONE(crazy2add_sub(c1_0, c2_0)) }
                      case ONE(c2_0) => {
                        ZERO(crazy2add_sub(ONE(NIL), crazy2add_sub(c1_0, c2_0)))
                      }
                      case MONE(c2_0) => { ZERO(crazy2add_sub(c1_0, c2_0)) }
                    }
                  }
                  case MONE(c1_0) => {
                    c2 match {
                      case NIL => { c1 }
                      case ZERO(c2_0) => { MONE(crazy2add_sub(c1_0, c2_0)) }
                      case ONE(c2_0) => { ZERO(crazy2add_sub(c1_0, c2_0)) }
                      case MONE(c2_0) => {
                        ZERO(
                          crazy2add_sub(MONE(NIL), crazy2add_sub(c1_0, c2_0)))
                      }
                    }
                  }
                }
            }
          }
          
            if (
              c1 == NIL || c2 == NIL
            ) {
              assert(false, "Failure with Invalid ") 
            } else {
              crazy2add_sub(c1, c2)
            }
        }
    }
  }
}