import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub7 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  sealed case class Error(param0: String) extends Exception {}
  
  def crazy2add(((a, b))) = {
    val _2 = {
      def crazy2add_internal(((a, b))) = {
        a match {
          case NIL => { b }
          case ZERO(a2) => {
            b match {
              case NIL => { a }
              case ZERO(b2) => { ZERO(crazy2add_internal(a2, b2)) }
              case ONE(b2) => { ONE(crazy2add_internal(a2, b2)) }
              case MONE(b2) => { MONE(crazy2add_internal(a2, b2)) }
            }
          }
          case ONE(a2) => {
            b match {
              case NIL => { a }
              case ZERO(b2) => { ONE(crazy2add_internal(a2, b2)) }
              case ONE(b2) => {
                ZERO(crazy2add_internal(crazy2add_internal(a2, b2), ONE(NIL)))
              }
              case MONE(b2) => { ZERO(crazy2add_internal(a2, b2)) }
            }
          }
          case MONE(a2) => {
            b match {
              case NIL => { a }
              case ZERO(b2) => { MONE(crazy2add_internal(a2, b2)) }
              case ONE(b2) => { ZERO(crazy2add_internal(a2, b2)) }
              case MONE(b2) => {
                ZERO(crazy2add_internal(crazy2add_internal(a2, b2), MONE(NIL)))
              }
            }
          }
        }
      }
      val _3 = {
        def crazy2add_reduceZero_internal(n) = {
          n match {
            case NIL => { NIL }
            case ZERO(NIL) => { NIL }
            case ZERO(b) => {
              ZERO(
                crazy2add_reduceZero_internal(crazy2add_reduceZero_internal(b)))
            }
            case ONE(b) => { ONE(crazy2add_reduceZero_internal(b)) }
            case MONE(b) => { MONE(crazy2add_reduceZero_internal(b)) }
          }
        }
        val _4 = {
          def crazy2add_reduceZero(n) = {
            val _7 = {
              val x = crazy2add_reduceZero_internal(n)
              x match {
                case NIL => { ZERO(NIL) }
                case _ => { x }
              }
            }
          }
          
            if (
              a == NIL || b == NIL
            ) {
              assert(false, "Error with NIL is not number ") 
            } else {
              crazy2add_reduceZero(crazy2add_internal(a, b))
            }
        }
      }
    }
  }
}