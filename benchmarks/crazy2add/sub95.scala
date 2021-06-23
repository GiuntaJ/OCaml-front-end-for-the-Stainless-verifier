import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub95 {
  sealed case class TODO() extends Exception {}
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2length(x: Crazy2): Int63 = {
    x match {
      case NIL => { 0 }
      case MONE(x_sub) => { 1 + crazy2length(x_sub) }
      case ZERO(x_sub) => { 1 + crazy2length(x_sub) }
      case ONE(x_sub) => { 1 + crazy2length(x_sub) }
    }
  }
  
  def addzero(x: Crazy2): Crazy2 = {
    x match {
      case NIL => { ZERO(NIL) }
      case MONE(x_sub) => { MONE(addzero(x_sub)) }
      case ZERO(x_sub) => { ZERO(addzero(x_sub)) }
      case ONE(x_sub) => { ONE(addzero(x_sub)) }
    }
  }
  
  def crazy2match(((x, y))) = {
    
      if (
        crazy2length(x) == crazy2length(y)
      ) {
        List(x, y) 
      } else if (
        crazy2length(x) > crazy2length(y)
      ) {
        crazy2match(x, addzero(y)) 
      } else {
        crazy2match(addzero(x), y)
      }
  }
  
  def crazy2add(((a, b))) = {
    val _2 = {
      def dec(x) = {
        x match {
          case NIL => { MONE(NIL) }
          case ZERO(x_sub) => { MONE(x_sub) }
          case ONE(x_sub) => { ZERO(x_sub) }
          case MONE(x_sub) => { ZERO(dec(x_sub)) }
        }
      }
      val _3 = {
        def inc(x) = {
          x match {
            case NIL => { ONE(NIL) }
            case ZERO(x_sub) => { ONE(x_sub) }
            case MONE(x_sub) => { ZERO(x_sub) }
            case ONE(x_sub) => { ZERO(inc(x_sub)) }
          }
        }
        val _4 = {
          def crazy2add_length(((a, b))) = {
            (a, b) match {
              case (NIL, NIL) => { NIL }
              case (_, NIL) => { a }
              case (NIL, _) => { b }
              case (MONE(a_sub), MONE(b_sub)) => {
                ZERO(crazy2add_length(a_sub, dec(b_sub)))
              }
              case (MONE(a_sub), ZERO(b_sub)) => {
                MONE(crazy2add_length(a_sub, b_sub))
              }
              case (MONE(a_sub), ONE(b_sub)) => {
                ZERO(crazy2add_length(a_sub, b_sub))
              }
              case (ZERO(a_sub), MONE(b_sub)) => {
                MONE(crazy2add_length(a_sub, b_sub))
              }
              case (ZERO(a_sub), ZERO(b_sub)) => {
                ZERO(crazy2add_length(a_sub, b_sub))
              }
              case (ZERO(a_sub), ONE(b_sub)) => {
                ONE(crazy2add_length(a_sub, b_sub))
              }
              case (ONE(a_sub), MONE(b_sub)) => {
                ZERO(crazy2add_length(a_sub, b_sub))
              }
              case (ONE(a_sub), ZERO(b_sub)) => {
                ONE(crazy2add_length(a_sub, b_sub))
              }
              case (ONE(a_sub), ONE(b_sub)) => {
                ZERO(crazy2add_length(a_sub, inc(b_sub)))
              }
            }
          }
          val _5 = {
            val new_list = crazy2match(a, b)
            crazy2add_length(new_list.apply(0), new_list.apply(1))
          }
        }
      }
    }
  }
}