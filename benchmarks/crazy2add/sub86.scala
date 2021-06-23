import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub86 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  sealed abstract class Crazy2digit {}
  case object GZERO extends Crazy2digit {}
  case object GONE extends Crazy2digit {}
  case object GMONE extends Crazy2digit {}
  case object GDMONE extends Crazy2digit {}
  case object GDONE extends Crazy2digit {}
  case object GTMONE extends Crazy2digit {}
  case object GTONE extends Crazy2digit {}
  
  sealed case class InconsistencyError() extends Exception {}
  sealed case class DigitAddError() extends Exception {}
  
  def crazy2digit_add(((v1: Crazy2digit, v2: Crazy2digit))) = {
    v1 match {
      case GDMONE => {
        v2 match {
          case GMONE => { GTMONE }
          case GZERO => { GDMONE }
          case GONE => { GMONE }
          case _ => { GDMONE }
        }
      }
      case GMONE => {
        v2 match {
          case GMONE => { GDMONE }
          case GZERO => { GMONE }
          case GONE => { GZERO }
          case _ => { GMONE }
        }
      }
      case GZERO => {
        v2 match {
          case GMONE => { GMONE }
          case GZERO => { GZERO }
          case GONE => { GONE }
          case _ => { GZERO }
        }
      }
      case GONE => {
        v2 match {
          case GMONE => { GZERO }
          case GZERO => { GONE }
          case GONE => { GDONE }
          case _ => { GONE }
        }
      }
      case GDONE => {
        v2 match {
          case GMONE => { GONE }
          case GZERO => { GDONE }
          case GONE => { GTONE }
          case _ => { GDONE }
        }
      }
      case _ => { assert(false, "DigitAddError") }
    }
  }
  
  def crazy2add(((v1: Crazy2, v2: Crazy2))) = {
    val _2 = {
      def digit_identifier(v: Crazy2): (Crazy2digit, Crazy2) = {
        v match {
          case NIL => { (GZERO, NIL) }
          case ZERO(c) => { (GZERO, c) }
          case ONE(c) => { (GONE, c) }
          case MONE(c) => { (GMONE, c) }
        }
      }
      val _3 = {
        def add(((v1: Crazy2, v2: Crazy2, carry: Crazy2digit))): Crazy2 = {
          (v1, v2, carry) match {
            case (NIL, NIL, GZERO) => { NIL }
            case (NIL, NIL, v) => {
              
                if (
                  v == GONE
                ) {
                  ONE(NIL) 
                } else if (
                  v == GZERO
                ) {
                  ZERO(NIL) 
                } else {
                  MONE(NIL)
                }
            }
            case (_, _, _) => {
              val _6 = {
                val ((d1, c1)) = digit_identifier(v1)
                val _7 = {
                  val ((d2, c2)) = digit_identifier(v2)
                  val _8 = {
                    val s_temp = crazy2digit_add(d1, d2)
                    val _9 = {
                      val s = crazy2digit_add(s_temp, carry)
                      s match {
                        case GTMONE => { MONE(add(c1, c2, GMONE)) }
                        case GDMONE => { ZERO(add(c1, c2, GMONE)) }
                        case GMONE => { MONE(add(c1, c2, GZERO)) }
                        case GZERO => { ZERO(add(c1, c2, GZERO)) }
                        case GONE => { ONE(add(c1, c2, GZERO)) }
                        case GDONE => { ZERO(add(c1, c2, GONE)) }
                        case GTONE => { ONE(add(c1, c2, GONE)) }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        add(v1, v2, GZERO)
      }
    }
  }
}