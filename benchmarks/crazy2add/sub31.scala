import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub31 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((ca, cb))) = { crazy2add_in(ca, cb, ZERO(NIL)) }
  def crazy2add_in(a, b, carry) = {
    a match {
      case NIL => {
        b match {
          case NIL => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { NIL }
              case ONE(f) => { ONE(NIL) }
              case MONE(f) => { MONE(NIL) }
            }
          }
          case ZERO(e) => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { ZERO(crazy2add_in(a, e, ZERO(NIL))) }
              case ONE(f) => { ONE(crazy2add_in(a, e, ZERO(NIL))) }
              case MONE(f) => { MONE(crazy2add_in(a, e, ZERO(NIL))) }
            }
          }
          case ONE(e) => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { ONE(crazy2add_in(a, e, ZERO(NIL))) }
              case ONE(f) => { ZERO(crazy2add_in(a, e, ONE(NIL))) }
              case MONE(f) => { ZERO(crazy2add_in(a, e, ZERO(NIL))) }
            }
          }
          case MONE(e) => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { MONE(crazy2add_in(a, e, ZERO(NIL))) }
              case ONE(f) => { ZERO(crazy2add_in(a, e, ZERO(NIL))) }
              case MONE(f) => { ZERO(crazy2add_in(a, e, MONE(NIL))) }
            }
          }
        }
      }
      case ZERO(d) => {
        b match {
          case NIL => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { ZERO(crazy2add_in(d, b, ZERO(NIL))) }
              case ONE(f) => { ONE(crazy2add_in(d, b, ZERO(NIL))) }
              case MONE(f) => { MONE(crazy2add_in(d, b, ZERO(NIL))) }
            }
          }
          case ZERO(e) => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { ZERO(crazy2add_in(d, e, ZERO(NIL))) }
              case ONE(f) => { ONE(crazy2add_in(d, e, ZERO(NIL))) }
              case MONE(f) => { MONE(crazy2add_in(d, e, ZERO(NIL))) }
            }
          }
          case ONE(e) => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { ONE(crazy2add_in(d, e, ZERO(NIL))) }
              case ONE(f) => { ZERO(crazy2add_in(d, e, ONE(NIL))) }
              case MONE(f) => { ZERO(crazy2add_in(d, e, ZERO(NIL))) }
            }
          }
          case MONE(e) => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { MONE(crazy2add_in(d, e, ZERO(NIL))) }
              case ONE(f) => { ZERO(crazy2add_in(d, e, ZERO(NIL))) }
              case MONE(f) => { ZERO(crazy2add_in(d, e, MONE(NIL))) }
            }
          }
        }
      }
      case ONE(d) => {
        b match {
          case NIL => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { ONE(crazy2add_in(d, b, ZERO(NIL))) }
              case ONE(f) => { ZERO(crazy2add_in(d, b, ONE(NIL))) }
              case MONE(f) => { ZERO(crazy2add_in(d, b, ZERO(NIL))) }
            }
          }
          case ZERO(e) => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { ONE(crazy2add_in(d, e, ZERO(NIL))) }
              case ONE(f) => { ZERO(crazy2add_in(d, e, ONE(NIL))) }
              case MONE(f) => { ZERO(crazy2add_in(d, e, ZERO(NIL))) }
            }
          }
          case ONE(e) => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { ZERO(crazy2add_in(d, e, ONE(NIL))) }
              case ONE(f) => { ONE(crazy2add_in(d, e, ONE(NIL))) }
              case MONE(f) => { ONE(crazy2add_in(d, e, ZERO(NIL))) }
            }
          }
          case MONE(e) => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { ZERO(crazy2add_in(d, e, ZERO(NIL))) }
              case ONE(f) => { ONE(crazy2add_in(d, e, ZERO(NIL))) }
              case MONE(f) => { MONE(crazy2add_in(d, e, ZERO(NIL))) }
            }
          }
        }
      }
      case MONE(d) => {
        b match {
          case NIL => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { MONE(crazy2add_in(d, b, ZERO(NIL))) }
              case ONE(f) => { ZERO(crazy2add_in(d, b, ZERO(NIL))) }
              case MONE(f) => { ZERO(crazy2add_in(d, b, MONE(NIL))) }
            }
          }
          case ZERO(e) => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { ZERO(crazy2add_in(d, e, ZERO(NIL))) }
              case ONE(f) => { ZERO(crazy2add_in(d, e, ZERO(NIL))) }
              case MONE(f) => { ZERO(crazy2add_in(d, e, MONE(NIL))) }
            }
          }
          case ONE(e) => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { ZERO(crazy2add_in(d, e, ZERO(NIL))) }
              case ONE(f) => { ONE(crazy2add_in(d, e, ZERO(NIL))) }
              case MONE(f) => { MONE(crazy2add_in(d, e, ZERO(NIL))) }
            }
          }
          case MONE(e) => {
            carry match {
              case NIL => { NIL }
              case ZERO(f) => { ZERO(crazy2add_in(d, e, MONE(NIL))) }
              case ONE(f) => { MONE(crazy2add_in(d, e, ZERO(NIL))) }
              case MONE(f) => { MONE(crazy2add_in(d, e, MONE(NIL))) }
            }
          }
        }
      }
    }
  }
}