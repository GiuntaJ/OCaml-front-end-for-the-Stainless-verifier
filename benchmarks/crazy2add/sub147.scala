import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub147 {
  /*컴퓨터공학부/2011-11729/안진우/2-3*/
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  sealed abstract class Crazy2c {}
  case object N extends Crazy2c {}
  case object O extends Crazy2c {}
  case object M extends Crazy2c {}  
  
  def crazy2add_h(((x: Crazy2, y: Crazy2, z: Crazy2c))): Crazy2 = {
    (x, y, z) match {
      case (NIL, _, _) => {
        (y, z) match {
          case (NIL, _) => {
            z match {
              case N => { NIL }
              case O => { ONE(NIL) }
              case M => { MONE(NIL) }
            }
          }
          case (ZERO(b), _) => {
            z match {
              case N => { ZERO(b) }
              case O => { ONE(b) }
              case M => { MONE(b) }
            }
          }
          case (ONE(b), _) => {
            z match {
              case N => { ONE(b) }
              case O => { ZERO(crazy2add_h(NIL, b, O)) }
              case M => { ZERO(b) }
            }
          }
          case (MONE(b), _) => {
            z match {
              case N => { MONE(b) }
              case O => { ZERO(b) }
              case M => { ZERO(crazy2add_h(NIL, b, M)) }
            }
          }
        }
      }
      case (ZERO(a), _, _) => {
        (y, z) match {
          case (NIL, _) => {
            z match {
              case N => { ZERO(a) }
              case O => { ONE(a) }
              case M => { MONE(a) }
            }
          }
          case (ZERO(b), _) => {
            z match {
              case N => { ZERO(crazy2add_h(a, b, N)) }
              case O => { ONE(crazy2add_h(a, b, N)) }
              case M => { MONE(crazy2add_h(a, b, N)) }
            }
          }
          case (ONE(b), _) => {
            z match {
              case N => { ONE(crazy2add_h(a, b, N)) }
              case O => { ZERO(crazy2add_h(a, b, O)) }
              case M => { ZERO(crazy2add_h(a, b, N)) }
            }
          }
          case (MONE(b), _) => {
            z match {
              case N => { MONE(crazy2add_h(a, b, N)) }
              case O => { ZERO(crazy2add_h(a, b, N)) }
              case M => { ZERO(crazy2add_h(a, b, M)) }
            }
          }
        }
      }
      case (ONE(a), _, _) => {
        (y, z) match {
          case (NIL, _) => {
            z match {
              case N => { ONE(a) }
              case O => { ZERO(crazy2add_h(a, NIL, O)) }
              case M => { ZERO(a) }
            }
          }
          case (ZERO(b), _) => {
            z match {
              case N => { ONE(crazy2add_h(a, b, N)) }
              case O => { ZERO(crazy2add_h(a, b, O)) }
              case M => { ZERO(crazy2add_h(a, b, N)) }
            }
          }
          case (ONE(b), _) => {
            z match {
              case N => { ZERO(crazy2add_h(a, b, O)) }
              case O => { ONE(crazy2add_h(a, b, O)) }
              case M => { ONE(crazy2add_h(a, b, N)) }
            }
          }
          case (MONE(b), _) => {
            z match {
              case N => { ZERO(crazy2add_h(a, b, N)) }
              case O => { ONE(crazy2add_h(a, b, N)) }
              case M => { MONE(crazy2add_h(a, b, N)) }
            }
          }
        }
      }
      case (MONE(a), _, _) => {
        (y, z) match {
          case (NIL, _) => {
            z match {
              case N => { MONE(a) }
              case O => { ZERO(a) }
              case M => { ZERO(crazy2add_h(a, NIL, M)) }
            }
          }
          case (ZERO(b), _) => {
            z match {
              case N => { MONE(crazy2add_h(a, b, N)) }
              case O => { ZERO(crazy2add_h(a, b, N)) }
              case M => { ZERO(crazy2add_h(a, b, M)) }
            }
          }
          case (ONE(b), _) => {
            z match {
              case N => { ZERO(crazy2add_h(a, b, N)) }
              case O => { ONE(crazy2add_h(a, b, N)) }
              case M => { MONE(crazy2add_h(a, b, N)) }
            }
          }
          case (MONE(b), _) => {
            z match {
              case N => { ZERO(crazy2add_h(a, b, M)) }
              case O => { MONE(crazy2add_h(a, b, N)) }
              case M => { MONE(crazy2add_h(a, b, M)) }
            }
          }
        }
      }
    }
  }
   
  def crazy2add(((x: Crazy2, y: Crazy2))): Crazy2 = { crazy2add_h(x, y, N) }
}