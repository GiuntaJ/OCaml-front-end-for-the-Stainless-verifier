import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub43 {
  /*
      PL 2-2
      2008-11609 박성원
  */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1, c2))) = {
    val _2 = {
      def getNext(c) = {
        c match {
          case NIL => { NIL }
          case ZERO(n) => { n }
          case ONE(n) => { n }
          case MONE(n) => { n }
        }
      }
      val _3 = {
        def calcCarry(c1, c2, carry) = {
          (c1, c2, carry) match {
            case (ONE(_), ONE(_), ONE(_)) | (ONE(_), ONE(_), ZERO(_)) |
            (ONE(_), ZERO(_), ONE(_)) | (ZERO(_), ONE(_), ONE(_)) => {
              ONE(NIL)
            }
            case (MONE(_), MONE(_), MONE(_)) | (MONE(_), MONE(_), ZERO(_)) |
            (MONE(_), ZERO(_), MONE(_)) | (ZERO(_), MONE(_), MONE(_)) => {
              MONE(NIL)
            }
            case _ => { ZERO(NIL) }
          }
        }
        val _4 = {
          def addImpl(c1, c2, carry) = {
            (c1, c2, carry) match {
              case (NIL, NIL, ZERO(_)) => { NIL }
              case (NIL, NIL, _) => { carry }
              case _ => {
                val _7 = {
                  val p1 = if (c1 == NIL) ZERO(NIL) else c1
                  val _8 = {
                    val p2 = if (c2 == NIL) ZERO(NIL) else c2
                    val _9 = {
                      val nextCalc = addImpl(getNext(p1), getNext(p2), calcCarry(p1, p2, carry))
                      val _10 = {
                        val nextVal = if (nextCalc == ZERO(NIL)) NIL else nextCalc
                        (p1, p2, carry) match {
                          case (ONE(_), MONE(_), ONE(_)) => { ONE(nextVal) }
                          case (ONE(_), MONE(_), ZERO(_)) => { ZERO(nextVal) }
                          case (ONE(_), MONE(_), MONE(_)) => { MONE(nextVal) }
                          case (ZERO(_), ZERO(_), ONE(_)) => { ONE(nextVal) }
                          case (ZERO(_), ZERO(_), ZERO(_)) => { ZERO(nextVal) }
                          case (ZERO(_), ZERO(_), MONE(_)) => { MONE(nextVal) }
                          case (MONE(_), ONE(_), ONE(_)) => { ONE(nextVal) }
                          case (MONE(_), ONE(_), ZERO(_)) => { ZERO(nextVal) }
                          case (MONE(_), ONE(_), MONE(_)) => { MONE(nextVal) }
                          case (ONE(_), ONE(_), ZERO(_)) => { ZERO(nextVal) }
                          case (ONE(_), ONE(_), _) => { ONE(nextVal) }
                          case (ONE(_), ZERO(_), ZERO(_)) => { ONE(nextVal) }
                          case (ONE(_), ZERO(_), _) => { ZERO(nextVal) }
                          case (ZERO(_), ONE(_), ZERO(_)) => { ONE(nextVal) }
                          case (ZERO(_), ONE(_), _) => { ZERO(nextVal) }
                          case (ZERO(_), MONE(_), ZERO(_)) => { MONE(nextVal) }
                          case (ZERO(_), MONE(_), _) => { ZERO(nextVal) }
                          case (MONE(_), ZERO(_), ZERO(_)) => { MONE(nextVal) }
                          case (MONE(_), ZERO(_), _) => { ZERO(nextVal) }
                          case (MONE(_), MONE(_), ZERO(_)) => { ZERO(nextVal) }
                          case (MONE(_), MONE(_), _) => { MONE(nextVal) }
                          case _ => { ZERO(nextVal) }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          addImpl(c1, c2, ZERO(NIL))
        }
      }
    }
  }
}