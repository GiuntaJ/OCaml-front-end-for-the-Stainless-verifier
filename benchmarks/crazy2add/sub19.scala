import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub19 {
  sealed case class Error(param0: String) extends Exception {}
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  def crazy2add(((a: Crazy2, b: Crazy2))) = {
    val _2 = {
      def crazy2fulladd(((a: Crazy2, b: Crazy2, carry: Crazy2))) = {
        val _5 = {
          def realAdder(((h1: Crazy2, h2: Crazy2, carry: Crazy2))) = {
            val _8 = {
              def getHead(a: Crazy2) = {
                a match {
                  case NIL => { 0 }
                  case ZERO(p) => { 0 }
                  case ONE(p) => { 1 }
                  case MONE(p) => { -(1) }
                }
              }
              val _9 = {
                def getTail(a: Crazy2) = {
                  a match {
                    case NIL => { NIL }
                    case ZERO(p) => { p }
                    case ONE(p) => { p }
                    case MONE(p) => { p }
                  }
                }
                val _10 = {
                  def trim(a: Crazy2) = { if (a == ZERO(NIL)) NIL else a }
                  val _11 = {
                    def recur(((h1: Crazy2, h2: Crazy2, carry: Crazy2))) = {
                      trim(crazy2fulladd(getTail(h1), getTail(h2), carry))
                    }
                    getHead(h1) + getHead(h2) + getHead(carry) match {
                      case -3 => { MONE(recur(h1, h2, MONE(NIL))) }
                      case -2 => { ZERO(recur(h1, h2, MONE(NIL))) }
                      case -1 => { MONE(recur(h1, h2, NIL)) }
                      case 0 => { ZERO(recur(h1, h2, NIL)) }
                      case 1 => { ONE(recur(h1, h2, NIL)) }
                      case 2 => { ZERO(recur(h1, h2, ONE(NIL))) }
                      case 3 => { ONE(recur(h1, h2, ONE(NIL))) }
                      case _ => { assert(false, "Error with Invalid Number!!") }
                    }
                  }
                }
              }
            }
          }
          (a, b, carry) match {
            case (NIL, NIL, NIL) => { NIL }
            case (_, _, _) => { realAdder(a, b, carry) }
          }
        }
      }
      
        if (
          a == NIL || b == NIL
        ) {
          assert(false, "Error with All Crazy2 Should be non-NIL! ") 
        } else {
          crazy2fulladd(a, b, ZERO(NIL))
        }
    }
  }
}