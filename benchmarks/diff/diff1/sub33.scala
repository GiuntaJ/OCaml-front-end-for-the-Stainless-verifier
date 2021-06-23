import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub33 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((ae, str))) = {
    ae match {
      case CONST(i) => { CONST(0) }
      case VAR(var0) => { if (var0 == str) CONST(1) else CONST(0) }
      case POWER(var0, pow) => {
        
          if (
            var0 == str
          ) {
            
              if (
                pow == 1
              ) {
                CONST(1) 
              } else {
                TIMES(List(CONST(pow), POWER(var0, pow - 1)))
              } 
          } else {
            CONST(0)
          }
      }
      case TIMES(lst) => {
        val _10 = {
          def diff_product(left_elts, right_elts) = {
            
              if (
                right_elts == Nil()
              ) {
                Nil() 
              } else {
                val _14 = {
                  val hd = right_elts.head
                  val _15 = {
                    val tl = right_elts.tail
                    TIMES(left_elts reverse_:::(diff(hd, str) :: tl)) ::
                    diff_product(hd :: left_elts, tl)
                  }
                }
              }
          }
          SUM(diff_product(Nil(), lst))
        }
      }
      case SUM(lst) => {
        val _2 = {
          def diff_sum(elts) = {
            
              if (
                elts == Nil()
              ) {
                Nil() 
              } else {
                val _6 = {
                  val hd = elts.head
                  val _7 = {
                    val tl = elts.tail
                    diff(hd, str) :: diff_sum(tl)
                  }
                }
              }
          }
          SUM(diff_sum(lst))
        }
      }
    }
  }
}