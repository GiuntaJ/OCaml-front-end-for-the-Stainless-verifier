import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub65 {
  sealed case class TODO() extends Exception {}
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def in_crazy2val(c: Crazy2): Int63 = {
    c match {
      case NIL => { 0 }
      case ZERO(a) => { 2 * in_crazy2val(a) }
      case ONE(a) => { 2 * in_crazy2val(a) + 1 }
      case MONE(a) => { 2 * in_crazy2val(a) - 1 }
    }
  }
  
  def sum_carry(a: Int63, b: Int63, c: Int63): (Int63, Int63) = {
    val _2 = {
      val sum = a + b + c
      (sum % 2, sum / 2)
    }
  }
  
  def valTocrazy2(a: Int63): Crazy2 = {
    a match {
      case 0 => { ZERO(NIL) }
      case 1 => { ONE(NIL) }
      case -1 => { MONE(NIL) }
      case _ => { NIL }
    }
  }
  
  def append_crazy2(rhs: Crazy2, lhs: Crazy2): Crazy2 = {
    rhs match {
      case NIL => { lhs }
      case ZERO(NIL) => { ZERO(lhs) }
      case ONE(NIL) => { ONE(lhs) }
      case MONE(NIL) => { MONE(lhs) }
      case _ => { NIL }
    }
  }
  
  def divider(c: Crazy2): (Crazy2, Crazy2) = {
    c match {
      case NIL => { (NIL, NIL) }
      case ZERO(a) => { (ZERO(NIL), a) }
      case ONE(a) => { (ONE(NIL), a) }
      case MONE(a) => { (MONE(NIL), a) }
    }
  }
  
  def sum_carry_as_crazy2(lhs: Crazy2, rhs: Crazy2, c: Crazy2): (Crazy2, Crazy2) = {
    val _5 = {
      val lhs_num = in_crazy2val(lhs)
      val _6 = {
        val rhs_num = in_crazy2val(rhs)
        val _7 = {
          val c_num = in_crazy2val(c)
          val _8 = {
            val sumXcarry = sum_carry(lhs_num, rhs_num, c_num)
            val _9 = {
              val sum = fst(sumXcarry)
              val _10 = {
                val carry = snd(sumXcarry)
                (valTocrazy2(sum), valTocrazy2(carry))
              }
            }
          }
        }
      }
    }
  }
  
  def crazy2add_carry(lhs: Crazy2, rhs: Crazy2, carry: Crazy2): Crazy2 = {
    val _13 = {
      val lhs_first = fst(divider(lhs))
      val _14 = {
        val lhs_second = snd(divider(lhs))
        val _15 = {
          val rhs_first = fst(divider(rhs))
          val _16 = {
            val rhs_second = snd(divider(rhs))
            val _17 = {
              val sumXcarry = sum_carry_as_crazy2(lhs_first, rhs_first, carry)
              val _18 = {
                val sum = fst(sumXcarry)
                val _19 = {
                  val cry = snd(sumXcarry)
                  
                    if (
                      lhs_first == NIL && rhs_first == NIL
                    ) {
                      carry 
                    } else {
                      append_crazy2(
                        sum, crazy2add_carry(lhs_second, rhs_second, cry))
                    }
                }
              }
            }
          }
        }
      }
    }
  }
  
  
  def crazy2add(((a: Crazy2, b: Crazy2))): Crazy2 = {
    crazy2add_carry(a, b, ZERO(NIL))
  }
  
}
