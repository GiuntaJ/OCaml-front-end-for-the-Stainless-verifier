import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub91 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  sealed abstract class Crazy2bit {}
  case object Z extends Crazy2bit {}
  case object O extends Crazy2bit {}
  case object M extends Crazy2bit {}
  
  val val2bit: Crazy2 => Crazy2bit = (
    (x) =>
      {
        x match {
          case NIL | ZERO(_) => { Z }
          case ONE(_) => { O }
          case MONE(_) => { M }
        }
    }
  )
  
  def append_bit(bit: Crazy2bit, x: Crazy2): Crazy2 = {
    bit match {
      case Z => { ZERO(x) }
      case O => { ONE(x) }
      case M => { MONE(x) }
    }
  }
  
  def crazy_inner(x: Crazy2): Crazy2 = {
    x match {
      case NIL => { NIL }
      case ONE(y) => { y }
      case MONE(y) => { y }
      case ZERO(y) => { y }
    }
  }
  
  val crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        val _2 = {
          def halfadd(a, b) = {
            (a, b) match {
              case (O, O) => { (Z, O) }
              case (O, M) => { (Z, Z) }
              case (M, O) => { (Z, Z) }
              case (M, M) => { (Z, M) }
              case (_, Z) => { (a, Z) }
              case (Z, _) => { (b, Z) }
            }
          }
          val _3 = {
            def fulladd(a, b, c) = {
              val _6 = {
                val sum1 = halfadd(a, b)
                val _7 = {
                  val sum2 = halfadd(c, fst(sum1))
                  (fst(sum2), fst(halfadd(snd(sum1), snd(sum2))))
                }
              }
            }
            val _8 = {
              def crazy2add_carry(a, b, carry) = {
                (a, b) match {
                  case (NIL, NIL) => { NIL }
                  case _ => {
                    val _11 = {
                      val sum = fulladd(val2bit(a), val2bit(b), carry)
                      append_bit(
                        fst(sum),
                        crazy2add_carry(
                          crazy_inner(a), crazy_inner(b), snd(sum)))
                    }
                  }
                }
              }
              crazy2add_carry(a, b, Z)
            }
          }
        }
    }
  }
}