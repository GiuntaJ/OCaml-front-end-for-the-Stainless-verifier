import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub217 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  sealed abstract class Crazy2char {}
  case object Z extends Crazy2char {}
  case object O extends Crazy2char {}
  case object M extends Crazy2char {}
  
  def crazy2add(((cz1: Crazy2, cz2: Crazy2))): Crazy2 = {
    val _2 = {
      def crazy2ToChar(c: Crazy2): (Crazy2char, Crazy2) = {
        c match {
          case NIL => { (Z, NIL) }
          case ZERO(d) => { (Z, d) }
          case ONE(d) => { (O, d) }
          case MONE(d) => { (M, d) }
        }
      }
      val _3 = {
        def crazy2addOne(((c1: Crazy2char, c2: Crazy2char))): (Crazy2char, Crazy2char) = {
          (c1, c2) match {
            case (M, M) => { (Z, M) }
            case (M, O) => { (Z, Z) }
            case (Z, c) => { (c, Z) }
            case (c, Z) => { (c, Z) }
            case (O, M) => { (Z, Z) }
            case (O, O) => { (Z, O) }
          }
        }
        val _4 = {
          def crazy2addRec(((c1: Crazy2, c2: Crazy2, c: Crazy2char))): Crazy2 = {
            (c1, c2, c) match {
              case (NIL, NIL, c) => {
                c match {
                  case Z => { NIL }
                  case M => { MONE(NIL) }
                  case O => { ONE(NIL) }
                }
              }
              case (_, _, _) => {
                val _7 = {
                  val ((a1, b1)) = crazy2ToChar(c1)
                  val _8 = {
                    val ((a2, b2)) = crazy2ToChar(c2)
                    val _9 = {
                      val ((a3, a4)) = crazy2addOne(a1, a2)
                      val _10 = {
                        val ((a5, a6)) = crazy2addOne(a3, c)
                        val _11 = {
                          val ((a7, _)) = crazy2addOne(a4, a6)
                          a5 match {
                            case Z => { ZERO(crazy2addRec(b1, b2, a7)) }
                            case O => { ONE(crazy2addRec(b1, b2, a7)) }
                            case M => { MONE(crazy2addRec(b1, b2, a7)) }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          crazy2addRec(cz1, cz2, Z)
        }
      }
    }
  }
}