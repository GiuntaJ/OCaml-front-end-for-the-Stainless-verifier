import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub230 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  sealed abstract class Carry {}
  case object NONE extends Carry {}
  case object POS extends Carry {}
  case object NEG extends Carry {}
  
  def crazy2add(((z1: Crazy2, z2: Crazy2))) = {
    val _2 = {
      def crazy2adder(((x: Crazy2, y: Crazy2, carry: Carry))) = {
        carry match {
          case NONE => {
            (x, y) match {
              case (NIL, _) => { y }
              case (ZERO(c1), ZERO(c2)) => { ZERO(crazy2adder(c1, c2, NONE)) }
              case (ZERO(c1), ONE(c2)) => { ONE(crazy2adder(c1, c2, NONE)) }
              case (ZERO(c1), MONE(c2)) => { MONE(crazy2adder(c1, c2, NONE)) }
              case (ONE(c1), ONE(c2)) => { ZERO(crazy2adder(c1, c2, POS)) }
              case (ONE(c1), MONE(c2)) => { ZERO(crazy2adder(c1, c2, NONE)) }
              case (MONE(c1), MONE(c2)) => { ZERO(crazy2adder(c1, c2, NEG)) }
              case (_, _) => { crazy2adder(y, x, carry) }
            }
          }
          case POS => {
            (x, y) match {
              case (NIL, _) => {
                y match {
                  case NIL => { ONE(NIL) }
                  case MONE(c1) => { ZERO(c1) }
                  case ZERO(c1) => { ONE(c1) }
                  case ONE(c1) => { ZERO(crazy2adder(c1, NIL, POS)) }
                }
              }
              case (ZERO(c1), ZERO(c2)) => { ONE(crazy2adder(c1, c2, NONE)) }
              case (ZERO(c1), ONE(c2)) => { ZERO(crazy2adder(c1, c2, POS)) }
              case (ZERO(c1), MONE(c2)) => { ZERO(crazy2adder(c1, c2, NONE)) }
              case (ONE(c1), ONE(c2)) => { ONE(crazy2adder(c1, c2, POS)) }
              case (ONE(c1), MONE(c2)) => { ONE(crazy2adder(c1, c2, NONE)) }
              case (MONE(c1), MONE(c2)) => { MONE(crazy2adder(c1, c2, NONE)) }
              case (_, _) => { crazy2adder(y, x, carry) }
            }
          }
          case NEG => {
            (x, y) match {
              case (NIL, _) => {
                y match {
                  case NIL => { MONE(NIL) }
                  case MONE(c1) => { ZERO(crazy2adder(c1, NIL, NEG)) }
                  case ZERO(c1) => { MONE(c1) }
                  case ONE(c1) => { ZERO(c1) }
                }
              }
              case (ZERO(c1), ZERO(c2)) => { MONE(crazy2adder(c1, c2, NONE)) }
              case (ZERO(c1), ONE(c2)) => { ZERO(crazy2adder(c1, c2, NONE)) }
              case (ZERO(c1), MONE(c2)) => { ZERO(crazy2adder(c1, c2, NEG)) }
              case (ONE(c1), ONE(c2)) => { ONE(crazy2adder(c1, c2, NONE)) }
              case (ONE(c1), MONE(c2)) => { MONE(crazy2adder(c1, c2, NONE)) }
              case (MONE(c1), MONE(c2)) => { MONE(crazy2adder(c1, c2, NEG)) }
              case (_, _) => { crazy2adder(y, x, carry) }
            }
          }
        }
      }
      crazy2adder(z1, z2, NONE)
    }
  }
}