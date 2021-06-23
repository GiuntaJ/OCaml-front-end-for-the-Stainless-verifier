import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub204 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  sealed abstract class Carry {}
  case object PLUS extends Carry {}
  case object MINUS extends Carry {}
  case object NONE extends Carry {}
  
  val crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) =>
      {
        val _2 = {
          def fulladd: (Crazy2, Crazy2, Carry) => Crazy2 = {
            case (a, b, c) =>
              {
                (a, b, c) match {
                  case (NIL, NIL, NONE) => { ZERO(NIL) }
                  case (NIL, NIL, PLUS) => { ONE(NIL) }
                  case (NIL, NIL, MINUS) => { MONE(NIL) }
                  case (NIL, _, NONE) => { b }
                  case (NIL, MONE(b1), PLUS) => { ZERO(b1) }
                  case (NIL, ZERO(b1), PLUS) => { ONE(b1) }
                  case (NIL, ONE(b1), PLUS) => { ZERO(fulladd(NIL, b1, PLUS)) }
                  case (NIL, ONE(b1), MINUS) => { ZERO(b1) }
                  case (NIL, ZERO(b1), MINUS) => { MONE(b1) }
                  case (NIL, MONE(b1), MINUS) => { ZERO(fulladd(NIL, b1, MINUS))
                  }
                  case (_, NIL, NONE) => { a }
                  case (MONE(a1), NIL, PLUS) => { ZERO(a1) }
                  case (ZERO(a1), NIL, PLUS) => { ONE(a1) }
                  case (ONE(a1), NIL, PLUS) => { ZERO(fulladd(a1, NIL, PLUS)) }
                  case (ONE(a1), NIL, MINUS) => { ZERO(a1) }
                  case (ZERO(a1), NIL, MINUS) => { MONE(a1) }
                  case (MONE(a1), NIL, MINUS) => { ZERO(fulladd(NIL, a1, MINUS))
                  }
                  case (ONE(a1), MONE(b1), NONE) | (MONE(a1), ONE(b1), NONE) |
                  (ZERO(a1), ZERO(b1), NONE) => {
                    ZERO(fulladd(a1, b1, NONE))
                  }
                  case (ZERO(a1), ONE(b1), NONE) | (ONE(a1), ZERO(b1), NONE) => {
                    ONE(fulladd(a1, b1, NONE))
                  }
                  case (ZERO(a1), MONE(b1), NONE) | (MONE(a1), ZERO(b1), NONE) => {
                    MONE(fulladd(a1, b1, NONE))
                  }
                  case (ONE(a1), ONE(b1), NONE) => { ZERO(fulladd(a1, b1, PLUS))
                  }
                  case (MONE(a1), MONE(b1), NONE) => {
                    ZERO(fulladd(a1, b1, MINUS))
                  }
                  case (MONE(a1), ZERO(b1), PLUS) | (ZERO(a1), MONE(b1), PLUS) => {
                    ZERO(fulladd(a1, b1, NONE))
                  }
                  case (ONE(a1), MONE(b1), PLUS) | (MONE(a1), ONE(b1), PLUS) |
                  (ZERO(a1), ZERO(b1), PLUS) => {
                    ONE(fulladd(a1, b1, NONE))
                  }
                  case (MONE(a1), MONE(b1), PLUS) => {
                    MONE(fulladd(a1, b1, NONE))
                  }
                  case (ZERO(a1), ONE(b1), PLUS) | (ONE(a1), ZERO(b1), PLUS) => {
                    ZERO(fulladd(a1, b1, PLUS))
                  }
                  case (ONE(a1), ONE(b1), PLUS) => { ONE(fulladd(a1, b1, PLUS))
                  }
                  case (ONE(a1), ZERO(b1), MINUS) | (ZERO(a1), ONE(b1), MINUS) => {
                    ZERO(fulladd(a1, b1, NONE))
                  }
                  case (ONE(a1), MONE(b1), MINUS) | (MONE(a1), ONE(b1), MINUS) |
                  (ZERO(a1), ZERO(b1), MINUS) => {
                    MONE(fulladd(a1, b1, NONE))
                  }
                  case (ONE(a1), ONE(b1), MINUS) => { ONE(fulladd(a1, b1, NONE))
                  }
                  case (ZERO(a1), MONE(b1), MINUS) | (MONE(a1), ZERO(b1), MINUS) => {
                    ZERO(fulladd(a1, b1, MINUS))
                  }
                  case (MONE(a1), MONE(b1), MINUS) => {
                    MONE(fulladd(a1, b1, MINUS))
                  }
                }
            }
          }
          fulladd(a, b, NONE)
        }
    }
  }
}