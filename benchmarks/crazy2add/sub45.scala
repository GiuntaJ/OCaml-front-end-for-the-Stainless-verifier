import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub45 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  sealed abstract class Carry {}
  case object NONE extends Carry {}
  case object PLUS extends Carry {}
  case object MINUS extends Carry {}
  
  def crazy2add(((a, b))) = {
    val _2 = {
      def carryToCrazy(c, subCrazy) = {
        c match {
          case NONE => { ZERO(subCrazy) }
          case PLUS => { ONE(subCrazy) }
          case MINUS => { MONE(subCrazy) }
        }
      }
      val _3 = {
        def addWithCarry(((a, b, c))) = {
          (a, b, c) match {
            case (NIL, NIL, c) => { carryToCrazy(c, NIL) }
            case (NIL, b, c) => { addWithCarry(ZERO(NIL), b, c) }
            case (a, NIL, c) => { addWithCarry(a, ZERO(NIL), c) }
            case (ZERO(subA), ZERO(subB), c) => {
              carryToCrazy(c, addWithCarry(subA, subB, NONE))
            }
            case (ONE(subA), ONE(subB), c) => {
              carryToCrazy(c, addWithCarry(subA, subB, PLUS))
            }
            case (MONE(subA), MONE(subB), c) => {
              carryToCrazy(c, addWithCarry(subA, subB, MINUS))
            }
            case (ONE(subA), MONE(subB), c) | (MONE(subA), ONE(subB), c) => {
              carryToCrazy(c, addWithCarry(subA, subB, NONE))
            }
            case (ZERO(subA), ONE(subB), NONE) | (ONE(subA), ZERO(subB), NONE) => {
              ONE(addWithCarry(subA, subB, NONE))
            }
            case (ZERO(subA), MONE(subB), NONE) | (MONE(subA), ZERO(subB), NONE) => {
              MONE(addWithCarry(subA, subB, NONE))
            }
            case (ZERO(subA), b, c) => {
              addWithCarry(carryToCrazy(c, subA), b, NONE)
            }
            case (a, ZERO(subB), c) => {
              addWithCarry(a, carryToCrazy(c, subB), NONE)
            }
          }
        }
        addWithCarry(a, b, NONE)
      }
    }
  }
}