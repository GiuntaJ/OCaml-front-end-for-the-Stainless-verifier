import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub77 {
  /* KIHWAN KANG HW02-2 */
  
  /* PREDEFINED TYPES */
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  /* END OF PREDEFINED TYPES */
  
  def diff(((expr, var_ref))) = {
    val _2 = {
      def lowerPow(((var0, pow))) = {
        
          if (
            pow == 1
          ) {
            CONST(1) 
          } else {
            TIMES(List(CONST(pow), POWER(var0, pow - 1)))
          }
      }
      val _3 = {
        def diffSum(((aelist, var_ref))) = {
          aelist match {
            case Nil() => { Nil() }
            case Cons(head, tail) => {
              diff(head, var_ref) :: diffSum(tail, var_ref)
            }
          }
        }
        val _4 = {
          def diffTim(((prelist, aelist, var_ref))) = {
            aelist match {
              case Nil() => { Nil() }
              case Cons(head, tail) => {
                TIMES(prelist ++ List(diff(head, var_ref)) ++ tail) ::
                diffTim(prelist ++ List(head), tail, var_ref)
              }
            }
          }
          expr match {
            case CONST(_) => { CONST(0) }
            case VAR(var0) => { if (var0 == var_ref) CONST(1) else CONST(0) }
            case POWER(var0, pow) => {
              if (var0 == var_ref && pow ne 0) lowerPow(var0, pow) else CONST(0)
            }
            case TIMES(aelist) => {
              aelist match {
                case Nil() => { assert(false, "InvalidArgument") }
                case Cons(head, tail) => { SUM(diffTim(Nil(), aelist, var_ref))
                }
              }
            }
            case SUM(aelist) => {
              aelist match {
                case Nil() => { assert(false, "InvalidArgument") }
                case Cons(head, tail) => { SUM(diffSum(aelist, var_ref)) }
              }
            }
          }
        }
      }
    }
  }
}