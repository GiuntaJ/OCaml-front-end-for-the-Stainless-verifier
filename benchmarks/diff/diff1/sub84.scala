import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub84 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((ae, instr))) = {
    ae match {
      case CONST(a) => { CONST(0) }
      case VAR(x) => { if (x == instr) CONST(1) else CONST(0) }
      case POWER(x, y) => {
        
          if (
            x == instr && y == 0
          ) {
            CONST(0) 
          } else if (
            x == instr && y != 0
          ) {
            TIMES(List(CONST(y), POWER(x, y - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(Nil()) => { assert(false, "InvalidArgument") }
      case TIMES(lst) => { timeProcess(lst, instr) }
      case SUM(Nil()) => { assert(false, "InvalidArgument") }
      case SUM(lst) => { SUM(lst.map(( (x) => { diff(x, instr) } ))) }
    }
  }
  def timeProcess(((l, istr))) = {
    val _9 = {
      def check(x) = {
        x match {
          case VAR(x) => { if (x == istr) true else false }
          case POWER(x, _) => { if (x == istr) true else false }
          case _ => { false }
        }
      }
      val _10 = {
        val varList = l.filter(( (x) => { check(x) } ))
        
          if (
            l.exists(( (x) => { check(x) } ))
          ) {
            TIMES(
              diff(polyProcess(varList, istr), istr) ::
              l.filter(( (x) => { not(check(x)) } ))) 
          } else {
            CONST(0)
          }
      }
    }
  }
  def polyProcess(((l, is))) = {
    val _2 = {
      val varToPower = (
        x =>
          x match {
            case VAR(x) => { POWER(x, 1) }
            case POWER(x, y) => { POWER(x, y) }
            case _ => { assert(false, "InvalidArgument") }
          }
      )
      val _3 = {
        def powerFold(x) = {
          (
            x =>
              x match {
                case POWER(_, a) => { x + a }
                case _ => { assert(false, "InvalidArgument") }
              }
          )
        }
        val _4 = {
          val poweredList = l.map(( (x) => { varToPower(x) } ))
          val _5 = {
            def fold_left(f, a, l) = {
              l match {
                case Nil() => { a }
                case Cons(h, t) => { fold_left(f, f(a, h), t) }
              }
            }
            val _6 = {
              val muled = fold_left(powerFold, 0, poweredList)
              POWER(is, muled)
            }
          }
        }
      }
    }
  }
}