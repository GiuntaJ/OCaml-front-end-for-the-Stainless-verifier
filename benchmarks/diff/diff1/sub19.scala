import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub19 {
  /* hw2-2 */
  /* 2010-11687 Keunjun Choi */
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((expr, value))) = {
    val _2 = {
      def sum_term(expr) = {
        expr match {
          case Nil() => { Nil() }
          case Cons(expr1, expr2) => { diff(expr1, value) :: sum_term(expr2) }
        }
      }
      val _3 = {
        def time_term(((expr, num))) = {
          val _6 = {
            def dx(((expr, count))) = {
              expr match {
                case Nil() => { Nil() }
                case Cons(expr1, expr2) => {
                  
                    if (
                      num < count || num > count
                    ) {
                      expr1 :: dx(expr2, count + 1) 
                    } else {
                      expr1 match {
                        case CONST(opr) => { CONST(0) :: dx(expr2, count + 1) }
                        case VAR(opr) => {
                          
                            if (
                              opr == value
                            ) {
                              CONST(1) :: dx(expr2, count + 1) 
                            } else {
                              CONST(0) :: dx(expr2, count + 1)
                            }
                        }
                        case POWER(opr1, opr2) => {
                          
                            if (
                              opr1 == value && opr2 ne 0
                            ) {
                              CONST(opr2) :: POWER(opr1, opr2 - 1) ::
                              dx(expr2, count + 1) 
                            } else {
                              CONST(0) :: dx(expr2, count + 1)
                            }
                        }
                        case TIMES(opr) => {
                          SUM(time_term(opr, 0)) :: dx(expr2, count + 1)
                        }
                        case SUM(opr) => {
                          SUM(sum_term(opr)) :: dx(expr2, count + 1)
                        }
                      }
                    }
                }
              }
            }
            
              if (
                num == expr.length
              ) {
                Nil() 
              } else {
                TIMES(dx(expr, 0)) :: time_term(expr, num + 1)
              }
          }
        }
        expr match {
          case CONST(opr) => { CONST(0) }
          case VAR(opr) => { if (opr == value) CONST(1) else CONST(0) }
          case POWER(opr1, opr2) => {
            
              if (
                opr1 == value && opr2 ne 0
              ) {
                TIMES(List(CONST(opr2), POWER(opr1, opr2 - 1))) 
              } else {
                CONST(0)
              }
          }
          case TIMES(opr) => { SUM(time_term(opr, 0)) }
          case SUM(opr) => { SUM(sum_term(opr)) }
        }
      }
    }
  }
}