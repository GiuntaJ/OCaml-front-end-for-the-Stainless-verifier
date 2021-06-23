import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub10 {
  /* ex2 */
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((ex, var0))) = {
    val _2 = {
      def diffSum(e) = { diff(e, var0) }
      val _3 = {
        def replace(lst, n, elem) = {
          val _6 = {
            def get_result(res, prev) = {
              prev match {
                case Nil() => { res }
                case Cons(hd, tl) => {
                  
                    if (
                      res.length == n
                    ) {
                      res ++ List(elem) ++ tl 
                    } else {
                      get_result(res ++ List(hd), tl)
                    }
                }
              }
            }
            get_result(Nil(), lst)
          }
        }
        val _7 = {
          def diffTimes(lst) = {
            val _10 = {
              def looper(i, n) = {
                
                  if (
                    i == n
                  ) {
                    Nil() 
                  } else {
                    TIMES(replace(lst, i, diff(lst.apply(i), var0))) ::
                    looper(i + 1, n)
                  }
              }
              SUM(looper(0, lst.length))
            }
          }
          ex match {
            case CONST(i) => { CONST(0) }
            case VAR(v) => { if (v == var0) CONST(1) else CONST(0) }
            case POWER(v, i) => {
              
                if (
                  v == var0
                ) {
                  
                    if (
                      i == 1
                    ) {
                      CONST(i) 
                    } else {
                      TIMES(List(CONST(i), POWER(v, i - 1)))
                    } 
                } else {
                  CONST(0)
                }
            }
            case TIMES(lst) => { diffTimes(lst) }
            case SUM(lst) => { SUM(lst.map(diffSum)) }
          }
        }
      }
    }
  }
}