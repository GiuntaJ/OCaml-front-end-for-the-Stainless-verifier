import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub211 {
  /*problem 4*/
  
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
   
  def diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        e match {
          case Const(a) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(a, b) => {
            
              if (
                a == x
              ) {
                Times(List(Const(b), Power(a, b - 1))) 
              } else {
                Times(List(Const(0), Var(a)))
              }
          }
          case Times(a) => {
            a match {
              case Cons(hd, tl) => {
                tl match {
                  case Nil() => { diff(hd, x) }
                  case _ => {
                    val _7 = {
                      val l1 = diff(hd, x) :: tl
                      val _8 = {
                        val l2 = List(hd) ++ List(diff(Times(tl), x))
                        val _9 = {
                          val l3 = List(Times(l1), Times(l2))
                          Sum(l3)
                        }
                      }
                    }
                  }
                }
              }
              case Nil() => { Const(0) }
            }
          }
          case Sum(a) => {
            a match {
              case Cons(hd, tl) => {
                tl match {
                  case Nil() => { diff(hd, x) }
                  case _ => {
                    val _2 = {
                      val l1 = diff(hd, x)
                      val _3 = {
                        val l2 = diff(Sum(tl), x)
                        val _4 = {
                          val l3 = List(l1, l2)
                          Sum(l3)
                        }
                      }
                    }
                  }
                }
              }
              case Nil() => { Const(0) }
            }
          }
        }
    }
  }
}