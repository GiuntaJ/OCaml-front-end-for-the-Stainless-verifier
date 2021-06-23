import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub182 {
  /* problem 4*/
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
   def diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        val _2 = {
          def timediff(((tlst, x))) = {
            tlst match {
              case Nil() => { Nil() }
              case Cons(hd, tl) => {
                hd match {
                  case Const(c) => { Const(c) :: timediff(tl, x) }
                  case Var(v) => {
                    
                      if (
                        v == x
                      ) {
                        Const(1) :: timediff(tl, x) 
                      } else {
                        Const(0) :: timediff(tl, x)
                      }
                  }
                  case Power(v, c) => {
                    
                      if (
                        v == x
                      ) {
                        Times(List(Const(c), Power(v, c - 1))) ::
                        timediff(tl, x) 
                      } else {
                        Const(0) :: timediff(tl, x)
                      }
                  }
                }
              }
            }
          }
          val _3 = {
            def sumdiff(((slst, x))) = {
              slst match {
                case Nil() => { Nil() }
                case Cons(hd, tl) => {
                  hd match {
                    case Const(c) => { Const(0) :: sumdiff(tl, x) }
                    case Var(v) => {
                      
                        if (
                          v == x
                        ) {
                          Const(1) :: sumdiff(tl, x) 
                        } else {
                          Const(0) :: sumdiff(tl, x)
                        }
                    }
                    case Power(v, c) => {
                      
                        if (
                          v == x
                        ) {
                          Times(List(Const(c), Power(v, c - 1))) ::
                          sumdiff(tl, x) 
                        } else {
                          Const(0) :: sumdiff(tl, x)
                        }
                    }
                    case Sum(lst) => { Sum(sumdiff(lst, x)) :: sumdiff(tl, x) }
                    case Times(lst) => {
                      Times(timediff(lst, x)) :: sumdiff(tl, x)
                    }
                  }
                }
              }
            }
            val _4 = {
              def differ(((e, x))) = {
                e match {
                  case Const(c) => { Const(0) }
                  case Var(v) => { if (v == x) Const(1) else Const(0) }
                  case Power(v, c) => {
                    
                      if (
                        v == x
                      ) {
                        Times(List(Const(c), Power(v, c - 1))) 
                      } else {
                        Const(0)
                      }
                  }
                  case Sum(lst) => { Sum(sumdiff(lst, x)) }
                  case Times(lst) => { Times(timediff(lst, x)) }
                }
              }
              differ(e, x)
            }
          }
        }
    }
  }
}