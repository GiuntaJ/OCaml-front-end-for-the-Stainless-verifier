import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub238 {
  /* problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  val diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        val _2 = {
          def rev_list(lst, num) = {
            lst match {
              case Cons(hd, tl) => {
                
                  if (
                    tl.length == num
                  ) {
                    Nil() ++ rev_list(tl, num) 
                  } else {
                    List(hd) ++ rev_list(tl, num)
                  }
              }
              case Nil() => { Nil() }
            }
          }
          val _3 = {
            def tl_list(e) = {
              e match {
                case Cons(hd, tl) => { Sum(tl) }
                case Nil() => { Sum(Nil()) }
              }
            }
            val _4 = {
              def differ(e, x) = {
                e match {
                  case Const(n) => { Const(0) }
                  case Var(a) => { if (a == x) Const(1) else Var(a) }
                  case Power(a, b) => {
                    
                      if (
                        a == x
                      ) {
                        Times(List(Const(b), Power(a, b - 1))) 
                      } else {
                        Const(0)
                      }
                  }
                  case Times(lst) => {
                    val _7 = {
                      def times_diff(lst2) = {
                        lst2 match {
                          case Cons(hd, tl) => {
                            
                              if (
                                tl == Nil()
                              ) {
                                Times(
                                  differ(hd, x) :: rev_list(lst, tl.length)) 
                              } else {
                                Sum(
                                  List(Times(
                                     differ(hd, x) :: rev_list(lst, tl.length)),
                                   times_diff(tl)))
                              }
                          }
                          case Nil() => { Sum(List(Const(0))) }
                        }
                      }
                      times_diff(lst)
                    }
                  }
                  case Sum(lst) => {
                    lst match {
                      case Cons(hd, tl) => {
                        
                          if (
                            tl == Nil()
                          ) {
                            Sum(List(differ(hd, x))) 
                          } else {
                            Sum(List(differ(hd, x), differ(tl_list(lst), x)))
                          }
                      }
                      case Nil() => { Sum(List(Const(0))) }
                    }
                  }
                }
              }
              differ(e, x)
            }
          }
        }
    }
  }
}