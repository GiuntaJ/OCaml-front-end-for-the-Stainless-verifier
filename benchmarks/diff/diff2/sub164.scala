import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub164 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  	
  	
  	def length: List[Aexp] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { 0 }
          case Cons(hd, tl) => { 1 + length(tl) }
        }
    }
  )
  
    val diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        val _2 = {
          def diff_exp: (Aexp, String) => Aexp = {
            case (exp, var0) =>
              {
                exp match {
                  case Const(n) => { Const(0) }
                  case Var(value) => { if (var0 == value) Const(1) else Const(0)
                  }
                  case Power(value, n) => {
                    
                      if (
                        var0 == value
                      ) {
                        Times(List(Const(n), Power(value, n - 1))) 
                      } else {
                        Const(0)
                      }
                  }
                  case Sum(lst) => {
                    val _9 = {
                      def sum_diff: (List[Aexp], String) => List[Aexp] = {
                        case (sum_lst, key) =>
                          {
                            sum_lst match {
                              case Nil() => { Nil() }
                              case Cons(hd, tl) => {
                                diff_exp(hd, key) :: sum_diff(tl, key)
                              }
                            }
                        }
                      }
                      Sum(sum_diff(lst, var0))
                    }
                  }
                  case Times(lst) => {
                    val _5 = {
                      def filter: (List[Aexp], String, Int63, Int63) => List[Aexp] = {
                        case (lst, key, p, q) =>
                          {
                            lst match {
                              case Nil() => { Nil() }
                              case Cons(hd, tl) => {
                                
                                  if (
                                    p eq q
                                  ) {
                                    diff_exp(hd, key) ::
                                    filter(tl, key, p + 1, q) 
                                  } else {
                                    hd :: filter(tl, key, p + 1, q)
                                  }
                              }
                            }
                        }
                      }
                      val _6 = {
                        def time_diff: (List[Aexp], String, Int63, Int63) => List[Aexp] = {
                          case (time_lst, key, x, y) =>
                            {
                              
                                if (
                                  x <= y
                                ) {
                                  Times(filter(time_lst, key, 1, x)) ::
                                  time_diff(time_lst, key, x + 1, y) 
                                } else {
                                  Nil()
                                }
                          }
                        }
                        Sum(time_diff(lst, var0, 1, length(lst)))
                      }
                    }
                  }
                }
            }
          }
          diff_exp(exp, var0)
        }
    }
  }
}