import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub296 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def map_2(f, a, lst) = {
    lst match {
      case Nil() => { a }
      case Cons(hd, tl) => {
        val _2 = {
          val r = f(a, hd)
          map_2(f, r, tl)
        }
      }
    }
  }
  
  val get_type: Aexp => Int63 = (
    (exp) =>
      {
        exp match {
          case Const(n) => { n }
          case Power(str, n) => { n }
          case Sum(lst) => { lst.length }
          case _ => { 0 }
        }
    }
  )
  
  val get_list: Aexp => List[Aexp] = (
    (exp) =>
      {
        exp match {
          case Sum(lst) => { lst }
          case Times(lst) => { lst }
          case _ => { Nil() }
        }
    }
  )
  
  
  def map_lst: ((Aexp, String) => Aexp, Aexp, String) => List[Aexp] = {
    case (f, exp, str) =>
      {
        val _5 = {
          val a = get_list(exp)
          a match {
            case Nil() => { Nil() }
            case Cons(hd, tl) => {
              val _8 = {
                val b = f(hd, str)
                val _9 = {
                  val c = map_lst(f, Sum(tl), str)
                  b :: c
                }
              }
            }
          }
        }
    }
  }
  
  def eval(time_times): (Aexp, String) => Aexp = {
    {
      case (exp, x) =>
        {
          exp match {
            case Const(n) => { exp }
            case Power(str, n) => { exp }
            case Var(str) => { exp }
            case Times(lst) => {
              val _15 = {
                val a = get_list(exp)
                a match {
                  case Cons(hd, tl) => {
                    time_times(
                      Times(List(Const(1), Power("x", 0))), hd, Times(tl))
                  }
                  case Nil() => { Times(Nil()) }
                }
              }
            }
            case Sum(lst) => {
              val _12 = {
                val c = map_lst(eval(time_times), exp, x)
                Sum(c)
              }
            }
          }
      }
    }
  }
  def sum_time(time_sum, exp, lst) = {
    val _18 = {
      val a = get_list(lst)
      exp match {
        case Const(_) | Var(_) | Power(_, _) | Times(_) => {
          a match {
            case Cons(hd, tl) => {
              Times(List(exp, hd)) :: sum_time(time_sum, exp, Sum(tl))
            }
            case Nil() => { Nil() }
          }
        }
        case Sum(lst2) => { time_sum(exp, lst) }
      }
    }
  }
  
  def time_sum(lst1, lst2) = {
    val _21 = {
      val b = get_list(lst2)
      b match {
        case Cons(hd, tl) => {
          val _24 = {
            val a = sum_time(time_sum, hd, lst1)
            a reverse_:::(time_sum(lst1, Sum(tl)))
          }
        }
        case Nil() => { Nil() }
      }
    }
  }
  
  def time_times: (Aexp, Aexp, Aexp) => Aexp = {
    case (exp1, x, exp2) =>
      {
        val _27 = {
          val a = get_list(exp1)
          val _28 = {
            val b = get_list(exp2)
            x match {
              case Const(n) => {
                val _59 = {
                  val c = n
                  val _60 = {
                    val d = get_type(a.apply(0))
                    b match {
                      case Cons(hd, tl) => {
                        time_times(
                          Times(List(Const(d * c), a.apply(1))), hd, Times(tl))
                      }
                      case Nil() => { Times(List(Const(d * c), a.apply(1))) }
                    }
                  }
                }
              }
              case Var(str) => {
                a.apply(1) match {
                  case Power(str, n) => {
                    b match {
                      case Cons(hd, tl) => {
                        time_times(
                          Times(List(a.apply(0), Power(str, n + 1))), hd,
                          Times(tl))
                      }
                      case Nil() => { Times(List(a.apply(0), Power(str, n + 1)))
                      }
                    }
                  }
                  case _ => {
                    assert(
                      false,
                      "Failure with                     type error in time_times->Var()")
                  }
                }
              }
              case Power(str, n) => {
                val _56 = {
                  val c = n
                  a.apply(1) match {
                    case Power(str, n) => {
                      b match {
                        case Cons(hd, tl) => {
                          time_times(
                            Times(List(a.apply(0), Power(str, n + c))), hd,
                            Times(tl))
                        }
                        case Nil() => {
                          Times(List(a.apply(0), Power(str, n + c)))
                        }
                      }
                    }
                    case _ => {
                      assert(
                        false,
                        "Failure with                       type error in time_times->Var()")
                    }
                  }
                }
              }
              case Times(lst) => {
                val _45 = {
                  val p = get_list(x)
                  p match {
                    case Cons(Const(int1), Cons(Power(str, int2), Nil())) => {
                      val _51 = {
                        val e = get_type(p.apply(0))
                        val _52 = {
                          val g = get_type(p.apply(1))
                          val _53 = {
                            val h = str
                            b match {
                              case Nil() => {
                                Times(
                                  List(Const(get_type(a.apply(0)) * e),
                                   Power(h, get_type(a.apply(1)) + g)))
                              }
                              case Cons(hd, tl) => {
                                time_times(
                                  Times(
                                    List(Const(get_type(a.apply(0)) * e),
                                     Power(h, get_type(a.apply(1)) + g))),
                                  hd, Times(tl))
                              }
                            }
                          }
                        }
                      }
                    }
                    case Cons(hd, tl) => {
                      val _48 = {
                        val y = time_times(Times(List(Const(1), Power("x", 0))), hd, Times(tl))
                        time_times(exp1, y, exp2)
                      }
                    }
                    case Nil() => {
                      b match {
                        case Cons(hd, tl) => { time_times(exp1, hd, Times(tl)) }
                        case Nil() => { exp1 }
                      }
                    }
                  }
                }
              }
              case Sum(lst) => {
                a.apply(1) match {
                  case Power(str, n) => {
                    val _31 = {
                      val h = str
                      val _32 = {
                        val e = sum_time(time_sum, exp1, x)
                        val _33 = {
                          val q = eval(time_times, Sum(e), h)
                          b match {
                            case Nil() => { q }
                            case Cons(hd, tl) => {
                              val _36 = {
                                val j = time_times(Times(List(Const(1), Power("x", 0))), hd, Times(tl))
                                j match {
                                  case Times(lst) => {
                                    val _42 = {
                                      val k = sum_time(time_sum, j, q)
                                      eval(time_times, Sum(k), h)
                                    }
                                  }
                                  case Sum(lst) => {
                                    val _39 = {
                                      val p = time_sum(j, q)
                                      eval(time_times, Sum(p), h)
                                    }
                                  }
                                  case _ => {
                                    assert(
                                      false,
                                      "Failure with                                     Type error: j must be Times or Sum")
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
    }
  }                                      
  
  def differ: Aexp => Aexp = (
    (exp) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(str) => { Const(1) }
          case Power(str, n) => {
            
              if (
                n ne 0
              ) {
                Times(List(Const(n), Power(str, n - 1))) 
              } else {
                Times(List(Const(n)))
              }
          }
          case Times(lst) => {
            lst match {
              case Cons(Const(int1), Cons(Power(str, int2), Nil())) => {
                
                  if (
                    int2 == 0
                  ) {
                    Const(0) 
                  } else {
                    Times(List(Const(int1 * int2), Power(str, int2 - 1)))
                  }
              }
              case _ => { assert(false, "Failure with type error in differ") }
            }
          }
          case Sum(lst) => {
            val _63 = {
              val a = lst.map(differ)
              Sum(a)
            }
          }
        }
    }
  )
  
  val diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        val _66 = {
          val a = eval(time_times, exp, x)
          differ(a)
        }
    }
  }
}