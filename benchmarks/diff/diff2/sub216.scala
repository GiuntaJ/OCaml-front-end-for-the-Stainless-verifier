import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub216 {
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
          def diff_calculation: (Aexp, String) => Aexp = {
            case (e, x) =>
              {
                e match {
                  case Const(a) => { Const(0) }
                  case Sum(l) => {
                    l match {
                      case Nil() => { Sum(Nil()) }
                      case Cons(a, Nil()) => { diff_calculation(a, x) }
                      case Cons(hd, tl) => {
                        hd match {
                          case Const(_) => { diff_calculation(Sum(tl), x) }
                          case _ => {
                            Sum(
                              List(diff_calculation(hd, x)) ++(List(diff_calculation(Sum(tl), x))))
                          }
                        }
                      }
                    }
                  }
                  case Power(a, b) => {
                    
                      if (
                        x == a
                      ) {
                        
                          if (
                            b eq 1
                          ) {
                            Const(1) 
                          } else {
                            Times(List(Const(b), Power(a, b - 1)))
                          } 
                      } else {
                        Const(0)
                      }
                  }
                  case Times(l) => {
                    l match {
                      case Nil() => { Const(0) }
                      case Cons(a, Nil()) => { diff_calculation(a, x) }
                      case Cons(hd, tl) => {
                        hd match {
                          case Const(0) => { Const(0) }
                          case _ => {
                            Sum(
                              List(Times(
                                 List(hd, diff_calculation(Times(tl), x))),
                               Times(List(diff_calculation(hd, x), Times(tl)))))
                          }
                        }
                      }
                    }
                  }
                  case Var(a) => { if (x == a) Const(1) else Const(0) }
                }
            }
          }
          val _3 = {
            def beautiful_diff: Aexp => Aexp = (
              (e) =>
                {
                  e match {
                    case Const(a) => { Const(a) }
                    case Var(a) => { Var(a) }
                    case Power(a, b) => { Power(a, b) }
                    case Times(l) => {
                      l match {
                        case Nil() => { Const(0) }
                        case Cons(a, Nil()) => { beautiful_diff(a) }
                        case Cons(hd, tl) => {
                          hd match {
                            case Times(Nil()) => {
                              Times(List(beautiful_diff(Times(tl))))
                            }
                            case Sum(Nil()) => {
                              Times(List(beautiful_diff(Times(tl))))
                            }
                            case Const(1) => {
                              Times(List(beautiful_diff(Times(tl))))
                            }
                            case Const(0) => { Const(0) }
                            case _ => {
                              Times(
                                List(hd) ++(List(beautiful_diff(Times(tl)))))
                            }
                          }
                        }
                      }
                    }
                    case Sum(l) => {
                      l match {
                        case Nil() => { Const(0) }
                        case Cons(a, Nil()) => { beautiful_diff(a) }
                        case Cons(hd, tl) => {
                          hd match {
                            case Times(Nil()) => {
                              Sum(List(beautiful_diff(Times(tl))))
                            }
                            case Sum(Nil()) => {
                              Sum(List(beautiful_diff(Times(tl))))
                            }
                            case Const(0) => {
                              Sum(List(beautiful_diff(Times(tl))))
                            }
                            case _ => {
                              Sum(List(hd) ++(List(beautiful_diff(Times(tl)))))
                            }
                          }
                        }
                      }
                    }
                  }
              }
            )
            beautiful_diff(diff_calculation(e, x))
          }
        }
    }
  }
}