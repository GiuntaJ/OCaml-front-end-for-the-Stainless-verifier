import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub135 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    val diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        val _2 = {
          def down(e) = {
            e match {
              case Var(s) => { if (s == var0) Const(1) else Const(0) }
              case Power(s, n) => {
                
                  if (
                    s == var0
                  ) {
                    n match {
                      case 0 => { Const(0) }
                      case 1 => { Const(1) }
                      case _ => { Times(List(Const(n), Power(s, n - 1))) }
                    } 
                  } else {
                    Const(0)
                  }
              }
              case Const(_) | Times(Nil()) | Sum(Nil()) => { Const(0) }
              case Times(Cons(h, Nil())) | Sum(Cons(h, Nil())) => { down(h) }
              case Times(l) => {
                val _8 = {
                  def loop(p, f, r) = {
                    r match {
                      case Cons(h, t) => {
                        loop(
                          p ++ List(Times(f ++ List(down(h)) ++ t)),
                          f ++ List(h), t)
                      }
                      case _ => { p }
                    }
                  }
                  Sum(loop(Nil(), Nil(), l))
                }
              }
              case Sum(l) => {
                val _5 = {
                  def loop(p, r) = {
                    r match {
                      case Cons(h, t) => { loop(p ++ List(down(h)), t) }
                      case _ => { p }
                    }
                  }
                  Sum(loop(Nil(), l))
                }
              }
            }
          }
          val _9 = {
            def simplify(e) = {
              e match {
                case Sum(l) => {
                  val _15 = {
                    def loop(n, x, r) = {
                      r match {
                        case Cons(h, t) => {
                          simplify(h) match {
                            case Const(m) => { loop(n + m, x, t) }
                            case sh => { loop(n, x ++ List(sh), t) }
                          }
                        }
                        case _ => {
                          (n, x) match {
                            case (sn, Nil()) => { Const(sn) }
                            case (0, sx) => { Sum(sx) }
                            case (sn, sx) => { Sum(sx ++ List(Const(sn))) }
                          }
                        }
                      }
                    }
                    loop(0, Nil(), l)
                  }
                }
                case Times(l) => {
                  val _12 = {
                    def loop(n, x, r) = {
                      r match {
                        case Cons(h, t) => {
                          simplify(h) match {
                            case Const(m) => { loop(n * m, x, t) }
                            case sh => { loop(n, x ++ List(sh), t) }
                          }
                        }
                        case _ => {
                          (n, x) match {
                            case (0, _) => { Const(0) }
                            case (sn, Nil()) => { Const(sn) }
                            case (1, sx) => { Times(sx) }
                            case (sn, sx) => { Times(Const(sn) :: sx) }
                          }
                        }
                      }
                    }
                    loop(1, Nil(), l)
                  }
                }
                case Power(s, 1) => { Var(s) }
                case Power(s, 0) => { Const(1) }
                case _ => { e }
              }
            }
            simplify(down(exp))
          }
        }
    }
  }
}