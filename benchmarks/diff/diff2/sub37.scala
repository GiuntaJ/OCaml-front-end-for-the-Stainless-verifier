import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub37 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  /*let rec diff_cln aexp = */
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Const(i) => { Const(0) }
          case Var(v) => { if (v == x) Const(1) else Var(v) }
          case Power(v, p) => {
            
              if (
                v == x
              ) {
                p match {
                  case 1 => { Const(1) }
                  case 2 => { Times(List(Const(p), Var(v))) }
                  case _ => { Times(List(Const(p), Power(v, p - 1))) }
                } 
              } else {
                Power(v, p)
              }
          }
          case Times(l1) => {
            val _5 = {
              def iter(l) = {
                l match {
                  case Nil() => { Nil() }
                  case Cons(h, t) => {
                    val _8 = {
                      def isConst(l) = {
                        l match {
                          case Nil() => { true }
                          case Cons(h, t) => {
                            h match {
                              case Const(c) => { isConst(t) }
                              case _ => { false }
                            }
                          }
                        }
                      }
                      
                        if (
                          isConst(l)
                        ) {
                          List(Const(0)) 
                        } else {
                          h match {
                            case Const(c) => { h :: iter(t) }
                            case Sum(lx) => { h :: iter(t) }
                            case _ => { diff(h, x) :: iter(t) }
                          }
                        }
                    }
                  }
                }
              }
              Times(iter(l1))
            }
          }
          case Sum(l2) => {
            val _2 = {
              def iter(l) = {
                l match {
                  case Nil() => { Nil() }
                  case Cons(h, t) => {
                    h match {
                      case Const(0) => { iter(t) }
                      case Times(Cons(Const(0), Nil())) => { iter(t) }
                      case _ => { diff(h, x) :: iter(t) }
                    }
                  }
                }
              }
              Sum(iter(l2))
            }
          }
        }
    }
  }
}