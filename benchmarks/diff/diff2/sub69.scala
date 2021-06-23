import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub69 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def check_times: (List[Aexp], String) => Int63 = {
    case (li, str) =>
      {
        li match {
          case Nil() => { 0 }
          case Cons(h, t) => {
            h match {
              case Var(str3) => { if (str == str3) 1 else check_times(t, str) }
              case Power(str2, a) => {
                if (str == str2) 2 else check_times(t, str)
              }
              case _ => { check_times(t, str) }
            }
          }
        }
    }
  }
  
  def align_head: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Times(Cons(h, t)) => {
            
              if (
                h == Var(x)
              ) {
                Times(h :: t) 
              } else {
                align_head(Times(t ++ List(h)), x)
              }
          }
          case _ => { assert(false, "Failure with HowDidYouGetHere") }
        }
    }
  }
  
  val work_head: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Times(Cons(h, t)) => { Times(t) }
          case _ => { assert(false, "Failure with HowDidYouGetHere") }
        }
    }
  }
  
  def align_head_power: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Times(Cons(h, t)) => {
            h match {
              case Power(x, _) => { Times(h :: t) }
              case _ => { align_head_power(Times(t ++ List(h)), x) }
            }
          }
          case _ => { assert(false, "Failure with HowDidYouGetHere") }
        }
    }
  }
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Sum(Cons(h, t)) => { Sum(List(diff(h, x), diff(Sum(t), x))) }
          case Times(li) => {
            check_times(li, x) match {
              case 0 => { Const(0) }
              case 1 => { work_head(align_head(Times(li), x), x) }
              case 2 => { work_head_power(align_head_power(Times(li), x), x) }
              case _ => { Const(0) }
            }
          }
          case Sum(Nil()) => { Const(0) }
          case Const(a) => { Const(0) }
          case Var(str) => { if (str == x) Const(1) else Const(0) }
          case Power(str, a) => {
            if (str == x) Times(List(Const(a), Power(str, a - 1))) else Const(0)
          }
        }
    }
  }
  def work_head_power: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Times(Cons(h, t)) => { Times(List(diff(h, x)) ++ t) }
          case _ => { assert(false, "Failure with HowDidYouGetHere") }
        }
    }
  }
}