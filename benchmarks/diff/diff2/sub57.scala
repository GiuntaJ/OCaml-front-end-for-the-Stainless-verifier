import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub57 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Const(n) => { Const(0) }
          case Var(st) => { if (st == x) Const(1) else Const(0) }
          case Power(st, n) => {
            if (st == x) Times(List(Const(n), Power(st, n - 1))) else Const(0)
          }
          case Times(Nil()) => { Const(0) }
          case Times(Cons(hd, Nil())) => { Times(List(diff(hd, x), Const(1))) }
          case Times(Cons(hd, tl)) => {
            val _2 = {
              val hdd = diff(hd, x)
              val _3 = {
                val ttl = Times(tl)
                val _4 = {
                  val hdp = Times(List(hdd, ttl))
                  val _5 = {
                    val tlp = Times(List(hd, diff(ttl, x)))
                    Sum(List(hdp, tlp))
                  }
                }
              }
            }
          }
          case Sum(Nil()) => { Const(0) }
          case Sum(Cons(hd, Nil())) => { Sum(List(diff(hd, x), Const(0))) }
          case Sum(Cons(hd, tl)) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
        }
    }
  }
}