import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub127 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  type V_set = List[Var]
  
  def union1: (V_set, V_set) => V_set = {
    case (s1, s2) =>
      {
        s1 match {
          case Nil() => { s2 }
          case Cons(hd, tl) => {
            if (s2.contains(hd)) union1(tl, s2) else union1(tl, hd :: s2)
          }
        }
    }
  }
  
  def find_erase: (Var, V_set, V_set) => V_set = {
    case (v, s, sc) =>
      {
        s match {
          case Cons(hd, tl) => {
            if (hd == v) tl ++ sc else find_erase(v, tl, hd :: sc)
          }
          case Nil() => { sc }
        }
    }
  }
  
  def union2: (V_set, V_set) => V_set = {
    case (s1, s2) =>
      {
        s1 match {
          case Nil() => { s1 }
          case Cons(hd, tl) => {
            if (s2.contains(hd)) hd :: union2(tl, s2) else union2(tl, s2)
          }
        }
    }
  }
  
  def save: (Lambda, V_set) => V_set = {
    case (lam, s) =>
      {
        lam match {
          case P(_, l) => { save(l, s) }
          case C(l1, l2) => { union1(save(l1, s), save(l2, s)) }
          case V(v) => { if (s.contains(v)) s else v :: s }
        }
    }
  }
  
  def erase: (Lambda, V_set) => V_set = {
    case (lam, s) =>
      {
        lam match {
          case P(v, l) => { erase(l, find_erase(v, s, Nil())) }
          case C(l1, l2) => { union2(erase(l1, s), erase(l2, s)) }
          case V(v) => { s }
        }
    }
  }
  
  val check: Lambda => Boolean = (
    (lam) =>
      {
        erase(lam, save(lam, Nil())) match {
          case Nil() => { true }
          case Cons(hd, tl) => { false }
        }
    }
  )
  
  check(P("a", V("a")))
  check(P("a", P("a", V("a"))))
  check(P("a", P("b", C(V("a"), V("b")))))
  check(P("a", C(V("a"), P("b", V("a")))))
  
  check(P("a", V("b")))
  check(P("a", C(V("a"), P("b", V("c")))))
  check(P("a", P("b", C(V("a"), V("c")))))
}
