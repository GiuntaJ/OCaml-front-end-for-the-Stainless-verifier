import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub1 {
  /*********************/
  /*   Problem 2        */
  /*********************/
  
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def check: Lambda => Boolean = (
    (lam) =>
      {
        val _4 = {
          def checker(lamb, set) = {
            val _7 = {
              def probe(x, set) = {
                set match {
                  case Nil() => { false }
                  case Cons(hd, tl) => { if (x == hd) true else probe(x, tl) }
                }
              }
              lamb match {
                case V(v) => { probe(v, set) }
                case P(v, lamb) => {
                  val _10 = {
                    def extend(x, set) = { x :: set }
                    val _11 = {
                      val set2 = extend(v, set)
                      checker(lamb, set2)
                    }
                  }
                }
                case C(l1, l2) => { checker(l1, set) && checker(l2, set) }
              }
            }
          }
          checker(lam, Nil())
        }
    }
  )
  /*
  let t1 = P ("a", V "a")
  let t2 = P ("a", P ("a", V "a"))
  let t3 = P ("a", P ("b", C (V "a", V "b")))
  let t4 = P ("a", C (V "a", P ("b", V "a")))
  
  let f1 = P ("a", V "b")
  let f2 = P ("a", C (V "a", P ("b", V "c")))
  let f3 = P ("a", P ("b", C (V "a", V "c")))
  
  let test = LET("y", CONST 3, 
    LETREC("f", "x", 
      IF (ISZERO (VAR "x"), CONST 1, MUL( CALL (VAR "f", SUB (VAR "x", CONST 1)), VAR "x")), 
        MUL(CALL(VAR "f", CONST 3), VAR "y")))
  */
}