import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda3_sub116 {
  sealed abstract class Lambda {}
  case class V(param0: Var) extends Lambda {}
  case class P(param0: Var,  param1: Lambda) extends Lambda {}
  case class C(param0: Lambda,  param1: Lambda) extends Lambda {}
  
  type Var = String
  
  def lookup: (List[Var], Var) => Boolean = {
    case (board, str) =>
      {
        board match {
          case Cons(hd, tl) => { if (hd == str) true else lookup(tl, str) }
          case Nil() => { false }
        }
    }
  }
  
  /*let rec find_element : var list -> var -> var list
  = fun board str -> match board with
                      | hd::tl -> if hd = str then tl else hd :: (find_element tl str)
                      | [] -> board;;*/
  
                              
  def scoring: (List[Var], List[Var], Lambda) => (List[Var], List[Var]) = {
    case (p, v, lam) =>
      {
        lam match {
          case P(x, lams) => {
            
              if (
                lookup(p, x)
              ) {
                scoring(p, v, lams) 
              } else {
                val _10 = {
                  val new_P = x :: p
                  scoring(new_P, v, lams)
                }
              }
          }
          case C(lam1, lam2) => {
            val _5 = {
              val ((n_P, n_V)) = scoring(p, v, lam1)
              val _6 = {
                val ((new_P, new_V)) = scoring(n_P, n_V, lam2)
                (new_P, new_V)
              }
            }
          }
          case V(x) => {
            val _2 = {
              val new_V = x :: v
              (p, new_V)
            }
          }
        }
    }
  }
  /*                             
  let rec scoringP : var list -> lambda -> var list
  = fun p lam -> match lam with
                    | P (x, lams) -> if (lookup p x) then scoringP p lams
                                     else let new_P = x :: p 
                                          in scoringP new_P lams
                    | C (lam1, lam2) -> let n_P = scoringP p lam1
                                        in let new_P = scoringP n_P lam2
                                           in new_P
                    | V (x) -> p;;
                    
  let rec scoringV : var list -> lambda -> var list
  = fun v lam -> match lam with
                    | P (x, lams) -> scoringV v lams
                    | C (lam1, lam2) -> let n_V = scoringV v lam1
                                        in let new_V = scoringV n_V lam2
                                           in new_V
                    | V (x) -> let new_V = x :: v
                               in new_V;;                  
                              */
  def checking: (List[Var], List[Var]) => Boolean = {
    case (p, v) =>
      {
        v match {
          case Cons(hd, tl) => { if (lookup(p, hd)) checking(p, tl) else false }
          case Nil() => { true }
        }
    }
  }
                
                      
  val check: Lambda => Boolean = (
    (lam) =>
      {
        val _15 = {
          val boardP = Nil()
          val _16 = {
            val boardV = Nil()
            val _17 = {
              val ((resultP, resultV)) = scoring(boardP, boardV, lam)
              checking(resultP, resultV)
            }
          }
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
