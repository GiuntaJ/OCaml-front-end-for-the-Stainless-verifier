import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub33 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {} 
  
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    val _2 = {
      def c2addWithCarry(x, y, c) = {
        (x, y, c) match {
          case (_, _, ZERO(_)) => { c2addWithCarry(x, y, NIL) }
          case (NIL, NIL, n) | (NIL, n, NIL) | (n, NIL, NIL) => { n }
          case (NIL, n, c) | (n, NIL, c) => {
            (n, c) match {
              case (n, (NIL | ZERO(_))) => { n }
              case (NIL, c) => { c }
              case (MONE(k), MONE(_)) => {
                ZERO(c2addWithCarry(NIL, k, MONE(NIL)))
              }
              case (ZERO(k), MONE(_)) => { MONE(k) }
              case (MONE(k), ONE(_)) | (ONE(k), MONE(_)) => { ZERO(k) }
              case (ZERO(k), ONE(_)) => { ONE(k) }
              case (ONE(k), ONE(_)) => { ZERO(c2addWithCarry(NIL, k, ONE(NIL)))
              }
            }
          }
          case (MONE(n1), MONE(n2), MONE(_)) => {
            MONE(c2addWithCarry(n1, n2, MONE(NIL)))
          }
          case (MONE(n1), MONE(n2), NIL) | (MONE(n1), ZERO(n2), MONE(_)) |
          (ZERO(n1), MONE(n2), MONE(_)) => {
            ZERO(c2addWithCarry(n1, n2, MONE(NIL)))
          }
          case (MONE(n1), ZERO(n2), NIL) | (ZERO(n1), MONE(n2), NIL) |
          (ZERO(n1), ZERO(n2), MONE(_)) | (MONE(n1), MONE(n2), ONE(_)) |
          (MONE(n1), ONE(n2), MONE(_)) | (ONE(n1), MONE(n2), MONE(_)) => {
            MONE(c2addWithCarry(n1, n2, ZERO(NIL)))
          }
          case (ZERO(n1), ZERO(n2), NIL) | (MONE(n1), ONE(n2), NIL) |
          (MONE(n1), ZERO(n2), ONE(_)) | (ZERO(n1), MONE(n2), ONE(_)) |
          (ONE(n1), MONE(n2), NIL) | (ONE(n1), ZERO(n2), MONE(_)) |
          (ZERO(n1), ONE(n2), MONE(_)) => {
            ZERO(c2addWithCarry(n1, n2, ZERO(NIL)))
          }
          case (ONE(n1), ZERO(n2), NIL) | (ZERO(n1), ONE(n2), NIL) |
          (ZERO(n1), ZERO(n2), ONE(_)) | (MONE(n1), ONE(n2), ONE(_)) |
          (ONE(n1), ONE(n2), MONE(_)) | (ONE(n1), MONE(n2), ONE(_)) => {
            ONE(c2addWithCarry(n1, n2, ZERO(NIL)))
          }
          case (ONE(n1), ONE(n2), NIL) | (ONE(n1), ZERO(n2), ONE(_)) |
          (ZERO(n1), ONE(n2), ONE(_)) => {
            ZERO(c2addWithCarry(n1, n2, ONE(NIL)))
          }
          case (ONE(n1), ONE(n2), ONE(_)) => {
            ONE(c2addWithCarry(n1, n2, ONE(NIL)))
          }
        }
      }
      c2addWithCarry(c1, c2, NIL)
    }
  }
    
  /* TESTCASE
  let rec string_of_crazy2 c = 
    match c with
    | NIL -> ""
    | ZERO n -> "0" ^ (string_of_crazy2 n)
    | MONE n -> "-" ^ (string_of_crazy2 n)
    | ONE n  -> "+" ^ (string_of_crazy2 n)
  
  let rec int2crazy2_test (i:int) :crazy2 = 
    (let q, r = i / 2, i mod 2 in
      if q = 0 then (match r with -1 -> MONE NIL | 1 -> ONE NIL | _ -> ZERO NIL)
  	else let n = int2crazy2_test q in (match r with -1 -> MONE n | 1 -> ONE n | _ -> ZERO n))
  
  let rec crazy2val_test (c :crazy2) :int = 
  	(match c with
  	  NIL -> 0
  	| ZERO next -> 2 * (crazy2val_test next)
  	| ONE next -> 1 + 2 * (crazy2val_test next)
  	| MONE next -> -1 + 2 * (crazy2val_test next))
  
  let testA = int2crazy2_test 1095
  let testB = int2crazy2_test (-1095)
  let testC = crazy2add (testA, testB)
  
  let _ = print_endline((string_of_crazy2 testA) ^ " + " ^ (string_of_crazy2 testB) ^ " = " ^ (string_of_crazy2 testC))
  let _ = print_endline(string_of_int (crazy2val_test testC))
  */
}