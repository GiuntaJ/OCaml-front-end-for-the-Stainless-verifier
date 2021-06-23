import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub60 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  /* ex2.ml */
  
  /* exception 이름은 대문자로 시작!! */
  sealed case class Error(param0: String) extends Exception {}
  
  def crazy2add(((_c1, _c2))) = {
    val _2 = {
      def carry = (
        x =>
          x match {
            case (NIL, NIL, cr) => {
              cr match {
                case ZERO(NIL) => { NIL }
                case _ => { cr }
              }
            }
            case (c1, NIL, cr) => { carry(c1, ZERO(NIL), cr) }
            case (NIL, c2, cr) => { carry(ZERO(NIL), c2, cr) }
            case (ZERO(c1_0), ZERO(c2_0), ZERO(NIL)) |
            (ZERO(c1_0), ONE(c2_0), MONE(NIL)) |
            (ZERO(c1_0), MONE(c2_0), ONE(NIL)) |
            (ONE(c1_0), ZERO(c2_0), MONE(NIL)) |
            (ONE(c1_0), MONE(c2_0), ZERO(NIL)) |
            (MONE(c1_0), ZERO(c2_0), ONE(NIL)) |
            (MONE(c1_0), ONE(c2_0), ZERO(NIL)) => {
              ZERO(carry(c1_0, c2_0, ZERO(NIL)))
            }
            case (ZERO(c1_0), ZERO(c2_0), ONE(NIL)) |
            (ZERO(c1_0), ONE(c2_0), ZERO(NIL)) |
            (ONE(c1_0), ZERO(c2_0), ZERO(NIL)) |
            (ONE(c1_0), ONE(c2_0), MONE(NIL)) |
            (ONE(c1_0), MONE(c2_0), ONE(NIL)) |
            (MONE(c1_0), ONE(c2_0), ONE(NIL)) => {
              ONE(carry(c1_0, c2_0, ZERO(NIL)))
            }
            case (ZERO(c1_0), ZERO(c2_0), MONE(NIL)) |
            (ONE(c1_0), MONE(c2_0), MONE(NIL)) |
            (MONE(c1_0), ZERO(c2_0), ZERO(NIL)) => {
              MONE(carry(c1_0, c2_0, ZERO(NIL)))
            }
            case (ZERO(c1_0), ONE(c2_0), ONE(NIL)) |
            (ONE(c1_0), ZERO(c2_0), ONE(NIL)) |
            (ONE(c1_0), ONE(c2_0), ZERO(NIL)) => {
              ZERO(carry(c1_0, c2_0, ONE(NIL)))
            }
            case (ZERO(c1_0), MONE(c2_0), ZERO(NIL)) |
            (MONE(c1_0), ONE(c2_0), MONE(NIL)) |
            (MONE(c1_0), MONE(c2_0), ONE(NIL)) => {
              MONE(carry(c1_0, c2_0, ZERO(NIL)))
            }
            case (ZERO(c1_0), MONE(c2_0), MONE(NIL)) |
            (MONE(c1_0), ZERO(c2_0), MONE(NIL)) |
            (MONE(c1_0), MONE(c2_0), ZERO(NIL)) => {
              ZERO(carry(c1_0, c2_0, MONE(NIL)))
            }
            case (ONE(c1_0), ONE(c2_0), ONE(NIL)) => {
              ONE(carry(c1_0, c2_0, ONE(NIL)))
            }
            case (MONE(c1_0), MONE(c2_0), MONE(NIL)) => {
              MONE(carry(c1_0, c2_0, MONE(NIL)))
            }
            case _ => { assert(false, "Error with carry error") }
          }
      )
      carry(_c1, _c2, ZERO(NIL))
    }
  }
              
  
  /* WITH INT
  let rec crazy2add (_c1,_c2) =
      
      let rec carry = function
          (NIL,NIL,cr) -> (match cr with -1 -> MONE NIL | 0 -> ZERO NIL | 1 -> ONE NIL | _ -> raise error)
          | (c1,NIL,cr) -> carry (c1,ZERO(NIL),cr)
          | (NIL,c2,cr) -> carry (ZERO(NIL),c2,cr)
          | (c1,c2,cr) -> 
              (match value_c c1 with (i1,p1) ->
                  (match value_c c2 with (i2,p2) ->
                      (match i1 + i2 + cr with
                          -3 -> MONE(carry(p1,p2,-1))
                          | -2 -> ZERO(carry(p1,p2,-1))
                          | -1 -> MONE(carry(p1,p2,0))
                          | 0 -> ZERO(carry(p1,p2,0))
                          | 1 -> ONE(carry(p1,p2,0))
                          | 2 -> ZERO(carry(p1,p2,1))
                          | 3 -> ONE(carry(p1,p2,1))
                      )))
      and value_c = function
          ZERO(c') -> (0,c')
          | ONE(c') -> (1,c')
          | MONE(c') -> (-1,c')
          | NIL -> raise error
  
      in carry (_c1,_c2,0)
  */
}
