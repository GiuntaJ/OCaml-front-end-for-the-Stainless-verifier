import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub12 {
  sealed case class Error(param0: String) extends Exception {}
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a1, a2) =>
      {
        val _2 = {
          def sum(c1, c2) = {
            (c1, c2) match {
              case (ZERO(NIL), ZERO(NIL)) => { ZERO(NIL) }
              case (NIL, _) => { assert(false, "Error with Invalid arg") }
              case (_, NIL) => { assert(false, "Error with Invalid arg") }
              case (ZERO(NIL), _) => { c2 }
              case (_, ZERO(NIL)) => { c1 }
              case (MONE(NIL), MONE(NIL)) => { ZERO(MONE(NIL)) }
              case (MONE(NIL), ONE(NIL)) => { ZERO(NIL) }
              case (ONE(NIL), ONE(NIL)) => { ZERO(ONE(NIL)) }
              case (ONE(NIL), MONE(NIL)) => { ZERO(NIL) }
              case (ONE(NIL), ONE(d2)) => { ZERO(crazy2add(ONE(NIL), d2)) }
              case (ONE(NIL), ZERO(d2)) => { ONE(d2) }
              case (ONE(NIL), MONE(d2)) => { ZERO(d2) }
              case (ONE(d1), ONE(NIL)) => { ZERO(crazy2add(d1, ONE(NIL))) }
              case (ZERO(d1), ONE(NIL)) => { ONE(d1) }
              case (MONE(d1), ONE(NIL)) => { ZERO(d1) }
              case (ONE(d1), MONE(NIL)) => { ZERO(d1) }
              case (ZERO(d1), MONE(NIL)) => { ONE(crazy2add(MONE(NIL), d1)) }
              case (MONE(d1), MONE(NIL)) => { ZERO(crazy2add(d1, MONE(NIL))) }
              case (MONE(NIL), ZERO(d2)) => { MONE(d2) }
              case (MONE(NIL), ONE(d2)) => { ZERO(d2) }
              case (MONE(NIL), MONE(d2)) => { ZERO(crazy2add(MONE(NIL), d2)) }
              case (ZERO(d1), ONE(d2)) => { ONE(crazy2add(d1, d2)) }
              case (ONE(d1), ZERO(d2)) => { ONE(crazy2add(d1, d2)) }
              case (MONE(d1), ZERO(d2)) => { MONE(crazy2add(d1, d2)) }
              case (ZERO(d1), ZERO(d2)) => { ZERO(crazy2add(d1, d2)) }
              case (ONE(d1), ONE(d2)) => {
                ZERO(crazy2add(ONE(NIL), crazy2add(d1, d2)))
              }
              case (MONE(d1), MONE(d2)) => {
                ZERO(crazy2add(MONE(NIL), crazy2add(d1, d2)))
              }
              case (ONE(d1), MONE(d2)) => { ZERO(crazy2add(d1, d2)) }
              case (MONE(d1), ONE(d2)) => { ZERO(crazy2add(d1, d2)) }
              case (ZERO(d1), MONE(d2)) => {
                ONE(crazy2add(MONE(NIL), crazy2add(d1, d2)))
              }
            }
          }
          val _3 = {
            def check(s1) = {
              s1 match {
                case NIL => { assert(false, "Error with Invalid arg") }
                case ZERO(NIL) => { NIL }
                case ONE(NIL) => { ONE(NIL) }
                case MONE(NIL) => { MONE(NIL) }
                case ZERO(t1) => {
                  
                    if (
                      check(t1) == NIL
                    ) {
                      NIL 
                    } else if (
                      check(t1) == ONE(NIL)
                    ) {
                      ZERO(ONE(NIL)) 
                    } else if (
                      check(t1) == MONE(NIL)
                    ) {
                      ZERO(MONE(NIL)) 
                    } else {
                      ZERO(check(t1))
                    }
                }
                case ONE(t1) => { ONE(check(t1)) }
                case MONE(t1) => { MONE(check(t1)) }
              }
            }
            if (check(sum(a1, a2)) == NIL) ZERO(NIL) else check(sum(a1, a2))
          }
        }
    }
  }
}