import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub117 {
  /*
      Homework 2, Exercise 3
      2015-15894 Jonghoon Won
      Sep 28, 2017
  */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (expr1, expr2) =>
      {
        (expr1, expr2) match {
          case (NIL, _) => { expr2 }
          case (_, NIL) => { expr1 }
          case (ONE(e1), ONE(e2)) => {
            ZERO(crazy2add(ONE(NIL), crazy2add(e1, e2)))
          }
          case (MONE(e1), MONE(e2)) => {
            ZERO(crazy2add(MONE(NIL), crazy2add(e1, e2)))
          }
          case (ZERO(e1), ZERO(e2)) | (MONE(e1), ONE(e2)) | (ONE(e1), MONE(e2)) => {
            ZERO(crazy2add(e1, e2))
          }
          case (ONE(e1), ZERO(e2)) | (ZERO(e1), ONE(e2)) => {
            ONE(crazy2add(e1, e2))
          }
          case (ZERO(e1), MONE(e2)) | (MONE(e1), ZERO(e2)) => {
            MONE(crazy2add(e1, e2))
          }
        }
    }
  }
}