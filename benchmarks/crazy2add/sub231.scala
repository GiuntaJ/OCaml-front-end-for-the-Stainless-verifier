import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub231 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2sum(((c1: Crazy2, c2: Crazy2, cin: Crazy2))): Crazy2 = {
    (c1, c2, cin) match {
      case (_, _, NIL) => { NIL }
      case (NIL, NIL, _) => { cin }
      case (NIL, _, ZERO(_)) => { c2 }
      case (NIL, ZERO(c2m), ONE(_)) => { ONE(crazy2sum(c1, c2m, ZERO(NIL))) }
      case (NIL, ONE(c2m), ONE(_)) => { ZERO(crazy2sum(c1, c2m, ONE(NIL))) }
      case (NIL, MONE(c2m), ONE(_)) => { ZERO(crazy2sum(c1, c2m, ZERO(NIL))) }
      case (NIL, ZERO(c2m), MONE(_)) => { MONE(crazy2sum(c1, c2m, ZERO(NIL))) }
      case (NIL, ONE(c2m), MONE(_)) => { ZERO(crazy2sum(c1, c2m, ZERO(NIL))) }
      case (NIL, MONE(c2m), MONE(_)) => { ZERO(crazy2sum(c1, c2m, MONE(NIL))) }
      case (_, NIL, ZERO(_)) => { c1 }
      case (ZERO(c1m), NIL, ONE(_)) => { ONE(crazy2sum(c1m, c2, ZERO(NIL))) }
      case (ONE(c1m), NIL, ONE(_)) => { ZERO(crazy2sum(c1m, c2, ONE(NIL))) }
      case (MONE(c1m), NIL, ONE(_)) => { ZERO(crazy2sum(c1m, c2, ZERO(NIL))) }
      case (ZERO(c1m), NIL, MONE(_)) => { MONE(crazy2sum(c1m, c2, ZERO(NIL))) }
      case (ONE(c1m), NIL, MONE(_)) => { ZERO(crazy2sum(c1m, c2, ZERO(NIL))) }
      case (MONE(c1m), NIL, MONE(_)) => { ZERO(crazy2sum(c1m, c2, MONE(NIL))) }
      case (ZERO(c1m), ZERO(c2m), ZERO(_)) => {
        ZERO(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (ZERO(c1m), ZERO(c2m), ONE(_)) => {
        ONE(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (ZERO(c1m), ZERO(c2m), MONE(_)) => {
        MONE(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (ZERO(c1m), ONE(c2m), ZERO(_)) => {
        ONE(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (ZERO(c1m), ONE(c2m), ONE(_)) => {
        ZERO(crazy2sum(c1m, c2m, ONE(NIL)))
      }
      case (ZERO(c1m), ONE(c2m), MONE(_)) => {
        ZERO(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (ZERO(c1m), MONE(c2m), ZERO(_)) => {
        MONE(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (ZERO(c1m), MONE(c2m), ONE(_)) => {
        ZERO(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (ZERO(c1m), MONE(c2m), MONE(_)) => {
        ZERO(crazy2sum(c1m, c2m, MONE(NIL)))
      }
      case (ONE(c1m), ZERO(c2m), ZERO(_)) => {
        ONE(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (ONE(c1m), ZERO(c2m), ONE(_)) => {
        ZERO(crazy2sum(c1m, c2m, ONE(NIL)))
      }
      case (ONE(c1m), ZERO(c2m), MONE(_)) => {
        ZERO(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (ONE(c1m), ONE(c2m), ZERO(_)) => {
        ZERO(crazy2sum(c1m, c2m, ONE(NIL)))
      }
      case (ONE(c1m), ONE(c2m), ONE(_)) => { ONE(crazy2sum(c1m, c2m, ONE(NIL)))
      }
      case (ONE(c1m), ONE(c2m), MONE(_)) => {
        ONE(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (ONE(c1m), MONE(c2m), ZERO(_)) => {
        ZERO(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (ONE(c1m), MONE(c2m), ONE(_)) => {
        ONE(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (ONE(c1m), MONE(c2m), MONE(_)) => {
        MONE(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (MONE(c1m), ZERO(c2m), ZERO(_)) => {
        MONE(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (MONE(c1m), ZERO(c2m), ONE(_)) => {
        ZERO(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (MONE(c1m), ZERO(c2m), MONE(_)) => {
        ZERO(crazy2sum(c1m, c2m, MONE(NIL)))
      }
      case (MONE(c1m), ONE(c2m), ZERO(_)) => {
        ZERO(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (MONE(c1m), ONE(c2m), ONE(_)) => {
        ONE(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (MONE(c1m), ONE(c2m), MONE(_)) => {
        MONE(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (MONE(c1m), MONE(c2m), ZERO(_)) => {
        ZERO(crazy2sum(c1m, c2m, MONE(NIL)))
      }
      case (MONE(c1m), MONE(c2m), ONE(_)) => {
        MONE(crazy2sum(c1m, c2m, ZERO(NIL)))
      }
      case (MONE(c1m), MONE(c2m), MONE(_)) => {
        MONE(crazy2sum(c1m, c2m, MONE(NIL)))
      }
    }
  }
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    crazy2sum(c1, c2, ZERO(NIL))
  }
}