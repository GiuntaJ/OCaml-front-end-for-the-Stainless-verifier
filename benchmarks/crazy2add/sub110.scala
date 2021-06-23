import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub110 {
  /*real code start*/
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2val(c: Crazy2): Int63 = {
    c match {
      case NIL => { 0 }
      case ZERO(x) => { 2 * crazy2val(x) }
      case ONE(x) => { 2 * crazy2val(x) + 1 }
      case MONE(x) => { 2 * crazy2val(x) - 1 }
    }
  }
  
  def inside(c: Crazy2): Crazy2 = {
    c match {
      case NIL => { NIL }
      case ZERO(x) | ONE(x) | MONE(x) => { x }
    }
  }
  
  def c2carry(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    (c1, c2) match {
      case (NIL, _) => { NIL }
      case (_, NIL) => { NIL }
      case (ONE(x), ONE(y)) => { ONE(c2carry(x, y)) }
      case (MONE(x), MONE(y)) => { MONE(c2carry(x, y)) }
      case (_, _) => { ZERO(c2carry(inside(c1), inside(c2))) }
    }
  }
  
  def c2ex(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    (c1, c2) match {
      case (NIL, NIL) => { NIL }
      case (NIL, _) => { c2 }
      case (_, NIL) => { c1 }
      case (ZERO(x), MONE(y)) | (MONE(x), ZERO(y)) => { MONE(c2ex(x, y)) }
      case (ZERO(x), ONE(y)) | (ONE(x), ZERO(y)) => { ONE(c2ex(x, y)) }
      case (_, _) => { ZERO(c2ex(inside(c1), inside(c2))) }
    }
  }
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    val _2 = {
      val carry = c2carry(c1, c2)
      val _3 = {
        val ex = c2ex(c1, c2)
        if (crazy2val(carry) eq 0) ex else crazy2add(ZERO(carry), ex)
      }
    }
  }
  /*end*/
}