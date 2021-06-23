import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub69 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  val c2i: Crazy2 => Int63 = (
    (c) =>
      {
        c match {
          case ONE(c1) => { 1 }
          case MONE(c1) => { -(1) }
          case _ => { 0 }
        }
    }
  )
  
  val nextdigit: Crazy2 => Crazy2 = (
    (c) =>
      {
        c match {
          case ZERO(c1) | ONE(c1) | MONE(c1) => { c1 }
          case NIL => { NIL }
        }
    }
  )
  
  def onebitadd: (Crazy2, Crazy2, Crazy2) => Int63 = {
    case (a, b, c) => { (c2i(a) + c2i(b) + c2i(c)) % 2 }
  }
  
  def onebitcarry: (Crazy2, Crazy2, Crazy2) => Crazy2 = {
    case (a, b, c) =>
      {
        (c2i(a) + c2i(b) + c2i(c)) / 2 match {
          case 1 => { ONE(NIL) }
          case -1 => { MONE(NIL) }
          case _ => { NIL }
        }
    }
  }
  
  def fulladd: (Crazy2, Crazy2, Crazy2) => Crazy2 = {
    case (a, b, c) =>
      {
        (a, b) match {
          case (NIL, NIL) => { c }
          case _ => {
            onebitadd(a, b, c) match {
              case 1 => {
                ONE(fulladd(nextdigit(a), nextdigit(b), onebitcarry(a, b, c)))
              }
              case -1 => {
                MONE(fulladd(nextdigit(a), nextdigit(b), onebitcarry(a, b, c)))
              }
              case _ => {
                ZERO(fulladd(nextdigit(a), nextdigit(b), onebitcarry(a, b, c)))
              }
            }
          }
        }
    }
  }
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (a, b) => { fulladd(a, b, NIL) }
  }
}