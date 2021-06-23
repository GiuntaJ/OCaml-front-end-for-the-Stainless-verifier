import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub201 {
  /*
  	CSE / 2013-11426 / Im DongYeop
  	Homework 2: Exercise 3
  */
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def add(((c1: Crazy2, c2: Crazy2, up: Int63))): Crazy2 = {
    up match {
      case 1 => {
        c1 match {
          case NIL => {
            c2 match {
              case NIL => { ONE(NIL) }
              case ZERO(cin2) => { ONE(cin2) }
              case ONE(cin2) => { ZERO(add(NIL, cin2, 1)) }
              case MONE(cin2) => { ZERO(cin2) }
            }
          }
          case ZERO(cin1) => {
            c2 match {
              case NIL => { ONE(cin1) }
              case ZERO(cin2) => { ONE(add(cin1, cin2, 0)) }
              case ONE(cin2) => { ZERO(add(cin1, cin2, 1)) }
              case MONE(cin2) => { ZERO(add(cin1, cin2, 0)) }
            }
          }
          case ONE(cin1) => {
            c2 match {
              case NIL => { ZERO(add(cin1, NIL, 1)) }
              case ZERO(cin2) => { ZERO(add(cin1, cin2, 1)) }
              case ONE(cin2) => { ONE(add(cin1, cin2, 1)) }
              case MONE(cin2) => { ONE(add(cin1, cin2, 0)) }
            }
          }
          case MONE(cin1) => {
            c2 match {
              case NIL => { ZERO(add(cin1, NIL, 0)) }
              case ZERO(cin2) => { ZERO(add(cin1, cin2, 0)) }
              case ONE(cin2) => { ONE(add(cin1, cin2, 0)) }
              case MONE(cin2) => { MONE(add(cin1, cin2, 0)) }
            }
          }
        }
      }
      case 0 => {
        c1 match {
          case NIL => {
            c2 match {
              case NIL => { NIL }
              case ZERO(cin2) => { ZERO(cin2) }
              case ONE(cin2) => { ONE(cin2) }
              case MONE(cin2) => { MONE(cin2) }
            }
          }
          case ZERO(cin1) => {
            c2 match {
              case NIL => { ZERO(cin1) }
              case ZERO(cin2) => { ZERO(add(cin1, cin2, 0)) }
              case ONE(cin2) => { ONE(add(cin1, cin2, 0)) }
              case MONE(cin2) => { MONE(add(cin1, cin2, 0)) }
            }
          }
          case ONE(cin1) => {
            c2 match {
              case NIL => { ONE(cin1) }
              case ZERO(cin2) => { ONE(add(cin1, cin2, 0)) }
              case ONE(cin2) => { ZERO(add(cin1, cin2, 1)) }
              case MONE(cin2) => { ZERO(add(cin1, cin2, 0)) }
            }
          }
          case MONE(cin1) => {
            c2 match {
              case NIL => { MONE(cin1) }
              case ZERO(cin2) => { MONE(add(cin1, cin2, 0)) }
              case ONE(cin2) => { ZERO(add(cin1, cin2, 0)) }
              case MONE(cin2) => { ZERO(add(cin1, cin2, -(1))) }
            }
          }
        }
      }
      case _ => {
        c1 match {
          case NIL => {
            c2 match {
              case NIL => { MONE(NIL) }
              case ZERO(cin2) => { MONE(cin2) }
              case ONE(cin2) => { ZERO(cin2) }
              case MONE(cin2) => { ZERO(add(NIL, cin2, -(1))) }
            }
          }
          case ZERO(cin1) => {
            c2 match {
              case NIL => { MONE(cin1) }
              case ZERO(cin2) => { MONE(add(cin1, cin2, 0)) }
              case ONE(cin2) => { ZERO(add(cin1, cin2, 0)) }
              case MONE(cin2) => { ZERO(add(cin1, cin2, -(1))) }
            }
          }
          case ONE(cin1) => {
            c2 match {
              case NIL => { ZERO(cin1) }
              case ZERO(cin2) => { ZERO(add(cin1, cin2, 0)) }
              case ONE(cin2) => { ONE(add(cin1, cin2, 0)) }
              case MONE(cin2) => { MONE(add(cin1, cin2, 0)) }
            }
          }
          case MONE(cin1) => {
            c2 match {
              case NIL => { ZERO(add(cin1, NIL, -(1))) }
              case ZERO(cin2) => { ZERO(add(cin1, cin2, -(1))) }
              case ONE(cin2) => { MONE(add(cin1, cin2, 0)) }
              case MONE(cin2) => { MONE(add(cin1, cin2, -(1))) }
            }
          }
        }
      }
    }
  }
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = { add(c1, c2, 0) }
}
