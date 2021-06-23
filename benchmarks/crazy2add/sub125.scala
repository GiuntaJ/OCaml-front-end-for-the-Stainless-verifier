import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub125 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def c2l(c: Crazy2): List[Int63] = {
    c match {
      case NIL => { Nil() }
      case ZERO(cr) => { 0 :: c2l(cr) }
      case ONE(cr) => { 1 :: c2l(cr) }
      case MONE(cr) => { -(1) :: c2l(cr) }
    }
  }
  
  def l2c(l: List[Int63]): Crazy2 = {
    l match {
      case Nil() => { NIL }
      case Cons(h, t) => {
        
          if (
            h eq 0
          ) {
            ZERO(l2c(t)) 
          } else if (
            h eq 1
          ) {
            ONE(l2c(t)) 
          } else {
            MONE(l2c(t))
          }
      }
    }
  }
  
  def crazy2add(((l, r))) = {
    val _2 = {
      val ll = c2l(l)
      val _3 = {
        val rl = c2l(r)
        val _4 = {
          def ladd(((l, r)), c) = {
            (l, r) match {
              case (Cons(h, t), Nil()) => {
                
                  if (
                    c eq 0
                  ) {
                    l 
                  } else if (
                    c eq h
                  ) {
                    0 :: ladd(t, Nil(), c) 
                  } else {
                    c + h :: ladd(t, Nil(), 0)
                  }
              }
              case (Cons(hl, tl), Cons(hr, tr)) => {
                val _7 = {
                  val v = hl + hr + c
                  val _8 = {
                    val nc = 
                      if (
                        v >= 2
                      ) {
                        1 
                      } else if (
                        v <= -(2)
                      ) {
                        -(1) 
                      } else {
                        0
                      }
                    val _9 = {
                      val s = 
                        if (
                          v > 0 && v % 2 eq 1
                        ) {
                          1 
                        } else if (
                          v % 2 eq 0
                        ) {
                          0 
                        } else {
                          -(1)
                        }
                      s :: ladd(tl, tr, nc)
                    }
                  }
                }
              }
              case (Nil(), _) => { List(c) }
            }
          }
          
            if (
              ll.length < rl.length
            ) {
              l2c(ladd(rl, ll, 0)) 
            } else {
              l2c(ladd(ll, rl, 0))
            }
        }
      }
    }
  }
}