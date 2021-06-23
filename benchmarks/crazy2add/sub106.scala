import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub106 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def addOne(cra: Crazy2, carry: Int63): Crazy2 = {
    val _2 = {
      def add(cra, carry) = {
        cra match {
          case NIL => {
            
              if (
                carry eq 0
              ) {
                NIL 
              } else if (
                carry eq 1
              ) {
                ONE(NIL) 
              } else {
                MONE(NIL)
              }
          }
          case ONE(a) => {
            
              if (
                carry eq 0
              ) {
                ONE(a) 
              } else if (
                carry eq 1
              ) {
                ZERO(add(a, 1)) 
              } else {
                ZERO(a)
              }
          }
          case ZERO(a) => {
            
              if (
                carry eq 0
              ) {
                ZERO(a) 
              } else if (
                carry eq 1
              ) {
                ONE(a) 
              } else {
                MONE(a)
              }
          }
          case MONE(a) => {
            
              if (
                carry eq 0
              ) {
                MONE(a) 
              } else if (
                carry eq 1
              ) {
                ZERO(a) 
              } else {
                ZERO(add(a, 2))
              }
          }
        }
      }
      add(cra, carry)
    }
  }
  
  def crazy2add(((cra1, cra2))) = {
    val _5 = {
      def cradd(c1, c2, carry) = {
        (c1, c2) match {
          case (NIL, a) => { addOne(c2, carry) }
          case (a, NIL) => { addOne(c1, carry) }
          case (ONE(a), ONE(b)) => {
            
              if (
                carry eq 0
              ) {
                ZERO(cradd(a, b, 1)) 
              } else if (
                carry eq 1
              ) {
                ONE(cradd(a, b, 1)) 
              } else {
                ONE(cradd(a, b, 0))
              }
          }
          case (ONE(a), ZERO(b)) => {
            
              if (
                carry eq 1
              ) {
                ZERO(cradd(a, b, 1)) 
              } else if (
                carry eq 0
              ) {
                ONE(cradd(a, b, 0)) 
              } else {
                ZERO(cradd(a, b, 0))
              }
          }
          case (ZERO(a), ONE(b)) => {
            
              if (
                carry eq 1
              ) {
                ZERO(cradd(a, b, 1)) 
              } else if (
                carry eq 0
              ) {
                ONE(cradd(a, b, 0)) 
              } else {
                ZERO(cradd(a, b, 0))
              }
          }
          case (ONE(a), MONE(b)) => {
            
              if (
                carry eq 1
              ) {
                ONE(cradd(a, b, 0)) 
              } else if (
                carry eq 0
              ) {
                ZERO(cradd(a, b, 0)) 
              } else {
                MONE(cradd(a, b, 0))
              }
          }
          case (MONE(a), ONE(b)) => {
            
              if (
                carry eq 1
              ) {
                ONE(cradd(a, b, 0)) 
              } else if (
                carry eq 0
              ) {
                ZERO(cradd(a, b, 0)) 
              } else {
                MONE(cradd(a, b, 0))
              }
          }
          case (ZERO(a), ZERO(b)) => {
            
              if (
                carry eq 1
              ) {
                ONE(cradd(a, b, 0)) 
              } else if (
                carry eq 0
              ) {
                ZERO(cradd(a, b, 0)) 
              } else {
                MONE(cradd(a, b, 0))
              }
          }
          case (ZERO(a), MONE(b)) => {
            
              if (
                carry eq 1
              ) {
                ZERO(cradd(a, b, 0)) 
              } else if (
                carry eq 0
              ) {
                MONE(cradd(a, b, 0)) 
              } else {
                ZERO(cradd(a, b, 2))
              }
          }
          case (MONE(a), ZERO(b)) => {
            
              if (
                carry eq 1
              ) {
                ZERO(cradd(a, b, 0)) 
              } else if (
                carry eq 0
              ) {
                MONE(cradd(a, b, 0)) 
              } else {
                ZERO(cradd(a, b, 2))
              }
          }
          case (MONE(a), MONE(b)) => {
            
              if (
                carry eq 1
              ) {
                MONE(cradd(a, b, 0)) 
              } else if (
                carry eq 0
              ) {
                ZERO(cradd(a, b, 2)) 
              } else {
                MONE(cradd(a, b, 2))
              }
          }
        }
      }
      cradd(cra1, cra2, 0)
    }
  }
}
