import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub145 {
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2val: Crazy2 => Int63 = (
    (x) =>
      {
        x match {
          case NIL => { 0 }
          case ZERO(x_0) => { 0 + 2 * crazy2val(x_0) }
          case ONE(x_0) => { 1 + 2 * crazy2val(x_0) }
          case MONE(x_0) => { -(1) + 2 * crazy2val(x_0) }
        }
    }
  )
  
  def int2crazy: Int63 => Crazy2 = (
    (x) =>
      {
        x match {
          case 0 => { NIL }
          case _ => {
            val _2 = {
              val r = x % 2
              val _3 = {
                val q = x / 2
                
                  if (
                    r == 0
                  ) {
                    ZERO(int2crazy(q)) 
                  } else if (
                    r == 1
                  ) {
                    ONE(int2crazy(q)) 
                  } else {
                    MONE(int2crazy(q))
                  }
              }
            }
          }
        }
    }
  )
        
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (x, y) =>
      {
        val _6 = {
          val x_0 = crazy2val(x)
          val _7 = {
            val y_0 = crazy2val(y)
            int2crazy(x_0 + y_0)
          }
        }
    }
  }
}