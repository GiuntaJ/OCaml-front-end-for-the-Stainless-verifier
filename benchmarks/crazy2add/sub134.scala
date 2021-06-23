import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub134 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  /* exercise 3*/
  
  
  def crazy2add(((add1, add2))) = {
    val _2 = {
      def toint(x) = {
        x match {
          case NIL => { (0, NIL) }
          case ZERO(t) => { (0, t) }
          case ONE(t) => { (1, t) }
          case MONE(t) => { (-(1), t) }
        }
      }
      val _3 = {
        def calculater(((sadd1, sadd2)), carry) = {
          val _6 = {
            val x = toint(sadd1)
            val _7 = {
              val y = toint(sadd2)
              val _8 = {
                val z = fst(x) + fst(y) + carry
                
                  if (
                    sadd1 eq NIL && sadd2 eq NIL && carry eq 0
                  ) {
                    NIL 
                  } else {
                    z % 2 match {
                      case 1 => { ONE(calculater(snd(x), snd(y), z / 2)) }
                      case 0 => { ZERO(calculater(snd(x), snd(y), z / 2)) }
                      case -1 => { MONE(calculater(snd(x), snd(y), z / 2)) }
                      case _ => { NIL }
                    }
                  }
              }
            }
          }
        }
        calculater(add1, add2, 0)
      }
    }
  }
  
}
