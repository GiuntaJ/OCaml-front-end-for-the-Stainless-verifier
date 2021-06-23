import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub78 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2addcarry: (Int63, (Crazy2, Crazy2)) => Crazy2 = {
    case (x, y) =>
      {
        x match {
          case 0 => {
            y match {
              case (NIL, NIL) => { NIL }
              case (NIL, y2) => { y2 }
              case (y1, NIL) => { y1 }
              case (ZERO(y1_), ZERO(y2_)) => {
                ZERO(crazy2addcarry(0, (y1_, y2_)))
              }
              case (ZERO(y1_), ONE(y2_)) => { ONE(crazy2addcarry(0, (y1_, y2_)))
              }
              case (ZERO(y1_), MONE(y2_)) => {
                MONE(crazy2addcarry(0, (y1_, y2_)))
              }
              case (ONE(y1_), ZERO(y2_)) => { ONE(crazy2addcarry(0, (y1_, y2_)))
              }
              case (ONE(y1_), ONE(y2_)) => { ZERO(crazy2addcarry(1, (y1_, y2_)))
              }
              case (ONE(y1_), MONE(y2_)) => {
                ZERO(crazy2addcarry(0, (y1_, y2_)))
              }
              case (MONE(y1_), ZERO(y2_)) => {
                MONE(crazy2addcarry(0, (y1_, y2_)))
              }
              case (MONE(y1_), ONE(y2_)) => {
                ZERO(crazy2addcarry(0, (y1_, y2_)))
              }
              case (MONE(y1_), MONE(y2_)) => {
                ZERO(crazy2addcarry(-(1), (y1_, y2_)))
              }
            }
          }
          case 1 => {
            y match {
              case (NIL, y2) => { crazy2addcarry(0, (ONE(NIL), y2)) }
              case (y1, NIL) => { crazy2addcarry(0, (y1, ONE(NIL))) }
              case (ZERO(y1_), ZERO(y2_)) => {
                ONE(crazy2addcarry(0, (y1_, y2_)))
              }
              case (ZERO(y1_), ONE(y2_)) => {
                ZERO(crazy2addcarry(1, (y1_, y2_)))
              }
              case (ZERO(y1_), MONE(y2_)) => {
                ZERO(crazy2addcarry(0, (y1_, y2_)))
              }
              case (ONE(y1_), ZERO(y2_)) => {
                ZERO(crazy2addcarry(1, (y1_, y2_)))
              }
              case (ONE(y1_), ONE(y2_)) => { ONE(crazy2addcarry(1, (y1_, y2_)))
              }
              case (ONE(y1_), MONE(y2_)) => { ONE(crazy2addcarry(0, (y1_, y2_)))
              }
              case (MONE(y1_), ZERO(y2_)) => {
                ZERO(crazy2addcarry(0, (y1_, y2_)))
              }
              case (MONE(y1_), ONE(y2_)) => { ONE(crazy2addcarry(0, (y1_, y2_)))
              }
              case (MONE(y1_), MONE(y2_)) => {
                MONE(crazy2addcarry(0, (y1_, y2_)))
              }
            }
          }
          case -1 => {
            y match {
              case (NIL, y2) => { crazy2addcarry(0, (MONE(NIL), y2)) }
              case (y1, NIL) => { crazy2addcarry(0, (y1, MONE(NIL))) }
              case (ZERO(y1_), ZERO(y2_)) => {
                MONE(crazy2addcarry(0, (y1_, y2_)))
              }
              case (ZERO(y1_), ONE(y2_)) => {
                ZERO(crazy2addcarry(0, (y1_, y2_)))
              }
              case (ZERO(y1_), MONE(y2_)) => {
                ZERO(crazy2addcarry(-(1), (y1_, y2_)))
              }
              case (ONE(y1_), ZERO(y2_)) => {
                ZERO(crazy2addcarry(0, (y1_, y2_)))
              }
              case (ONE(y1_), ONE(y2_)) => { ONE(crazy2addcarry(0, (y1_, y2_)))
              }
              case (ONE(y1_), MONE(y2_)) => {
                MONE(crazy2addcarry(0, (y1_, y2_)))
              }
              case (MONE(y1_), ZERO(y2_)) => {
                ZERO(crazy2addcarry(-(1), (y1_, y2_)))
              }
              case (MONE(y1_), ONE(y2_)) => {
                MONE(crazy2addcarry(0, (y1_, y2_)))
              }
              case (MONE(y1_), MONE(y2_)) => {
                MONE(crazy2addcarry(-(1), (y1_, y2_)))
              }
            }
          }
          case n => { NIL }
        }
    }
  }
    
  val crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (x, y) => { crazy2addcarry(0, (x, y)) }
  }
}