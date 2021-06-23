import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub166 {
  /*
    Department : Electrical Engineering
    Student-Id : 2008-11923
    Name : HyeonIL Choi (최현일)
    Date: 2017-9-13
    Homework-# : 2-3
    Excercise-Name : sum of k-nary number
  */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  sealed abstract class Carry {}
  case object CZERO extends Carry {}
  case object CONE extends Carry {}
  case object CMONE extends Carry {}
  
  def crazy2add(((crazy2_1, crazy2_2))) = {
    val _2 = {
      def crazy2addWithCarry(((crazy2_1, crazy2_2, carry))) = {
        carry match {
          case CZERO => {
            (crazy2_1, crazy2_2) match {
              case (NIL, _) => { crazy2_2 }
              case (_, NIL) => { crazy2_1 }
              case (MONE(crazy2_1_sub), ONE(crazy2_2_sub)) |
              (ONE(crazy2_1_sub), MONE(crazy2_2_sub)) |
              (ZERO(crazy2_1_sub), ZERO(crazy2_2_sub)) => {
                ZERO(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CZERO))
              }
              case (ZERO(crazy2_1_sub), ONE(crazy2_2_sub)) |
              (ONE(crazy2_1_sub), ZERO(crazy2_2_sub)) => {
                ONE(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CZERO))
              }
              case (ZERO(crazy2_1_sub), MONE(crazy2_2_sub)) |
              (MONE(crazy2_1_sub), ZERO(crazy2_2_sub)) => {
                MONE(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CZERO))
              }
              case (ONE(crazy2_1_sub), ONE(crazy2_2_sub)) => {
                ZERO(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CONE))
              }
              case (MONE(crazy2_1_sub), MONE(crazy2_2_sub)) => {
                ZERO(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CMONE))
              }
            }
          }
          case CONE => {
            (crazy2_1, crazy2_2) match {
              case (NIL, _) => { crazy2addWithCarry(ONE(NIL), crazy2_2, CZERO) }
              case (_, NIL) => { crazy2addWithCarry(crazy2_1, ONE(NIL), CZERO) }
              case (MONE(crazy2_1_sub), ONE(crazy2_2_sub)) |
              (ONE(crazy2_1_sub), MONE(crazy2_2_sub)) |
              (ZERO(crazy2_1_sub), ZERO(crazy2_2_sub)) => {
                ONE(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CZERO))
              }
              case (ZERO(crazy2_1_sub), ONE(crazy2_2_sub)) |
              (ONE(crazy2_1_sub), ZERO(crazy2_2_sub)) => {
                ZERO(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CONE))
              }
              case (ZERO(crazy2_1_sub), MONE(crazy2_2_sub)) |
              (MONE(crazy2_1_sub), ZERO(crazy2_2_sub)) => {
                ZERO(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CZERO))
              }
              case (ONE(crazy2_1_sub), ONE(crazy2_2_sub)) => {
                ONE(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CONE))
              }
              case (MONE(crazy2_1_sub), MONE(crazy2_2_sub)) => {
                MONE(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CZERO))
              }
            }
          }
          case CMONE => {
            (crazy2_1, crazy2_2) match {
              case (NIL, _) => { crazy2addWithCarry(MONE(NIL), crazy2_2, CZERO)
              }
              case (_, NIL) => { crazy2addWithCarry(crazy2_1, MONE(NIL), CZERO)
              }
              case (MONE(crazy2_1_sub), ONE(crazy2_2_sub)) |
              (ONE(crazy2_1_sub), MONE(crazy2_2_sub)) |
              (ZERO(crazy2_1_sub), ZERO(crazy2_2_sub)) => {
                MONE(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CZERO))
              }
              case (ZERO(crazy2_1_sub), ONE(crazy2_2_sub)) |
              (ONE(crazy2_1_sub), ZERO(crazy2_2_sub)) => {
                ZERO(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CZERO))
              }
              case (ZERO(crazy2_1_sub), MONE(crazy2_2_sub)) |
              (MONE(crazy2_1_sub), ZERO(crazy2_2_sub)) => {
                ZERO(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CMONE))
              }
              case (ONE(crazy2_1_sub), ONE(crazy2_2_sub)) => {
                ONE(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CZERO))
              }
              case (MONE(crazy2_1_sub), MONE(crazy2_2_sub)) => {
                MONE(crazy2addWithCarry(crazy2_1_sub, crazy2_2_sub, CMONE))
              }
            }
          }
        }
      }
      crazy2addWithCarry(crazy2_1, crazy2_2, CZERO)
    }
  }
  
}
