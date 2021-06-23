import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub64 {
  /* SNU Programming Language Fall 2015
   * Homework 2 
   * Exercise 2: crazy2add
   * Written by Dongho Kang 
   */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (num1, num2) =>
      {
        val _2 = {
          def crazy2inc: Crazy2 => Crazy2 = (
            (num) =>
              {
                num match {
                  case NIL => { ONE(NIL) }
                  case MONE(num_n) => { ZERO(num_n) }
                  case ZERO(num_n) => { ONE(num_n) }
                  case ONE(num_n) => { ZERO(crazy2inc(num_n)) }
                }
            }
          )
          val _3 = {
            def crazy2dec: Crazy2 => Crazy2 = (
              (num) =>
                {
                  num match {
                    case NIL => { MONE(NIL) }
                    case MONE(num_n) => { ZERO(crazy2dec(num_n)) }
                    case ZERO(num_n) => { MONE(num_n) }
                    case ONE(num_n) => { ZERO(num_n) }
                  }
              }
            )
            num2 match {
              case NIL => { num1 }
              case ZERO(num2_n) => {
                num1 match {
                  case NIL => { num2 }
                  case ZERO(num1_n) => { ZERO(crazy2add(num1_n, num2_n)) }
                  case ONE(num1_n) => { ONE(crazy2add(num1_n, num2_n)) }
                  case MONE(num1_n) => { MONE(crazy2add(num1_n, num2_n)) }
                }
              }
              case ONE(num2_n) => {
                num1 match {
                  case NIL => { num2 }
                  case ZERO(num1_n) => { ONE(crazy2add(num1_n, num2_n)) }
                  case ONE(num1_n) => {
                    ZERO(crazy2inc(crazy2add(num1_n, num2_n)))
                  }
                  case MONE(num1_n) => { ZERO(crazy2add(num1_n, num2_n)) }
                }
              }
              case MONE(num2_n) => {
                num1 match {
                  case NIL => { num2 }
                  case ZERO(num1_n) => { MONE(crazy2add(num1_n, num2_n)) }
                  case ONE(num1_n) => { ZERO(crazy2add(num1_n, num2_n)) }
                  case MONE(num1_n) => {
                    ZERO(crazy2dec(crazy2add(num1_n, num2_n)))
                  }
                }
              }
            }
          }
        }
    }
  }
}