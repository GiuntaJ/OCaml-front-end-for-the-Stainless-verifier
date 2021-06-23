import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub34 {
  /* 2011-10915 / 생명과학부 / 신지민 / Homework 2-2 */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) =>
      {
        c1 match {
          case NIL => { c2 }
          case ZERO(tl1) => {
            c2 match {
              case NIL => { c1 }
              case ZERO(tl2) => { ZERO(crazy2add(tl1, tl2)) }
              case ONE(tl2) => { ONE(crazy2add(tl1, tl2)) }
              case MONE(tl2) => { MONE(crazy2add(tl1, tl2)) }
            }
          }
          case ONE(tl1) => {
            c2 match {
              case NIL => { c1 }
              case ZERO(tl2) => { ONE(crazy2add(tl1, tl2)) }
              case ONE(tl2) => {
                val _5 = {
                  val tl = crazy2add(ONE(NIL), tl1)
                  ZERO(crazy2add(tl, tl2))
                }
              }
              case MONE(tl2) => { ZERO(crazy2add(tl1, tl2)) }
            }
          }
          case MONE(tl1) => {
            c2 match {
              case NIL => { c1 }
              case ZERO(tl2) => { MONE(crazy2add(tl1, tl2)) }
              case ONE(tl2) => { ZERO(crazy2add(tl1, tl2)) }
              case MONE(tl2) => {
                val _2 = {
                  val tl = crazy2add(MONE(NIL), tl1)
                  ZERO(crazy2add(tl, tl2))
                }
              }
            }
          }
        }
    }
  }
  /*
  let rec crazy2val : crazy2 -> int = fun cr ->
  	match cr with
  	|NIL -> 0
  	|ZERO cr2 -> 0+ 2*crazy2val(cr2)
  	|ONE cr2 -> 1+ 2*crazy2val(cr2)
  	|MONE cr2 -> -1 + 2*crazy2val(cr2)
  
  
  let a = ONE(ZERO(ONE NIL))
  let b = ONE(MONE NIL)
  let c = ONE(MONE(ZERO(MONE NIL)))
  let d = NIL
  
  let a1 = crazy2val a
  let b1 = crazy2val b
  let c1 = crazy2val c
  let d1 = crazy2val d
  
  let e = crazy2val(crazy2add(a,b))
  let e1 = a1+b1
  let _= print_endline(string_of_int e)
  let _= print_endline(string_of_int e1)
  
  let _= print_endline("na")
  
  let e = crazy2val(crazy2add(c,d))
  let e1 = c1+d1
  let _= print_endline(string_of_int e)
  let _= print_endline(string_of_int e1)
  
  */
  
}
