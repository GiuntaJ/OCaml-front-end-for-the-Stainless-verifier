import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub138 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  val sep: Crazy2 => (Crazy2, Int63) = (
    (cra) =>
      {
        cra match {
          case NIL => { (NIL, 0) }
          case ZERO(cra1) => { (cra1, 0) }
          case ONE(cra1) => { (cra1, 1) }
          case MONE(cra1) => { (cra1, -(1)) }
        }
    }
  )
  
  
  def add1: Crazy2 => Crazy2 = (
    (cra) =>
      {
        cra match {
          case NIL => { ONE(NIL) }
          case ZERO(cra1) => { ONE(cra1) }
          case ONE(cra1) => { ZERO(add1(cra1)) }
          case MONE(cra1) => { ZERO(cra1) }
        }
    }
  )
  
  def minus1: Crazy2 => Crazy2 = (
    (cra) =>
      {
        cra match {
          case NIL => { MONE(NIL) }
          case ZERO(cra1) => { MONE(cra1) }
          case ONE(cra1) => { ZERO(cra1) }
          case MONE(cra1) => { ZERO(minus1(cra1)) }
        }
    }
  )
  
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (cra1, cra2) =>
      {
        (cra1, cra2) match {
          case (NIL, cra2) => { cra2 }
          case (cra1, NIL) => { cra1 }
          case (cra1, cra2) => {
            (sep(cra1), sep(cra2)) match {
              case ((scra1, n1), (scra2, n2)) => {
                
                  if (
                    n1 + n2 == 2
                  ) {
                    ZERO(crazy2add(scra1, add1(scra2))) 
                  } else if (
                    n1 + n2 == 1
                  ) {
                    ONE(crazy2add(scra1, scra2)) 
                  } else if (
                    n1 + n2 == 0
                  ) {
                    ZERO(crazy2add(scra1, scra2)) 
                  } else if (
                    n1 + n2 == -(1)
                  ) {
                    MONE(crazy2add(scra1, scra2)) 
                  } else {
                    ZERO(crazy2add(scra1, minus1(scra2)))
                  }
              }
            }
          }
        }
    }
  }
}