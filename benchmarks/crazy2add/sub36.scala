import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub36 {
  sealed case class TODO() extends Exception {}
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((a: Crazy2, b: Crazy2))): Crazy2 = {
    val _2 = {
      def integerize(c: Crazy2): List[Int63] = {
        c match {
          case NIL => { Nil() }
          case ZERO(c2) => { 0 :: integerize(c2) }
          case ONE(c2) => { 1 :: integerize(c2) }
          case MONE(c2) => { -(1) :: integerize(c2) }
        }
      }
      val _3 = {
        def add(li1, li2) = {
          li1 match {
            case Nil() => { li2 }
            case Cons(hd1, tl1) => {
              li2 match {
                case Nil() => { li1 }
                case Cons(hd2, tl2) => { hd1 + hd2 :: add(tl1, tl2) }
              }
            }
          }
        }
        val _4 = {
          def carry(li) = {
            li match {
              case Nil() => { Nil() }
              case Cons(fst, Nil()) => {
                
                  if (
                    fst < -(1)
                  ) {
                    List(fst + 2, -(1)) 
                  } else if (
                    fst > 1
                  ) {
                    List(fst - 2, 1) 
                  } else {
                    List(fst)
                  }
              }
              case Cons(fst, Cons(snd, tl)) => {
                
                  if (
                    fst < -(1)
                  ) {
                    fst + 2 :: carry(snd - 1 :: tl) 
                  } else if (
                    fst > 1
                  ) {
                    fst - 2 :: carry(snd + 1 :: tl) 
                  } else {
                    fst :: carry(snd :: tl)
                  }
              }
            }
          }
          val _5 = {
            def crazify(li: List[Int63]): Crazy2 = {
              li match {
                case Nil() => { NIL }
                case Cons(hd, tl) => {
                  hd match {
                    case 0 => { ZERO(crazify(tl)) }
                    case 1 => { ONE(crazify(tl)) }
                    case -1 => { MONE(crazify(tl)) }
                    case _ => { NIL }
                  }
                }
              }
            }
            crazify(carry(add(integerize(a), integerize(b))))
          }
        }
      }
    }
  }
  	
  
  
  
  
  
}
