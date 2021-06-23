import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub139 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (x, y) =>
      {
        val _2 = {
          def value_first(x) = {
            x match {
              case NIL => { 0 }
              case ZERO(_) => { 0 }
              case ONE(_) => { 1 }
              case MONE(_) => { -(1) }
            }
          }
          val _3 = {
            def tail(x) = {
              x match {
                case NIL => { NIL }
                case ZERO(x_0) => { x_0 }
                case ONE(x_0) => { x_0 }
                case MONE(x_0) => { x_0 }
              }
            }
            val _4 = {
              def rev(x, r) = {
                x match {
                  case NIL => { r }
                  case ZERO(x_0) => { rev(x_0, ZERO(r)) }
                  case ONE(x_0) => { rev(x_0, ONE(r)) }
                  case MONE(x_0) => { rev(x_0, MONE(r)) }
                }
              }
              val _5 = {
                def crazy2add_temp(x, y, c, r) = {
                  
                    if (
                      ((x, y, c)) == ((NIL, NIL, 0))
                    ) {
                      r 
                    } else {
                      value_first(x) + value_first(y) + c match {
                        case -3 => {
                          crazy2add_temp(tail(x), tail(y), -(1), MONE(r))
                        }
                        case -2 => {
                          crazy2add_temp(tail(x), tail(y), -(1), ZERO(r))
                        }
                        case -1 => {
                          crazy2add_temp(tail(x), tail(y), 0, MONE(r))
                        }
                        case 0 => { crazy2add_temp(tail(x), tail(y), 0, ZERO(r))
                        }
                        case 1 => { crazy2add_temp(tail(x), tail(y), 0, ONE(r))
                        }
                        case 2 => { crazy2add_temp(tail(x), tail(y), 1, ZERO(r))
                        }
                        case 3 => { crazy2add_temp(tail(x), tail(y), 1, ONE(r))
                        }
                        case _ => { failwith("ERROR!") }
                      }
                    }
                }
                rev(crazy2add_temp(x, y, 0, NIL), NIL)
              }
            }
          }
        }
    }
  }
}