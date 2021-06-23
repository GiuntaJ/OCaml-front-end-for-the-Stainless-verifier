import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub13 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  sealed case class Error(param0: String) extends Exception {}
  
  def crazy2add(((a, b))) = {
    
      if (
        a == NIL || b == NIL
      ) {
        assert(false, "Error with input is NIL ") 
      } else {
        val _3 = {
          def crazy2add_c(((a, b, carry))) = {
            a match {
              case ZERO(c) => {
                b match {
                  case ZERO(d) => {
                    carry match {
                      case ONE(temp) => { ONE(crazy2add_c(c, d, NIL)) }
                      case MONE(temp) => { MONE(crazy2add_c(c, d, NIL)) }
                      case _ => { ZERO(crazy2add_c(c, d, NIL)) }
                    }
                  }
                  case ONE(d) => {
                    carry match {
                      case ONE(temp) => { ZERO(crazy2add_c(c, d, ONE(NIL))) }
                      case MONE(temp) => { ZERO(crazy2add_c(c, d, NIL)) }
                      case _ => { ONE(crazy2add_c(c, d, NIL)) }
                    }
                  }
                  case MONE(d) => {
                    carry match {
                      case ONE(temp) => { ZERO(crazy2add_c(c, d, NIL)) }
                      case MONE(temp) => { ZERO(crazy2add_c(c, d, MONE(NIL))) }
                      case _ => { MONE(crazy2add_c(c, d, NIL)) }
                    }
                  }
                  case _ => {
                    carry match {
                      case ONE(temp) => { ONE(crazy2add_c(c, NIL, NIL)) }
                      case MONE(temp) => { MONE(crazy2add_c(c, NIL, NIL)) }
                      case _ => { ZERO(crazy2add_c(c, NIL, NIL)) }
                    }
                  }
                }
              }
              case ONE(c) => {
                b match {
                  case ZERO(d) => {
                    carry match {
                      case ONE(temp) => { ZERO(crazy2add_c(c, d, ONE(NIL))) }
                      case MONE(temp) => { ZERO(crazy2add_c(c, d, NIL)) }
                      case _ => { ONE(crazy2add_c(c, d, NIL)) }
                    }
                  }
                  case ONE(d) => {
                    carry match {
                      case ONE(temp) => { ONE(crazy2add_c(c, d, ONE(NIL))) }
                      case MONE(temp) => { ONE(crazy2add_c(c, d, NIL)) }
                      case _ => { ZERO(crazy2add_c(c, d, ONE(NIL))) }
                    }
                  }
                  case MONE(d) => {
                    carry match {
                      case ONE(temp) => { ONE(crazy2add_c(c, d, NIL)) }
                      case MONE(temp) => { MONE(crazy2add_c(c, d, NIL)) }
                      case _ => { ZERO(crazy2add_c(c, d, NIL)) }
                    }
                  }
                  case _ => {
                    carry match {
                      case ONE(temp) => { ZERO(crazy2add_c(c, NIL, ONE(NIL))) }
                      case MONE(temp) => { ZERO(crazy2add_c(c, NIL, NIL)) }
                      case _ => { ONE(crazy2add_c(c, NIL, NIL)) }
                    }
                  }
                }
              }
              case MONE(c) => {
                b match {
                  case ZERO(d) => {
                    carry match {
                      case ONE(temp) => { ZERO(crazy2add_c(c, d, NIL)) }
                      case MONE(temp) => { ZERO(crazy2add_c(c, d, MONE(NIL))) }
                      case _ => { MONE(crazy2add_c(c, d, NIL)) }
                    }
                  }
                  case ONE(d) => {
                    carry match {
                      case ONE(temp) => { ZERO(crazy2add_c(c, d, ONE(NIL))) }
                      case MONE(temp) => { MONE(crazy2add_c(c, d, NIL)) }
                      case _ => { ZERO(crazy2add_c(c, d, ONE(NIL))) }
                    }
                  }
                  case MONE(d) => {
                    carry match {
                      case ONE(temp) => { MONE(crazy2add_c(c, d, NIL)) }
                      case MONE(temp) => { MONE(crazy2add_c(c, d, MONE(NIL))) }
                      case _ => { ZERO(crazy2add_c(c, d, MONE(NIL))) }
                    }
                  }
                  case _ => {
                    carry match {
                      case ONE(temp) => { ZERO(crazy2add_c(c, NIL, NIL)) }
                      case MONE(temp) => { ZERO(crazy2add_c(c, NIL, MONE(NIL)))
                      }
                      case _ => { MONE(crazy2add_c(c, NIL, NIL)) }
                    }
                  }
                }
              }
              case _ => {
                b match {
                  case ZERO(d) => {
                    carry match {
                      case ONE(temp) => { ONE(crazy2add_c(NIL, d, NIL)) }
                      case MONE(temp) => { MONE(crazy2add_c(NIL, d, NIL)) }
                      case _ => { ZERO(crazy2add_c(NIL, d, NIL)) }
                    }
                  }
                  case ONE(d) => {
                    carry match {
                      case ONE(temp) => { ZERO(crazy2add_c(NIL, d, ONE(NIL))) }
                      case MONE(temp) => { ZERO(crazy2add_c(NIL, d, NIL)) }
                      case _ => { ONE(crazy2add_c(NIL, d, ONE(NIL))) }
                    }
                  }
                  case MONE(d) => {
                    carry match {
                      case ONE(temp) => { ZERO(crazy2add_c(NIL, d, NIL)) }
                      case MONE(temp) => { ZERO(crazy2add_c(NIL, d, MONE(NIL)))
                      }
                      case _ => { MONE(crazy2add_c(NIL, d, NIL)) }
                    }
                  }
                  case _ => {
                    carry match {
                      case ONE(temp) => { ONE(NIL) }
                      case MONE(temp) => { MONE(NIL) }
                      case _ => { NIL }
                    }
                  }
                }
              }
            }
          }
          val _4 = {
            def filter(x) = {
              x match {
                case NIL => { x }
                case ZERO(y) => { if (filter(y) == ZERO(NIL)) ZERO(NIL) else x }
                case ONE(y) => { ONE(filter(y)) }
                case MONE(y) => { MONE(filter(y)) }
              }
            }
            filter(crazy2add_c(a, b, NIL))
          }
        }
      }
  }
}