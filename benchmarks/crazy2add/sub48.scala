import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub48 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  
  
  def subCrazy2add(c1: Crazy2, c2: Crazy2, carry: Crazy2): Crazy2 = {
    c1 match {
      case NIL => {
        c2 match {
          case NIL => { carry }
          case ONE(x) => {
            carry match {
              case NIL => { ONE(subCrazy2add(NIL, x, NIL)) }
              case ONE(_) => { ZERO(subCrazy2add(NIL, x, ONE(NIL))) }
              case MONE(_) => { ZERO(subCrazy2add(NIL, x, NIL)) }
              case ZERO(_) => { ONE(subCrazy2add(NIL, x, NIL)) }
            }
          }
          case MONE(x) => {
            carry match {
              case NIL => { MONE(subCrazy2add(NIL, x, NIL)) }
              case MONE(_) => { ZERO(subCrazy2add(NIL, x, MONE(NIL))) }
              case ZERO(_) => { MONE(subCrazy2add(NIL, x, NIL)) }
              case ONE(_) => { ZERO(subCrazy2add(NIL, x, NIL)) }
            }
          }
          case ZERO(x) => {
            carry match {
              case NIL => { ZERO(subCrazy2add(NIL, x, NIL)) }
              case MONE(_) => { MONE(subCrazy2add(NIL, x, NIL)) }
              case ONE(_) => { ONE(subCrazy2add(NIL, x, NIL)) }
              case ZERO(_) => { ZERO(subCrazy2add(NIL, x, NIL)) }
            }
          }
        }
      }
      case ONE(y) => {
        c2 match {
          case NIL => {
            carry match {
              case NIL => { ONE(subCrazy2add(y, NIL, NIL)) }
              case ONE(_) => { ZERO(subCrazy2add(y, NIL, ONE(NIL))) }
              case MONE(_) => { ZERO(subCrazy2add(y, NIL, NIL)) }
              case ZERO(_) => { ONE(subCrazy2add(y, NIL, NIL)) }
            }
          }
          case ONE(x) => {
            carry match {
              case NIL => { ZERO(subCrazy2add(y, x, ONE(NIL))) }
              case ONE(_) => { ONE(subCrazy2add(y, x, ONE(NIL))) }
              case ZERO(_) => { ZERO(subCrazy2add(y, x, ONE(NIL))) }
              case MONE(_) => { ONE(subCrazy2add(y, x, NIL)) }
            }
          }
          case ZERO(x) => {
            carry match {
              case NIL => { ONE(subCrazy2add(y, x, NIL)) }
              case ONE(_) => { ZERO(subCrazy2add(y, x, ONE(NIL))) }
              case ZERO(_) => { ONE(subCrazy2add(y, x, NIL)) }
              case MONE(_) => { ZERO(subCrazy2add(y, x, NIL)) }
            }
          }
          case MONE(x) => {
            carry match {
              case NIL => { ZERO(subCrazy2add(y, x, NIL)) }
              case ONE(_) => { ONE(subCrazy2add(y, x, NIL)) }
              case ZERO(_) => { ZERO(subCrazy2add(y, x, NIL)) }
              case MONE(_) => { MONE(subCrazy2add(y, x, NIL)) }
            }
          }
        }
      }
      case ZERO(y) => {
        c2 match {
          case NIL => {
            carry match {
              case NIL => { ZERO(subCrazy2add(y, NIL, NIL)) }
              case ONE(_) => { ONE(subCrazy2add(y, NIL, NIL)) }
              case ZERO(_) => { ZERO(subCrazy2add(y, NIL, NIL)) }
              case MONE(_) => { MONE(subCrazy2add(y, NIL, NIL)) }
            }
          }
          case ONE(x) => {
            carry match {
              case NIL => { ONE(subCrazy2add(y, x, NIL)) }
              case ONE(_) => { ZERO(subCrazy2add(y, x, ONE(NIL))) }
              case ZERO(_) => { ONE(subCrazy2add(y, x, NIL)) }
              case MONE(_) => { ZERO(subCrazy2add(y, x, NIL)) }
            }
          }
          case ZERO(x) => {
            carry match {
              case NIL => { ZERO(subCrazy2add(y, x, NIL)) }
              case ONE(_) => { ONE(subCrazy2add(y, x, NIL)) }
              case ZERO(_) => { ZERO(subCrazy2add(y, x, NIL)) }
              case MONE(_) => { MONE(subCrazy2add(y, x, NIL)) }
            }
          }
          case MONE(x) => {
            carry match {
              case NIL => { MONE(subCrazy2add(y, x, NIL)) }
              case ONE(_) => { ZERO(subCrazy2add(y, x, NIL)) }
              case ZERO(_) => { MONE(subCrazy2add(y, x, NIL)) }
              case MONE(_) => { ZERO(subCrazy2add(y, x, MONE(NIL))) }
            }
          }
        }
      }
      case MONE(y) => {
        c2 match {
          case NIL => {
            carry match {
              case NIL => { MONE(subCrazy2add(y, NIL, NIL)) }
              case ONE(_) => { ZERO(subCrazy2add(y, NIL, NIL)) }
              case ZERO(_) => { MONE(subCrazy2add(y, NIL, NIL)) }
              case MONE(_) => { ZERO(subCrazy2add(y, NIL, MONE(NIL))) }
            }
          }
          case ONE(x) => {
            carry match {
              case NIL => { ZERO(subCrazy2add(y, x, NIL)) }
              case ONE(_) => { ONE(subCrazy2add(y, x, NIL)) }
              case ZERO(_) => { ZERO(subCrazy2add(y, x, NIL)) }
              case MONE(_) => { MONE(subCrazy2add(y, x, NIL)) }
            }
          }
          case ZERO(x) => {
            carry match {
              case NIL => { MONE(subCrazy2add(y, x, NIL)) }
              case ONE(_) => { ZERO(subCrazy2add(y, x, NIL)) }
              case ZERO(_) => { MONE(subCrazy2add(y, x, NIL)) }
              case MONE(_) => { ZERO(subCrazy2add(y, x, MONE(NIL))) }
            }
          }
          case MONE(x) => {
            carry match {
              case NIL => { ZERO(subCrazy2add(y, x, MONE(NIL))) }
              case ONE(_) => { MONE(subCrazy2add(y, x, NIL)) }
              case ZERO(_) => { ZERO(subCrazy2add(y, x, MONE(NIL))) }
              case MONE(_) => { MONE(subCrazy2add(y, x, MONE(NIL))) }
            }
          }
        }
      }
    }
  } 
  
  
  def crazy2add(((c1, c2))): Crazy2 = { subCrazy2add(c1, c2, NIL) }
  	
  	
}