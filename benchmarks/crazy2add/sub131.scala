import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub131 {
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  
  sealed abstract class Plusminus {}
  case object NONE extends Plusminus {}
  case object PLUS extends Plusminus {}
  case object MINUS extends Plusminus {}
  
  def add1(c: Crazy2): Crazy2 = {
    c match {
      case NIL => { ONE(NIL) }
      case ZERO(c_) => { ONE(c_) }
      case ONE(c_) => { ZERO(add1(c_)) }
      case MONE(c_) => { ZERO(c_) }
    }
  }
  def minus1(c) = {
    c match {
      case NIL => { MONE(NIL) }
      case ZERO(c_) => { MONE(c_) }
      case ONE(c_) => { ZERO(c_) }
      case MONE(c_) => { ZERO(minus1(c_)) }
    }
  }
  
  val crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c0, c1) =>
      {
        val _2 = {
          def crazy2add2(c0, c1, pm) = {
            pm match {
              case NONE => {
                (c0, c1) match {
                  case (NIL, _) => { c1 }
                  case (_, NIL) => { c0 }
                  case (ZERO(c0_), ZERO(c1_)) | (ONE(c0_), MONE(c1_)) |
                  (MONE(c0_), ONE(c1_)) => {
                    ZERO(crazy2add2(c0_, c1_, NONE))
                  }
                  case (ZERO(c0_), ONE(c1_)) | (ONE(c0_), ZERO(c1_)) => {
                    ONE(crazy2add2(c0_, c1_, NONE))
                  }
                  case (ZERO(c0_), MONE(c1_)) | (MONE(c0_), ZERO(c1_)) => {
                    MONE(crazy2add2(c0_, c1_, NONE))
                  }
                  case (ONE(c0_), ONE(c1_)) => {
                    ZERO(crazy2add2(c0_, c1_, PLUS))
                  }
                  case (MONE(c0_), MONE(c1_)) => {
                    ZERO(crazy2add2(c0_, c1_, MINUS))
                  }
                }
              }
              case PLUS => {
                (c0, c1) match {
                  case (NIL, _) => { add1(c1) }
                  case (_, NIL) => { add1(c0) }
                  case (ZERO(c0_), ZERO(c1_)) | (ONE(c0_), MONE(c1_)) |
                  (MONE(c0_), ONE(c1_)) => {
                    ONE(crazy2add2(c0_, c1_, NONE))
                  }
                  case (ZERO(c0_), ONE(c1_)) | (ONE(c0_), ZERO(c1_)) => {
                    ZERO(crazy2add2(c0_, c1_, PLUS))
                  }
                  case (ZERO(c0_), MONE(c1_)) | (MONE(c0_), ZERO(c1_)) => {
                    ZERO(crazy2add2(c0_, c1_, NONE))
                  }
                  case (ONE(c0_), ONE(c1_)) => { ONE(crazy2add2(c0_, c1_, PLUS))
                  }
                  case (MONE(c0_), MONE(c1_)) => {
                    MONE(crazy2add2(c0_, c1_, NONE))
                  }
                }
              }
              case MINUS => {
                (c0, c1) match {
                  case (NIL, _) => { minus1(c1) }
                  case (_, NIL) => { minus1(c0) }
                  case (ZERO(c0_), ZERO(c1_)) | (ONE(c0_), MONE(c1_)) |
                  (MONE(c0_), ONE(c1_)) => {
                    MONE(crazy2add2(c0_, c1_, NONE))
                  }
                  case (ZERO(c0_), ONE(c1_)) | (ONE(c0_), ZERO(c1_)) => {
                    ZERO(crazy2add2(c0_, c1_, NONE))
                  }
                  case (ZERO(c0_), MONE(c1_)) | (MONE(c0_), ZERO(c1_)) => {
                    ZERO(crazy2add2(c0_, c1_, MINUS))
                  }
                  case (ONE(c0_), ONE(c1_)) => { ONE(crazy2add2(c0_, c1_, NONE))
                  }
                  case (MONE(c0_), MONE(c1_)) => {
                    MONE(crazy2add2(c0_, c1_, MINUS))
                  }
                }
              }
            }
          }
          crazy2add2(c0, c1, NONE)
        }
    }
  }
}