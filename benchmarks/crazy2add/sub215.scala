import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub215 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add(((c1: Crazy2, c2: Crazy2))): Crazy2 = {
    val _2 = {
      val zero = 0
      val _3 = {
        val one = 1
        val _4 = {
          def carryadder(((c1: Crazy2, c2: Crazy2, carry: Int63))): Crazy2 = {
            c1 match {
              case NIL => {
                
                  if (
                    carry eq one
                  ) {
                    carryadder(ONE(NIL), c2, 0) 
                  } else if (
                    carry eq zero
                  ) {
                    c2 
                  } else {
                    carryadder(MONE(NIL), c2, 0)
                  }
              }
              case ONE(d1) => {
                c2 match {
                  case ONE(d2) => {
                    
                      if (
                        carry eq one
                      ) {
                        ONE(carryadder(d1, d2, 1)) 
                      } else if (
                        carry eq zero
                      ) {
                        ZERO(carryadder(d1, d2, 1)) 
                      } else {
                        ONE(carryadder(d1, d2, 0))
                      }
                  }
                  case ZERO(d2) => {
                    
                      if (
                        carry eq one
                      ) {
                        ZERO(carryadder(d1, d2, 1)) 
                      } else if (
                        carry eq zero
                      ) {
                        ONE(carryadder(d1, d2, 0)) 
                      } else {
                        ZERO(carryadder(d1, d2, 0))
                      }
                  }
                  case MONE(d2) => {
                    
                      if (
                        carry eq one
                      ) {
                        ONE(carryadder(d1, d2, 0)) 
                      } else if (
                        carry eq zero
                      ) {
                        ZERO(carryadder(d1, d2, 0)) 
                      } else {
                        MONE(carryadder(d1, d2, 0))
                      }
                  }
                  case NIL => {
                    
                      if (
                        carry eq one
                      ) {
                        carryadder(ONE(NIL), c1, 0) 
                      } else if (
                        carry eq zero
                      ) {
                        c1 
                      } else {
                        carryadder(MONE(NIL), c1, 0)
                      }
                  }
                }
              }
              case ZERO(d1) => {
                c2 match {
                  case ONE(d2) => {
                    
                      if (
                        carry eq one
                      ) {
                        ZERO(carryadder(d1, d2, 1)) 
                      } else if (
                        carry eq zero
                      ) {
                        ONE(carryadder(d1, d2, 0)) 
                      } else {
                        ZERO(carryadder(d1, d2, 0))
                      }
                  }
                  case ZERO(d2) => {
                    
                      if (
                        carry eq one
                      ) {
                        ONE(carryadder(d1, d2, 0)) 
                      } else if (
                        carry eq zero
                      ) {
                        ZERO(carryadder(d1, d2, 0)) 
                      } else {
                        MONE(carryadder(d1, d2, 0))
                      }
                  }
                  case MONE(d2) => {
                    
                      if (
                        carry eq one
                      ) {
                        ZERO(carryadder(d1, d2, 0)) 
                      } else if (
                        carry eq zero
                      ) {
                        MONE(carryadder(d1, d2, 0)) 
                      } else {
                        ZERO(carryadder(d1, d2, -(1)))
                      }
                  }
                  case NIL => {
                    
                      if (
                        carry eq one
                      ) {
                        carryadder(ONE(NIL), c1, 0) 
                      } else if (
                        carry eq zero
                      ) {
                        c1 
                      } else {
                        carryadder(MONE(NIL), c1, 0)
                      }
                  }
                }
              }
              case MONE(d1) => {
                c2 match {
                  case ONE(d2) => {
                    
                      if (
                        carry eq one
                      ) {
                        ONE(carryadder(d1, d2, 0)) 
                      } else if (
                        carry eq zero
                      ) {
                        ZERO(carryadder(d1, d2, 0)) 
                      } else {
                        MONE(carryadder(d1, d2, 0))
                      }
                  }
                  case ZERO(d2) => {
                    
                      if (
                        carry eq one
                      ) {
                        ZERO(carryadder(d1, d2, 0)) 
                      } else if (
                        carry eq zero
                      ) {
                        MONE(carryadder(d1, d2, 0)) 
                      } else {
                        ZERO(carryadder(d1, d2, -(1)))
                      }
                  }
                  case MONE(d2) => {
                    
                      if (
                        carry eq one
                      ) {
                        MONE(carryadder(d1, d2, 0)) 
                      } else if (
                        carry eq zero
                      ) {
                        ZERO(carryadder(d1, d2, -(1))) 
                      } else {
                        MONE(carryadder(d1, d2, -(1)))
                      }
                  }
                  case NIL => {
                    
                      if (
                        carry eq one
                      ) {
                        carryadder(ONE(NIL), c1, 0) 
                      } else if (
                        carry eq zero
                      ) {
                        c1 
                      } else {
                        carryadder(MONE(NIL), c1, 0)
                      }
                  }
                }
              }
            }
          }
          carryadder(c2, c1, 0)
        }
      }
    }
  }
}