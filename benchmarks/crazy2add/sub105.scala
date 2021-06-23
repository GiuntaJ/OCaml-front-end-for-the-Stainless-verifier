import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub105 {
  /* 2015-11380 박찬양 HW2_3 */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  sealed abstract class Crazy22 {}
  case object Z extends Crazy22 {}
  case object O extends Crazy22 {}
  case object M extends Crazy22 {}
  
  val crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (cra1, cra2) =>
      {
        val _2 = {
          def addadd(((a, b, c))) = {
            (a, b, c) match {
              case (NIL, NIL, Z) => { NIL }
              case (NIL, NIL, O) => { ONE(NIL) }
              case (NIL, NIL, M) => { MONE(NIL) }
              case (ONE(n), ONE(m), O) => { ONE(addadd(n, m, O)) }
              case (ONE(n), ONE(m), Z) | (ONE(n), ZERO(m), O) |
              (ZERO(n), ONE(m), O) => {
                ZERO(addadd(n, m, O))
              }
              case (ONE(n), ONE(m), M) | (ONE(n), ZERO(m), Z) |
              (ONE(n), MONE(m), O) | (ZERO(n), ONE(m), Z) |
              (ZERO(n), ZERO(m), O) | (MONE(n), ONE(m), O) => {
                ONE(addadd(n, m, Z))
              }
              case (ONE(n), ZERO(m), M) | (ONE(n), MONE(m), Z) |
              (ZERO(n), ONE(m), M) | (ZERO(n), ZERO(m), Z) |
              (ZERO(n), MONE(m), O) | (MONE(n), ONE(m), Z) |
              (MONE(n), ZERO(m), O) => {
                ZERO(addadd(n, m, Z))
              }
              case (ONE(n), MONE(m), M) | (ZERO(n), ZERO(m), M) |
              (ZERO(n), MONE(m), Z) | (MONE(n), ONE(m), M) |
              (MONE(n), ZERO(m), Z) | (MONE(n), MONE(m), O) => {
                MONE(addadd(n, m, Z))
              }
              case (ZERO(n), MONE(m), M) | (MONE(n), ZERO(m), M) |
              (MONE(n), MONE(m), Z) => {
                ZERO(addadd(n, m, M))
              }
              case (MONE(n), MONE(m), M) => { MONE(addadd(n, m, M)) }
              case (NIL, m, O) | (m, NIL, O) => { addadd(ONE(NIL), m, Z) }
              case (NIL, m, M) | (m, NIL, M) => { addadd(MONE(NIL), m, Z) }
              case (NIL, m, Z) | (m, NIL, Z) => { m }
            }
          }
          addadd(cra1, cra2, Z)
        }
    }
  }
}