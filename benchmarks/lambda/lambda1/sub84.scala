import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub84 {
  /* hw 1_7. */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def areaList(m1) = {
        m1 match {
          case AREA(a, b) => { a :: areaList(b) }
          case _ => { Nil() }
        }
      }
      val _3 = {
        def stationList(m2) = {
          m2 match {
            case STATION(a) => { List(a) }
            case AREA(a, b) => { stationList(b) }
            case CONNECT(a, b) => { stationList(a) ++ stationList(b) }
          }
        }
        val _4 = {
          def searchArea(al, st) = {
            al match {
              case Nil() => { false }
              case Cons(hd, tl) => { if (hd == st) true else searchArea(tl, st)
              }
            }
          }
          val _5 = {
            def matching(al, sl) = {
              sl match {
                case Nil() => { true }
                case Cons(hd, tl) => {
                  if (searchArea(al, hd) == false) false else matching(al, tl)
                }
              }
            }
            val _6 = {
              val _ = areaList(m)
              val _7 = {
                val _ = stationList(m)
                matching(areaList(m), stationList(m))
              }
            }
          }
        }
      }
    }
  }
  
  	/*
  let _ =
  	checkMetro (AREA("a", STATION "a"))
  let _ =
  	checkMetro (AREA("a", AREA("a", STATION "a")))
  let _ =
  	checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))
  let _ =
  	checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))
  let _ =
  	checkMetro (AREA("a", STATION "b"))
  let _ =
  	checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))
  let _ =
  	checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))
  	*/
}