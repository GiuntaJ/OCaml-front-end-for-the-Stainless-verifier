import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub413 {
  /*
   CSE 2012-11226 Kwak Jin Han
   exercise 4
   */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /*
  /* true */
  let a = AREA ("a", STATION "a")
  let b = AREA ("a", AREA ("a", STATION "a"))
  let c = AREA ("a", AREA ("b", CONNECT (STATION "a", STATION "b")))
  let d = AREA ("a", CONNECT (STATION "a", AREA ("b", STATION "a")))
  /* false */
  let e = AREA ("a", STATION "b")
  let f = AREA ("a", CONNECT (STATION "a", AREA ("b", STATION "c")))
  let g = AREA ("a", AREA ("b", CONNECT (STATION "a", STATION "c")))
  */
  
  /* checkMetro : metro -> bool */
  def checkMetro(m) = {
    val _2 = {
      def main(((m, saveStation))) = {
        m match {
          case STATION(pname) => {
            if (saveStation.contains(pname) == true) true else false
          }
          case AREA(pname, pmetro) => { main(pmetro, pname :: saveStation) }
          case CONNECT(leftmetro, rightmetro) => {
            main(leftmetro, saveStation) && main(rightmetro, saveStation)
          }
        }
      }
      main(m, Nil())
    }
  }
}