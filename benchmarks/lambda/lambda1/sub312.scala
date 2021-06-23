import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub312 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /*
  let rec arealist metrol =
  	match metrol with
  	| STATION i -> []
  	| AREA (i1, i2) -> (if List.mem i1 metrol then arealist i2 
  				else i1 :: arealist i2)
  	| CONNECT (i1, i2) -> arealist i1 :: arealist i2
  */
  
  /*
  let changeMetro list1 =
  	let _ list2 : (list1 * list)
  */
  /*
  let rec makearea : metro -> 'a list = function list1 ->
  	match list1 with
  	| AREA (i1, i2) -> i1 :: makearea i2
  	| STATION i -> []
  	| CONNECT (i1, i2) -> makearea i1 @ makearea i2
  */
  
  def checkMetrof(((metrol, list1))) = {
    metrol match {
      case STATION(i) => { if (list1.contains(i)) true else false }
      case CONNECT(i1, i2) => { checkMetrof(i1, list1) && checkMetrof(i2, list1)
      }
      case AREA(i3, i4) => { checkMetrof(i4, i3 :: list1) }
    }
  }
  
  def checkMetro(metrol: Metro): Boolean = { checkMetrof(metrol, Nil()) }
}