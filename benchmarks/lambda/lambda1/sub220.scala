import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub220 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /*
  let print_list f lst =
  	let rec print_elements = function
  	| [] -> ()
  	| h::t -> f h; print_string ";"; print_elements t
  	in
  	print_string "[";
  	print_elements lst;
  	print_string "]";;
  */
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def innerMetro(m, areanamelist) = {
        val _5 = {
          def search(n1, n2) = { n1 == n2 }
          m match {
            case STATION(s) => { areanamelist.exists(search(s)) }
            case AREA(a, m1) => { innerMetro(m1, a :: areanamelist) }
            case CONNECT(m1, m2) => {
              innerMetro(m1, areanamelist) && innerMetro(m2, areanamelist)
            }
          }
        }
      }
      innerMetro(met, Nil())
    }
  }
  
  /*
  let a81 = checkMetro (AREA("a", STATION "a")) 
  let a82 = checkMetro (AREA("a", AREA("a", STATION "a"))) 
  let a83 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) 
  let a84 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) 
  
  let a85 = checkMetro (AREA("a", STATION "b")) 
  let a86 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) 
  let a87 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) 
  
  */
  
}
