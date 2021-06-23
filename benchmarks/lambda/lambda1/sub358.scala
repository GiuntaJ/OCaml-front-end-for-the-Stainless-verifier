import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub358 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def station_list(x: Metro): List[Name] = {
    x match {
      case STATION(n) => { List(n) }
      case AREA(n, m) => { station_list(m) }
      case CONNECT(m1, m2) => { station_list(m1) ++(station_list(m2)) }
    }
  }
  
  def area_list(x: Metro): List[Name] = {
    x match {
      case STATION(n) => { Nil() }
      case AREA(n, m) => { List(n) ++(area_list(m)) }
      case CONNECT(m1, m2) => { area_list(m1) ++(area_list(m2)) }
    }
  }
  
  def is_included(((x, y))) = {
    x match {
      case Nil() => { true }
      case Cons(hd, tl) => { y.contains(hd) && is_included(tl, y) }
    }
  }
  
  /* check if stations from metro x is in area list l */
  def checkList(((x, l))) = {
    x match {
      case STATION(n) => { l.contains(n) }
      case AREA(n, m) => { checkList(m, n :: l) }
      case CONNECT(m1, m2) => { checkList(m1, l) && checkList(m2, l) }
    }
  }
  
  def checkMetro(x) = { checkList(x, Nil()) }
  /*
  	match x with
  	| STATION n -> false
  	| AREA (n, m) -> (match m with
  			 | STATION n_sub -> List.mem n_sub (area_list x)
  			 | AREA (n1, m1) -> is_included ((station_list m), (area_list x)) && (checkMetro m)
  			 | CONNECT (m1, m2) -> (match (m1, m2) with
  						| (AREA (m1_n, m1_m), _) -> is_included ((station_list m), (area_list x)) && (checkMetro
  						| (_, AREA (m2_n, m2_m)) -> 
  						| (_, _) -> )
  
  let rec checkMetro (x: metro): bool =
  	match x with
  	| STATION n -> false
  	| AREA (n, m) -> (match m with
  			 | STATION n_sub -> List.mem n (station_list m)
  			 | AREA (n1, m1) -> List.mem n (station_list m) || (checkMetro m)
  			 | CONNECT (m1, m2) -> (match (m1, m2) with
  						| (AREA (m1_n, m1_m), _) -> List.mem n (station_list m) || (checkMetro m1)
  						| (_, AREA (m2_n, m2_m)) -> List.mem n (station_list m) || (checkMetro m2)
  						| (_, _) -> List.mem n (station_list m)
  						)
  			)
  /* List.mem n (station_list m)) */
  	| CONNECT (m1, m2) -> /*(match (m1, m2) with
  				| (AREA (m_sub, n_sub), _) -> (checkMetro (AREA (m_sub, n_sub)))
  				| (_, AREA (m_sub2, n_sub2)) -> (checkMetro (AREA (m_sub2, n_sub2)))
  				| (_, _) -> false)
  */
  (checkMetro m1) && (checkMetro m2)
  */
}