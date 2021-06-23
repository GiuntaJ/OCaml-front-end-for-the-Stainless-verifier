import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub372 {
  type Name = String
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  
  /*let rec getan m : name list =
      match m with
      | AREA(a, b) -> a::[] @ (getan b)
      | CONNECT(a, b) -> (getan a) @ (getan b)
      | _ -> []*/
  
  /*let rec getan m : name list = 
      match m with
      | AREA (a, b) -> l @ a::[]
      | CONNECT (a, b) -> l @ (getan a) @ (getan b)
      | _ -> []*/
  
  /*let rec getsn m : name list = 
      match m with
      | STATION a -> a::[]
      | AREA(a, b) -> getsn b
      | CONNECT(a, b) -> (getsn a) @ (getsn b)*/
  
  /*let rec checkMetro m : bool = 
    /*  let memm sm : bool = (List.mem sm (getan m)) in*/
          match m with
          | AREA(a, STATION b) -> (List.for_all memm (getsn m))
          | AREA(a, b) -> (List.for_all memm (getsn m)) && (checkMetro b)
          | CONNECT(STATION a, STATION b) -> false
          | CONNECT(STATION a, b) -> (checkMetro b)
          | CONNECT(a, STATION b) -> (checkMetro a)
          | CONNECT(a, b) -> (checkMetro a) && (checkMetro b)
          | _ -> List.for_all memm (getsn m)*/
  
  def nameFilter(m: Metro, l: List[Name]): Boolean = {
    m match {
      case AREA(a, b) => { nameFilter(b, a :: l) }
      case CONNECT(a, b) => { nameFilter(a, l) && nameFilter(b, l) }
      case STATION(a) => { l.contains(a) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case AREA(a, b) => { nameFilter(m, Nil()) }
      case CONNECT(a, b) => { checkMetro(a) && checkMetro(b) }
      case _ => { false }
    }
  }
}