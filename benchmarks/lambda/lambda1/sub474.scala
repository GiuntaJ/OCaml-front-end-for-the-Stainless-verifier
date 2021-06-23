import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub474 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  /*
  let rec checkMetro (m: metro) : bool =
    match m with
    | STATION n -> true
    | CONNECT (m1, m2) -> (checkMetro m1) && (checkMetro m2)
    | AREA (n, m) -> 
        ( match m with
        | STATION n2 -> (if (n=n2) then true else false)
        | _ -> (checkMetro m)
        )
  */
  def checkMemory(((m, mem))): Boolean = {
    mem match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == m) true else checkMemory(m, tl) }
    }
  }
  
  def checkMetro_wMem(((m, mem))): Boolean = {
    m match {
      case STATION(n) => { checkMemory(n, mem) }
      case CONNECT(m1, m2) => {
        checkMetro_wMem(m1, mem) && checkMetro_wMem(m2, mem)
      }
      case AREA(n, m1) => { checkMetro_wMem(m1, n :: mem) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetro_wMem(m, Nil()) }
}