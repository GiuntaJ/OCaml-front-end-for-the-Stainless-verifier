import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub67 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro(metro_input: Metro): Boolean = {
    val _2 = {
      def listStation(m_input) = {
        m_input match {
          case STATION(n) => { List(n) }
          case AREA(n, m) => { deleteAll(listStation(m), n) }
          case CONNECT(m1, m2) => { listStation(m1) ++ listStation(m2) }
        }
      }
      def deleteAll(((list_input, target))) = {
        list_input match {
          case Cons(l, remain_list) => {
            
              if (
                l == target
              ) {
                deleteAll(remain_list, target) 
              } else {
                l :: deleteAll(remain_list, target)
              }
          }
          case Nil() => { Nil() }
        }
      }
      if (listStation(metro_input) == Nil()) true else false
    }
  }
}