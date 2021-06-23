import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub147 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def checkMetro: Metro => Boolean = val _0 = {
    def matchStr: (List[Name], Name) => Boolean = {
      case (namelist, name) =>
        {
          namelist match {
            case Cons(h, t) => { if (h == name) true else matchStr(t, name) }
            case Nil() => { false }
          }
      }
    }
    val _1 = {
      def checkDeeper: (Metro, List[Name]) => Boolean = {
        case (metro, namelist) =>
          {
            metro match {
              case STATION(name) => { matchStr(namelist, name) }
              case AREA(n, m) => { checkDeeper(m, List(n) ++ namelist) }
              case CONNECT(m1, m2) => {
                checkDeeper(m1, namelist) && checkDeeper(m2, namelist)
              }
            }
        }
      }
      ( (metro) => { checkDeeper(metro, Nil()) } )
    }
  }
}