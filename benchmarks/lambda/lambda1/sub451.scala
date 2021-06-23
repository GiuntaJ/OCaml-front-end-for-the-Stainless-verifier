import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub451 {
  /*
  	CSE / 2013-11426 / Im DongYeop
  	Homework 2: Exercies 4
  */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def isitin(((l: List[String], s: String))): Boolean = {
    l match {
      case Nil() => { false }
      case _ => { if (l.contains(s) eq true) true else false }
    }
  }
  /*	| hd::tl -> (
  		if (hd = s)
  			then false
  		else
  			isitin(tl, s))*/
  /*		(
  		if (List.hd l == s)
  			then true
  		else
  			isitin(List.tl l, s))
  */
  def inner(((l: List[String], m: Metro))): Boolean = {
    m match {
      case STATION(s) => { isitin(l, s) }
      case AREA(nn, mm) => { inner(nn :: l, mm) }
      case CONNECT(c1, c2) => {
        if (inner(l, c1) eq true && inner(l, c2) eq true) true else false
      }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { inner(Nil(), m) }
}
