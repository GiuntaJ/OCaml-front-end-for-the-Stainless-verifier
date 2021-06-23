import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub1 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro) = {
    m match {
      case AREA(a, STATION(b)) => { if (a == b) true else false }
      case AREA(a, AREA(b, c)) => {
        
          if (
            a == b
          ) {
            checkMetro(AREA(b, c)) 
          } else if (
            checkMetro(AREA(a, c)) == true
          ) {
            true 
          } else if (
            checkMetro(AREA(b, c)) == true
          ) {
            true 
          } else {
            false
          }
      }
      case AREA(a, CONNECT(b, c)) => {
        
          if (
            checkMetro(AREA(a, b)) == true
          ) {
            if (checkMetro(AREA(a, c)) == true) true else false 
          } else {
            false
          }
      }
    }
  }
  
  
  
  
  
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro) = {
    m match {
      case AREA(a, STATION(b)) => { if (a == b) true else false }
      case AREA(a, AREA(b, c)) => {
        
          if (
            a == b
          ) {
            checkMetro(AREA(b, c)) 
          } else if (
            checkMetro(AREA(a, c)) == true
          ) {
            checkMetro(AREA(a, c)) 
          } else if (
            checkMetro(AREA(b, c)) == true
          ) {
            checkMetro(AREA(b, c)) 
          } else {
            false
          }
      }
      case AREA(a, CONNECT(b, c)) => {
        
          if (
            checkMetro(AREA(a, b)) == true
          ) {
            if (checkMetro(AREA(a, c)) == true) true else false 
          } else {
            false
          }
      }
      case STATION(a) => { false }
      case CONNECT(a, b) => {
        
          if (
            checkMetro(a) == true
          ) {
            if (checkMetro(b) == true) true else false 
          } else {
            false
          }
      }
    }
  }
  
  
  
  
        
}