import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub357 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro: Metro => Boolean = (
    (a) =>
      {
        a match {
          case STATION(q) => { false }
          case AREA(q, p) => {
            p match {
              case STATION(x) => { if (q == x) true else false }
              case AREA(x, y) => {
                y match {
                  case AREA(a, b) => {
                    checkMetro(AREA(a, b)) || checkMetro(AREA(x, b)) ||
                    checkMetro(AREA(q, b))
                  }
                  case STATION(a) => { a == x || a == q }
                  case CONNECT(a, b) => {
                    (a, b) match {
                      case (STATION(a), STATION(b)) => {
                        (a == q || a == x) && (b == q || b == x)
                      }
                      case (STATION(a), AREA(c, d)) => {
                        (a == q || a == x) &&
                        (checkMetro(AREA(c, d)) || checkMetro(AREA(x, d)) ||
                         checkMetro(AREA(q, d)))
                      }
                      case (AREA(a, b), STATION(c)) => {
                        (checkMetro(AREA(a, b)) || checkMetro(AREA(x, b)) ||
                         checkMetro(AREA(q, b))) &&
                        (c == q || c == x)
                      }
                      case (AREA(a, b), AREA(c, d)) => {
                        (checkMetro(AREA(a, b)) || checkMetro(AREA(x, b)) ||
                         checkMetro(AREA(q, b))) &&
                        (checkMetro(AREA(c, d)) || checkMetro(AREA(x, d)) ||
                         checkMetro(AREA(q, d)))
                      }
                      case (_, _) => { checkMetro(a) && checkMetro(b) }
                    }
                  }
                }
              }
              case CONNECT(x, y) => {
                (x, y) match {
                  case (STATION(a), STATION(b)) => { a == q && b == q }
                  case (STATION(a), AREA(c, d)) => {
                    a == q && (checkMetro(AREA(c, d)) || checkMetro(AREA(q, d)))
                  }
                  case (AREA(a, b), STATION(c)) => {
                    (checkMetro(AREA(a, b)) || checkMetro(AREA(q, b))) && c == q
                  }
                  case (AREA(a, b), AREA(c, d)) => {
                    (checkMetro(AREA(a, b)) || checkMetro(AREA(q, b))) &&
                    (checkMetro(AREA(c, d)) || checkMetro(AREA(q, d)))
                  }
                  case (_, _) => { checkMetro(x) && checkMetro(y) }
                }
              }
            }
          }
          case CONNECT(q, p) => { checkMetro(q) && checkMetro(p) }
        }
    }
  )
   
             
  /*
  let _= 
  let print_bool x = print_endline (string_of_bool x) in 
  
  let a81 = checkMetro (AREA("a", STATION "a")) in 
  let a82 = checkMetro (AREA("a", AREA("a", STATION "a"))) in 
  let a83 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) in 
  let a84 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) in 
  let a85 = checkMetro (AREA("a", STATION "b")) in 
  let a86 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) in 
  let a87 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) in 
  
  print_bool(false = checkMetro ( STATION "a")); 
  print_bool(true = checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a")))))); 
  print_bool(false = checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c")))))); 
  print_bool(true = a81); 
  print_bool(true = a82); 
  print_bool(true = a83); 
  print_bool(true = a84); 
  print_bool(false = a85); 
  print_bool(false = a86); 
  print_bool(false = a87);;
  
  
  
  
  */
}