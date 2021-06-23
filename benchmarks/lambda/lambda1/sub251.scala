import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub251 {
  /* 4190,310 Programming Language (Fall 2014)
   * Homework 2 - Exercise 1
   * CSE / 2012-13456 / Gao, Chengbin */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def aux(met, under) = {
        met match {
          case CONNECT(m1, m2) => { aux(m1, under) && aux(m2, under) }
          case AREA(n, m) => { aux(m, n :: under) }
          case STATION(n) => { under.contains(n) }
        }
      }
      aux(met, Nil())
    }
  }
                
  /*
  let t1 = AREA("a", STATION "a")
  let t2 = AREA("a", AREA("a", STATION "a"))
  let t3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
  let t4 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
  let f1 = AREA("a", STATION "b")
  let f2 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
  let f3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
  let print_bool x = print_endline(string_of_bool x)
  let _ =
      print_bool (checkMetro t1) ;
      print_bool (checkMetro t2) ;
      print_bool (checkMetro t3) ;
      print_bool (checkMetro t4) ;
      print_bool (not (checkMetro f1)) ;
      print_bool (not (checkMetro f2)) ;
      print_bool (not (checkMetro f3)) ;
  */
}