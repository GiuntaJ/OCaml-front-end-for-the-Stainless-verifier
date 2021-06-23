import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub345 {
  /* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
   * Programming Languages 2015 Fall
   * Homework 2, Exercise 3 */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /* helper function isStringExist */
  def isStringExist[A](e: A, l: List[A]): Boolean = {
    val _2 = {
      val x = List.find(( (x) => { x == e } ), l)
      true
    }
  }
  
  /* checkMetro */
  def checkMetro(metro: Metro): Boolean = {
    val _5 = {
      def checkMetroWithArea(areaList, metro) = {
        metro match {
          case STATION(name) => { isStringExist(name, areaList) }
          case AREA(name, metro) => {
            checkMetroWithArea(areaList ++ List(name), metro)
          }
          case CONNECT(m1, m2) => {
            checkMetroWithArea(areaList, m1) && checkMetroWithArea(areaList, m2)
          }
        }
      }
      checkMetroWithArea(Nil(), metro)
    }
  }
}