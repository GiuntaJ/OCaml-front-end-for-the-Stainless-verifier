import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub289 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String 
  
  def checkMetro(met: Metro): Boolean = {
    val _2 = {
      def isElementOfList[A](e: A, l: List[A]): Boolean = {
        l match {
          case Nil() => { false }
          case Cons(h, t) => { if (e == h) true else isElementOfList(e, t) }
        }
      }
      val _3 = {
        def checkAreasTailing(met: Metro, areas: List[Name]): Boolean = {
          met match {
            case STATION(n) => { isElementOfList(n, areas) }
            case AREA(n, m) => { checkAreasTailing(m, n :: areas) }
            case CONNECT(m1, m2) => {
              checkAreasTailing(m1, areas) && checkAreasTailing(m2, areas)
            }
          }
        }
        checkAreasTailing(met, Nil())
      }
    }
  }
  
  /* MISUNDERSTOOD Version
    let rec tailAreas (met:metro) (tail:name list) :name list =
      match met with
  	| STATION n -> []
  	| AREA (n, m) -> n :: (tailAreas m)
  	| CONNECT (m1, m2) -> (tailAreas m1) @ (tailAreas m2)
    in
    let rec tailStations (met:metro) (tail:name list) :name list =
      match met with
      | STATION n -> n
      | AREA (n, m) -> tailStations m
      | CONNECT (m1, m2) -> (tailStations m1) @ (tailStations m2)
    in
    let rec isFstSublistOfSnd (fst:a' list) (snd:a' list) :bool =
      let rec isElementOfList (e:a') (l:a' list) :bool = 
  	  match l with
  	  | [] -> false
  	  | h :: t -> if e = a then true else isElementOfList e t
  	in
  	match fst with
  	| [] -> false
  	| h :: t -> if isElementOfList h snd then true else isFstSublistOfSnd t snd
    in
    isFstSublistOfSnd (tailStations met) (tailAreas met)
  */
  /* TESTCASE
  let m1 = AREA("a", STATION "a")
  let m2 = AREA("a", AREA("a", STATION "a"))
  let m3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
  let m4 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
  let m5 = AREA("a", STATION "b")
  let m6 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
  let m7 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
  
  let ms = m1::m2::m3::m4::m5::m6::m7::[]
  
  let f0 x = print_endline(string_of_bool (checkMetro x))
  
  let _ = List.map f0 ms 
  */
}