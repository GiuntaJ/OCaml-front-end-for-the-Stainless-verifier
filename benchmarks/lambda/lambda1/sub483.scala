import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub483 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def innermetro: (List[Name], Metro) => Boolean = {
    case (area_list, input) =>
      {
        input match {
          case CONNECT(a, b) => {
            innermetro(area_list, a) && innermetro(area_list, b)
          }
          case AREA(a, b) => {
            
              if (
                area_list.exists(( (x) => { a == x } ))
              ) {
                innermetro(area_list, b) 
              } else {
                innermetro(a :: area_list, b)
              }
          }
          case STATION(a) => {
            if (area_list.exists(( (x) => { a == x } ))) true else false
          }
        }
    }
  }
  
  val checkMetro: Metro => Boolean = (
    (input) =>
      {
        val _4 = {
          val area_list: List[Name] = Nil()
          innermetro(area_list, input)
        }
    }
  )
  	
  /*
  let rec checkMetro: metro -> bool =
  	fun(input) ->
  	match input with
  	| CONNECT (a, b) -> (checkMetro a && checkMetro b)
  	| AREA (a, b) ->
  		if a==[] then checkMetro b
  		else if		
  		if 
  		| [] -> checkMetro b
  		| Lists.exists (fun x->a=x) area_list =true
  	| STATION a -> List.exists (fun x->a=x) area_list
  */
  /*
  let rec checkMetro: metro->bool =
  	fun(input) ->
  	if input = CONNECT (a,b) then (checkMetro a && checkMetro b)
  	else if input = AREA (a,b) then 
  		if (List.exists (fun x->a=x) area_list) then checkMetro b
  		else area_list=area_list::a in
  */
}