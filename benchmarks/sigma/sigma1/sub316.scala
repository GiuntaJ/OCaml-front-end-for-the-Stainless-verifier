import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub316 {
  /* 생명과학부/2011-10915/신지민 Homework 1-2 */
  
  
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) =>
      {
        
          if (
            a eq b
          ) {
            f(a) 
          } else if (
            a > b
          ) {
            0 
          } else {
            sigma(a, b - 1, f) + sigma(b, b, f)
          }
    }
  }
  
  /*
  let f : int -> int = fun x->x+1
  
  
  let x : int  = sigma (7,7,f)
  let _= print_endline(string_of_int x) 
  	
  let x : int = sigma (10,7,f)
  let _= print_endline(string_of_int x)
  */
}