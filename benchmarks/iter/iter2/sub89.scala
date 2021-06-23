import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub89 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        n match {
          case 0 => {
            val _9 = {
              val g: Int63 => Int63 = ( (c) => { c } )
              g
            }
          }
          case _ => {
            (
              (c) =>
                {
                  val _6 = {
                    def doit(x) = { f(c) }
                    iter(n - 1, f, doit(c))
                  }
              }
            )
          }
        }
    }
  }
  
  
  iter(9, ( (x) => { 2 + x } ), 0)
  
  /*
  
  
  
  let rec iter : int * (int -> int) -> (int -> int)
  = fun (n,f)c -> if n = 0 then c else (iter(n-1, f))(f(c));;
  
  let f x = x + 2;;
  iter (2, f)0;;
  
  
  let doit : (int->int)*(int->int) -> (int->int)
              = fun a b -> match a with
                | 
  let g : int->int
  = fun c -> c;;
  let rec iter (n,f) = if n = 0 then g else f(iter(n-1, f));;
  let f x = 2 + x;;
  
  f(f(f (g 0)));;*/
}