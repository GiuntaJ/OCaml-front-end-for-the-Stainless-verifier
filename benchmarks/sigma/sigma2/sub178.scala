import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub178 {
  /* Problem 1 */
  def fib: Int63 => Int63 = (
    (n) =>
      {
        
          if (
            n < 0
          ) {
            -(1) 
          } else {
            n match {
              case 0 => { 0 }
              case 1 => { 1 }
              case _ => { fib(n - 1) + fib(n - 2) }
            }
          }
    }
  )
  
  /* Problem 2 */
  def pascal: (Int63, Int63) => Int63 = {
    case (n1, n2) =>
      {
        
          if (
            n1 < 0 || n2 < 0
          ) {
            -(1) 
          } else if (
            n2 == 0 || n1 == n2
          ) {
            1 
          } else {
            pascal(n1 - 1, n2 - 1) + pascal(n1 - 1, n2)
          }
    }
  }
  
  /* Problem 3 */
  def prime: Int63 => Boolean = (
    (n) =>
      {
        val _4 = {
          def cal(n1: Int63, n2: Int63): Boolean = {
            
              if (
                n2 == 1
              ) {
                true 
              } else if (
                n1 % n2 == 0
              ) {
                false 
              } else {
                cal(n1, n2 - 1)
              }
          }
          if (n <= 1) false else cal(n, n - 1)
        }
    }
  )
  
  /* Problem 4 */
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        val _7 = {
          val sum = 0
          
            if (
              a > b
            ) {
              sum 
            } else {
              val _9 = {
                val sum = sum + f(a) + sigma(f, a + 1, b)
                sum
              }
            }
        }
    }
  }
}