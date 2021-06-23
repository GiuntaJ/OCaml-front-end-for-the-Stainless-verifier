import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub278 {
  /*
    Department : Electrical Engineering
    Student-Id : 2008-11923
    Name : HyeonIL Choi (최현일)
    Date: 2017-9-7
    Homework-# : 1-3
    Excercise-Name : Iterator
  */
  
  def iter(((n: Int63, f: A => A)), arg) = {
    
      if (
        n <= 0
      ) {
        arg 
      } else {
        val _3 = {
          val arg_0 = f(arg)
          iter(n - 1, f, arg_0)
        }
      }
  }
}
