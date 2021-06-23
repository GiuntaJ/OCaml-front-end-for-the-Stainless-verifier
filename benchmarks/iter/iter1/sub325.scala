import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub325 {
  /*
  	CSE / 2013-11426 / Im DongYeop
  	Homework 1 : Exercise 3
  */
  
  def compose(((f: A => A, g: A => A))): A => A = { ( (x) => { f(g(x)) } ) }
  
  def iter(((n: Int63, f: A => A))): A => A = {
    n match {
      case 0 => { ( (x) => { x } ) }
      case 1 => { f }
      case 2 => { compose(f, f) }
      case _ => { compose(f, iter(n - 1, f)) }
    }
  }
}
