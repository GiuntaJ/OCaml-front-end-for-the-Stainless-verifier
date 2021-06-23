import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub62 {
  
  /********************/
  /*     Problem 4     */
  /********************/
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def mem(x: Int63, n: Btree): Boolean = {
    n match {
      case Empty => { false }
      case Node(k, left, right) => {
        
          if (
            x <= 0
          ) {
            false 
          } else if (
            k < x
          ) {
            mem(x, right) 
          } else if (
            k == x
          ) {
            true 
          } else if (
            k > x
          ) {
            true 
          } else {
            false
          }
      }
    }
  }
}