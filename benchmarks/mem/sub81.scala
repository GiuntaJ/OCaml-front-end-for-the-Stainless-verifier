import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub81 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def mem(x: Int63, btree: Btree): Boolean = {
    btree match {
      case Empty => { false }
      case Node(y, left, right) => {
        
          if (
            x == y
          ) {
            true 
          } else if (
            x < y
          ) {
            mem(x, left) 
          } else {
            mem(x, right)
          }
      }
    }
  }
}