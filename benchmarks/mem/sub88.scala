import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub88 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def mem(a: Int63, bt: Btree): Boolean = {
    bt match {
      case Empty => { false }
      case Node(i, b1, b2) => {
        
          if (
            i == a
          ) {
            true 
          } else if (
            mem(a, b1)
          ) {
            true 
          } else {
            mem(a, b2)
          }
      }
    }
  }   
}