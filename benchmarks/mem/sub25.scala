import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub25 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def mem(n: Int63, tree: Btree): Boolean = {
    tree match {
      case Empty => { false }
      case Node(a, b, c) => {
        
          if (
            a == n
          ) {
            true 
          } else if (
            mem(n, b)
          ) {
            true 
          } else {
            mem(n, c)
          }
      }
    }
  } 
}