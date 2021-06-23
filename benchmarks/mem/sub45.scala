import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub45 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {} 
  
  def mem(x: Int63, tree: Btree): Boolean = {
    tree match {
      case Node(data, ltree, rtree) => {
        
          if (
            x == data
          ) {
            true 
          } else if (
            x < data
          ) {
            mem(x, rtree) 
          } else {
            mem(x, ltree)
          }
      }
      case Empty => { false }
    }
  } 
}