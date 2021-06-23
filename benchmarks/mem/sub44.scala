import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub44 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def mem: (Int63, Btree) => Boolean = {
    case (n, tree) =>
      {
        tree match {
          case Empty => { false }
          case Node(rt, ltree, rtree) => {
            
              if (
                rt eq n
              ) {
                true 
              } else if (
                mem(n, ltree) == true
              ) {
                true 
              } else if (
                mem(n, rtree) == true
              ) {
                true 
              } else {
                false
              }
          }
        }
    }
  }
}