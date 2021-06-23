import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub75 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def treeGetnumber: Btree => Int63 = (
    (tree) =>
      {
        tree match {
          case Empty => { assert(false, "Failure with is empty") }
          case Node(n, child1, child2) => { n }
        }
    }
  )
  
  def treeGetchild1: Btree => Btree = (
    (tree) =>
      {
        tree match {
          case Empty => { assert(false, "Failure with is empty") }
          case Node(n, child1, child2) => { child1 }
        }
    }
  )
  
  def treeGetchild2: Btree => Btree = (
    (tree) =>
      {
        tree match {
          case Empty => { assert(false, "Failure with is empty") }
          case Node(n, child1, child2) => { child2 }
        }
    }
  )
  
  def mem: (Int63, Btree) => Boolean = {
    case (n, tree) =>
      {
        
          if (
            tree == Empty
          ) {
            false 
          } else if (
            n == treeGetnumber(tree)
          ) {
            true 
          } else {
            mem(n, treeGetchild1(tree)) || mem(n, treeGetchild2(tree))
          }
    }
  }
}