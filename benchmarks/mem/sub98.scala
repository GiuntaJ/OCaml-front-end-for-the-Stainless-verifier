import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub98 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  val getLeftChild: Btree => Btree = (
    (tree) =>
      {
        tree match {
          case Empty => {
            assert(false, "Failure with Tree does not have a left child.")
          }
          case Node(a, b, c) => { b }
        }
    }
  )
  
  val getRightChild: Btree => Btree = (
    (tree) =>
      {
        tree match {
          case Empty => {
            assert(false, "Failure with Tree does not have a right child.")
          }
          case Node(a, b, c) => { c }
        }
    }
  )
  
  val getInt: Btree => Int63 = (
    (tree) =>
      {
        tree match {
          case Empty => { assert(false, "Failure with Tree is empty.") }
          case Node(a, b, c) => { a }
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
            n == getInt(tree)
          ) {
            true 
          } else {
            mem(n, getLeftChild(tree)) || mem(n, getRightChild(tree))
          }
    }
  }
}