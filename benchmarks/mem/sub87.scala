import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub87 {
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
          case Node(a, Empty, Empty) => { if (a eq n) true else false }
          case Node(a, b, Empty) => {
            
              if (
                a eq n
              ) {
                true 
              } else if (
                mem(n, b)
              ) {
                true 
              } else {
                false
              }
          }
          case Node(a, Empty, c) => {
            
              if (
                a eq n
              ) {
                true 
              } else if (
                mem(n, c)
              ) {
                true 
              } else {
                false
              }
          }
          case Node(a, b, c) => {
            
              if (
                a eq n
              ) {
                true 
              } else if (
                mem(n, b) || mem(n, c)
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