import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub57 {
  /*problem 1*/
    sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
    def mirror: Btree => Btree = (
    (t) =>
      {
        t match {
          case Empty => { Empty }
          case Node(n, left, right) => {
            
              if (
                left == Empty && right == Empty
              ) {
                Empty 
              } else {
                Node(n, mirror(right), mirror(left))
              }
          }
        }
    }
  )
}