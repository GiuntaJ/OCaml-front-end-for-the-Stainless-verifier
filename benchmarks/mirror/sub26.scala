import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub26 {
  /* problem 1*/
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def mirror: Btree => Btree = (
    (tree) =>
      {
        tree match {
          case Empty => { tree }
          case Node(a, t1, t2) => {
            
              if (
                t1 == Empty
              ) {
                Node(a, mirror(t2), Empty) 
              } else if (
                t2 == Empty
              ) {
                Node(a, Empty, mirror(t1)) 
              } else {
                Node(a, mirror(t2), mirror(t1))
              }
          }
        }
    }
  )
}