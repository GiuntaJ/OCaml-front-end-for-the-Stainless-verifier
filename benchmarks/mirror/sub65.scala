import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub65 {
  /* problem 1*/
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
          case Node(a, b, c) => {
            
              if (
                b == Empty && c == Empty
              ) {
                t 
              } else if (
                b == Empty
              ) {
                Node(a, mirror(c), Empty) 
              } else if (
                c == Empty
              ) {
                Node(a, Empty, mirror(b)) 
              } else {
                Node(a, mirror(c), mirror(b))
              }
          }
        }
    }
  )
}