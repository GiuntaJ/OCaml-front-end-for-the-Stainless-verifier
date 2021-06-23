import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub41 {
  /* problem 1*/
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def combine(n: Int63, l: Btree, r: Btree): Btree = { Node(n, l, r) }
  
  def mirror: Btree => Btree = (
    (t) =>
      {
        t match {
          case Empty => { t }
          case Node(n, Empty, Empty) => { t }
          case Node(n, l, r) => { combine(n, mirror(r), mirror(l)) }
        }
    }
  )
}
