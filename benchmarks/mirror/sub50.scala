import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub50 {
  /*2014210080 Choi Kyuhyeon*/
  
  
  /*Problem 1*/
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def mirror(t: Btree): Btree = {
    t match {
      case Empty => { Empty }
      case Node(p, lc, rc) => { Node(p, mirror(rc), mirror(lc)) }
    }
  }
}