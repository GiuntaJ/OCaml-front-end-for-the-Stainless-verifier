import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub61 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        val _4 = {
          def compare: (Int63, Int63) => Int63 = {
            case (a, b) => { if (a > b) a else b }
          }
          
            if (
              l.length > 1
            ) {
              
                if (
                  l.head > l.tail.head
                ) {
                  compare(l.head, max(l.tail)) 
                } else {
                  max(l.tail)
                } 
            } else {
              l.head
            }
        }
    }
  )
   
}