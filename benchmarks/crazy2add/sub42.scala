import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub42 {
  /* C:\Users\saigoy\Desktop\crazy2add.ml */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (lc, rc) =>
      {
        val _2 = {
          def crazy2val_single(c) = {
            c match {
              case NIL => { 0 }
              case ZERO(c) => { 0 }
              case ONE(c) => { 1 }
              case MONE(c) => { -(1) }
            }
          }
          val _3 = {
            def crazy2_sum(sum) = {
              
                if (
                  sum eq 1 || sum eq 3
                ) {
                  1 
                } else if (
                  sum eq -(1) || sum eq -(3)
                ) {
                  -(1) 
                } else {
                  0
                }
            }
            val _4 = {
              def crazy2_carry(sum) = {
                
                  if (
                    sum eq 2 || sum eq 3
                  ) {
                    1 
                  } else if (
                    sum eq -(2) || sum eq -(3)
                  ) {
                    -(1) 
                  } else {
                    0
                  }
              }
              val _5 = {
                def crazy2_tail(c) = {
                  c match {
                    case NIL => { NIL }
                    case ZERO(ctl) => { ctl }
                    case ONE(ctl) => { ctl }
                    case MONE(ctl) => { ctl }
                  }
                }
                val _6 = {
                  def crazy2add_with_carry(((lc, rc, carry))) = {
                    val _9 = {
                      val sum = crazy2val_single(lc) + crazy2val_single(rc) + carry
                      val _10 = {
                        val c_sum = crazy2_sum(sum)
                        val _11 = {
                          val c_carry = crazy2_carry(sum)
                          val _12 = {
                            val l_tail = crazy2_tail(lc)
                            val _13 = {
                              val r_tail = crazy2_tail(rc)
                              
                                if (
                                  lc eq NIL && rc eq NIL && carry eq 0
                                ) {
                                  NIL 
                                } else if (
                                  c_sum eq 0
                                ) {
                                  ZERO(
                                    crazy2add_with_carry(
                                      l_tail, r_tail, c_carry)) 
                                } else if (
                                  c_sum eq 1
                                ) {
                                  ONE(
                                    crazy2add_with_carry(
                                      l_tail, r_tail, c_carry)) 
                                } else {
                                  MONE(
                                    crazy2add_with_carry(
                                      l_tail, r_tail, c_carry))
                                }
                            }
                          }
                        }
                      }
                    }
                  }
                  crazy2add_with_carry(lc, rc, 0)
                }
              }
            }
          }
        }
    }
  }
}