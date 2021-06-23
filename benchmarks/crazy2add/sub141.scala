import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub141 {
  /*
   * 2017 - 09 - 22
   * PL Homework 2-3
   * Joonmo Yang
  */
  
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  /* make reverse crazy2. */
  def rev_crz: (Crazy2, Crazy2) => Crazy2 = {
    case (crz, rst) =>
      {
        crz match {
          case NIL => { rst }
          case ZERO(c) => { rev_crz(c, ZERO(rst)) }
          case ONE(c) => { rev_crz(c, ONE(rst)) }
          case MONE(c) => { rev_crz(c, MONE(rst)) }
        }
    }
  }
  
  val crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (crz1, crz2) =>
      {
        val _2 = {
          def crazy2add_sub: (Crazy2, Crazy2, Crazy2, Crazy2) => Crazy2 = {
            case (c1, c2, crry, csum) =>
              {
                (c1, c2) match {
                  case (NIL, NIL) => {
                    crry match {
                      case NIL => { csum }
                      case ZERO(_) => { ZERO(csum) }
                      case ONE(_) => { ONE(csum) }
                      case MONE(_) => { MONE(csum) }
                    }
                  }
                  case (NIL, ZERO(c)) => {
                    crry match {
                      case NIL => { crazy2add_sub(NIL, c, NIL, ZERO(csum)) }
                      case ZERO(_) => { crazy2add_sub(NIL, c, NIL, ZERO(csum)) }
                      case ONE(_) => { crazy2add_sub(NIL, c, NIL, ONE(csum)) }
                      case MONE(_) => { crazy2add_sub(NIL, c, NIL, MONE(csum)) }
                    }
                  }
                  case (NIL, ONE(c)) => {
                    crry match {
                      case NIL => { crazy2add_sub(NIL, c, NIL, ONE(csum)) }
                      case ZERO(_) => { crazy2add_sub(NIL, c, NIL, ONE(csum)) }
                      case ONE(_) => {
                        crazy2add_sub(NIL, c, ONE(NIL), ZERO(csum))
                      }
                      case MONE(_) => { crazy2add_sub(NIL, c, NIL, ZERO(csum)) }
                    }
                  }
                  case (NIL, MONE(c)) => {
                    crry match {
                      case NIL => { crazy2add_sub(NIL, c, NIL, MONE(csum)) }
                      case ZERO(_) => { crazy2add_sub(NIL, c, NIL, MONE(csum)) }
                      case ONE(_) => { crazy2add_sub(NIL, c, NIL, ZERO(csum)) }
                      case MONE(_) => {
                        crazy2add_sub(NIL, c, MONE(NIL), ZERO(csum))
                      }
                    }
                  }
                  case (ZERO(c1z), NIL) => {
                    crry match {
                      case NIL => { crazy2add_sub(c1z, NIL, NIL, ZERO(csum)) }
                      case ZERO(_) => { crazy2add_sub(c1z, NIL, NIL, ZERO(csum))
                      }
                      case ONE(_) => { crazy2add_sub(c1z, NIL, NIL, ONE(csum)) }
                      case MONE(_) => { crazy2add_sub(c1z, NIL, NIL, MONE(csum))
                      }
                    }
                  }
                  case (ZERO(c1z), ZERO(c2z)) => {
                    crry match {
                      case NIL => { crazy2add_sub(c1z, c2z, NIL, ZERO(csum)) }
                      case ZERO(_) => { crazy2add_sub(c1z, c2z, NIL, ZERO(csum))
                      }
                      case ONE(_) => { crazy2add_sub(c1z, c2z, NIL, ONE(csum)) }
                      case MONE(_) => { crazy2add_sub(c1z, c2z, NIL, MONE(csum))
                      }
                    }
                  }
                  case (ZERO(c1z), ONE(c2o)) => {
                    crry match {
                      case NIL => { crazy2add_sub(c1z, c2o, NIL, ONE(csum)) }
                      case ZERO(_) => { crazy2add_sub(c1z, c2o, NIL, ONE(csum))
                      }
                      case ONE(_) => {
                        crazy2add_sub(c1z, c2o, ONE(NIL), ZERO(csum))
                      }
                      case MONE(_) => { crazy2add_sub(c1z, c2o, NIL, ZERO(csum))
                      }
                    }
                  }
                  case (ZERO(c1z), MONE(c2m)) => {
                    crry match {
                      case NIL => { crazy2add_sub(c1z, c2m, NIL, MONE(csum)) }
                      case ZERO(_) => { crazy2add_sub(c1z, c2m, NIL, MONE(csum))
                      }
                      case ONE(_) => { crazy2add_sub(c1z, c2m, NIL, ZERO(csum))
                      }
                      case MONE(_) => {
                        crazy2add_sub(c1z, c2m, MONE(NIL), ZERO(csum))
                      }
                    }
                  }
                  case (ONE(c1o), NIL) => {
                    crry match {
                      case NIL => { crazy2add_sub(c1o, NIL, NIL, ONE(csum)) }
                      case ZERO(_) => { crazy2add_sub(c1o, NIL, NIL, ONE(csum))
                      }
                      case ONE(_) => {
                        crazy2add_sub(c1o, NIL, ONE(NIL), ZERO(csum))
                      }
                      case MONE(_) => { crazy2add_sub(c1o, NIL, NIL, ZERO(csum))
                      }
                    }
                  }
                  case (ONE(c1o), ZERO(c2z)) => {
                    crry match {
                      case NIL => { crazy2add_sub(c1o, c2z, NIL, ONE(csum)) }
                      case ZERO(_) => { crazy2add_sub(c1o, c2z, NIL, ONE(csum))
                      }
                      case ONE(_) => {
                        crazy2add_sub(c1o, c2z, ONE(NIL), ZERO(csum))
                      }
                      case MONE(_) => { crazy2add_sub(c1o, c2z, NIL, ZERO(csum))
                      }
                    }
                  }
                  case (ONE(c1o), ONE(c2o)) => {
                    crry match {
                      case NIL => {
                        crazy2add_sub(c1o, c2o, ONE(NIL), ZERO(csum))
                      }
                      case ZERO(_) => {
                        crazy2add_sub(c1o, c2o, ONE(NIL), ZERO(csum))
                      }
                      case ONE(_) => {
                        crazy2add_sub(c1o, c2o, ONE(NIL), ONE(csum))
                      }
                      case MONE(_) => { crazy2add_sub(c1o, c2o, NIL, ZERO(csum))
                      }
                    }
                  }
                  case (ONE(c1o), MONE(c2m)) => {
                    crry match {
                      case NIL => { crazy2add_sub(c1o, c2m, NIL, ZERO(csum)) }
                      case ZERO(_) => { crazy2add_sub(c1o, c2m, NIL, ZERO(csum))
                      }
                      case ONE(_) => { crazy2add_sub(c1o, c2m, NIL, ONE(csum)) }
                      case MONE(_) => { crazy2add_sub(c1o, c2m, NIL, MONE(csum))
                      }
                    }
                  }
                  case (MONE(c1m), NIL) => {
                    crry match {
                      case NIL => { crazy2add_sub(c1m, NIL, NIL, MONE(csum)) }
                      case ZERO(_) => { crazy2add_sub(c1m, NIL, NIL, MONE(csum))
                      }
                      case ONE(_) => { crazy2add_sub(c1m, NIL, NIL, ZERO(csum))
                      }
                      case MONE(_) => {
                        crazy2add_sub(c1m, NIL, MONE(NIL), ZERO(csum))
                      }
                    }
                  }
                  case (MONE(c1m), ZERO(c2z)) => {
                    crry match {
                      case NIL => { crazy2add_sub(c1m, c2z, NIL, MONE(csum)) }
                      case ZERO(_) => { crazy2add_sub(c1m, c2z, NIL, MONE(csum))
                      }
                      case ONE(_) => { crazy2add_sub(c1m, c2z, NIL, ZERO(csum))
                      }
                      case MONE(_) => {
                        crazy2add_sub(c1m, c2z, MONE(NIL), ZERO(csum))
                      }
                    }
                  }
                  case (MONE(c1m), ONE(c2o)) => {
                    crry match {
                      case NIL => { crazy2add_sub(c1m, c2o, NIL, ZERO(csum)) }
                      case ZERO(_) => { crazy2add_sub(c1m, c2o, NIL, ZERO(csum))
                      }
                      case ONE(_) => { crazy2add_sub(c1m, c2o, NIL, ONE(csum)) }
                      case MONE(_) => { crazy2add_sub(c1m, c2o, NIL, MONE(csum))
                      }
                    }
                  }
                  case (MONE(c1m), MONE(c2m)) => {
                    crry match {
                      case NIL => {
                        crazy2add_sub(c1m, c2m, MONE(NIL), ZERO(csum))
                      }
                      case ZERO(_) => {
                        crazy2add_sub(c1m, c2m, MONE(NIL), ZERO(csum))
                      }
                      case ONE(_) => { crazy2add_sub(c1m, c2m, NIL, MONE(csum))
                      }
                      case MONE(_) => {
                        crazy2add_sub(c1m, c2m, MONE(NIL), MONE(csum))
                      }
                    }
                  }
                }
            }
          }
          rev_crz(crazy2add_sub(crz1, crz2, NIL, NIL), NIL)
        }
    }
  }
}