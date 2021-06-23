import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub171 {
  sealed case class Not_implemented() extends Exception {}
  /*problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
    
  val easy: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        e match {
          case Const(y) => { Const(0) }
          case Var(y) => { if (y == x) Const(1) else Const(0) }
          case Power(y, i) => {
            
              if (
                y == x
              ) {
                
                  if (
                    i == 2
                  ) {
                    Times(List(Const(i), Var(x))) 
                  } else if (
                    i == 1
                  ) {
                    Const(1) 
                  } else {
                    Times(List(Const(i), Power(x, i - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case _ => { assert(false, "Not_implemented") }
        }
    }
  }
  
  
  
  def hard: ((Aexp, String), List[Aexp], List[Aexp]) => List[Aexp] = {
    case ((e, x), prev, next) =>
      {
        next match {
          case Cons(nhd, ntl) => {
            e match {
              case Times(al1) => {
                ntl match {
                  case Nil() => {
                    nhd match {
                      case Sum(al2) => {
                        val _46 = {
                          val diffnow = hard(Sum(al2), x, Nil(), al2)
                          val _47 = {
                            val now = prev ++ diffnow
                            prev match {
                              case Nil() => { List(Sum(List(Times(now)))) }
                              case _ => { List(Times(now)) }
                            }
                          }
                        }
                      }
                      case Times(al2) => {
                        val _42 = {
                          val diffnow = hard(Times(al2), x, Nil(), al2)
                          val _43 = {
                            val now = prev ++ diffnow
                            prev match {
                              case Nil() => { List(Sum(List(Times(now)))) }
                              case _ => { List(Times(now)) }
                            }
                          }
                        }
                      }
                      case _ => {
                        val _38 = {
                          val diffnow = easy(nhd, x)
                          val _39 = {
                            val now = prev ++ List(diffnow)
                            prev match {
                              case Nil() => { List(Sum(List(Times(now)))) }
                              case _ => { List(Times(now)) }
                            }
                          }
                        }
                      }
                    }
                  }
                  case _ => {
                    nhd match {
                      case Sum(al2) => {
                        val _33 = {
                          val diffnow = hard(Sum(al2), x, Nil(), al2)
                          val _34 = {
                            val now = (prev ++ diffnow) ++ ntl
                            val _35 = {
                              val later = hard(Times(al2), x, prev ++ List(nhd), ntl)
                              prev match {
                                case Nil() => {
                                  List(Sum(List(Times(now)) ++ later))
                                }
                                case _ => { List(Times(now)) ++ later }
                              }
                            }
                          }
                        }
                      }
                      case Times(al2) => {
                        val _28 = {
                          val diffnow = hard(Times(al2), x, Nil(), al2)
                          val _29 = {
                            val now = (prev ++ diffnow) ++ ntl
                            val _30 = {
                              val later = hard(Times(al2), x, prev ++ List(nhd), ntl)
                              prev match {
                                case Nil() => {
                                  List(Sum(List(Times(now)) ++ later))
                                }
                                case _ => { List(Times(now)) ++ later }
                              }
                            }
                          }
                        }
                      }
                      case _ => {
                        val _23 = {
                          val diffnow = easy(nhd, x)
                          val _24 = {
                            val now = (prev ++ List(diffnow)) ++ ntl
                            val _25 = {
                              val later = hard(Times(al1), x, prev ++ List(nhd), ntl)
                              prev match {
                                case Nil() => {
                                  List(Sum(List(Times(now)) ++ later))
                                }
                                case _ => { List(Times(now)) ++ later }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
              case Sum(al1) => {
                ntl match {
                  case Nil() => {
                    nhd match {
                      case Sum(al2) => {
                        val _20 = {
                          val diffnow = hard(Sum(al2), x, Nil(), al2)
                          prev match {
                            case Nil() => { List(Sum(diffnow)) }
                            case _ => { diffnow }
                          }
                        }
                      }
                      case Times(al2) => {
                        val _17 = {
                          val diffnow = hard(Times(al2), x, Nil(), al2)
                          prev match {
                            case Nil() => { List(Sum(diffnow)) }
                            case _ => { diffnow }
                          }
                        }
                      }
                      case _ => {
                        val _14 = {
                          val diffnow = easy(nhd, x)
                          prev match {
                            case Nil() => { List(Sum(List(diffnow))) }
                            case _ => { List(diffnow) }
                          }
                        }
                      }
                    }
                  }
                  case _ => {
                    nhd match {
                      case Sum(al2) => {
                        val _10 = {
                          val diffnow = hard(Sum(al2), x, Nil(), al2)
                          val _11 = {
                            val later = hard(Sum(al2), x, prev ++ List(nhd), ntl)
                            prev match {
                              case Nil() => { List(Sum(diffnow ++ later)) }
                              case _ => { diffnow ++ later }
                            }
                          }
                        }
                      }
                      case Times(al2) => {
                        val _6 = {
                          val diffnow = hard(Times(al2), x, Nil(), al2)
                          val _7 = {
                            val later = hard(Sum(al2), x, prev ++ List(nhd), ntl)
                            prev match {
                              case Nil() => { List(Sum(diffnow ++ later)) }
                              case _ => { diffnow ++ later }
                            }
                          }
                        }
                      }
                      case _ => {
                        val _2 = {
                          val diffnow = easy(nhd, x)
                          val _3 = {
                            val later = hard(Sum(al1), x, prev ++ List(nhd), ntl)
                            prev match {
                              case Nil() => { List(Sum(List(diffnow) ++ later))
                              }
                              case _ => { List(diffnow) ++ later }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
              case _ => { assert(false, "Not_implemented") }
            }
          }
          case _ => { assert(false, "Not_implemented") }
        }
    }
  }
     
  
  val diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        e match {
          case Const(y) => { easy(e, x) }
          case Var(y) => { easy(e, x) }
          case Power(y, i) => { easy(e, x) }
          case Times(al) => {
            val _53 = {
              val asdf = hard(Times(al), x, Nil(), al)
              asdf.head
            }
          }
          case Sum(al) => {
            val _50 = {
              val asdf = hard(Sum(al), x, Nil(), al)
              asdf.head
            }
          }
        }
    }
  }
}