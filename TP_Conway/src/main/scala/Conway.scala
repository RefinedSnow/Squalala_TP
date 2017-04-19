import scala.annotation.tailrec

class Conway(init: Int = 1) {

  def apply(rang: Int) : String = {
    def f(B: (Int,Int, List[Int]),A: Int) : (Int, Int, List[Int]) = {
      if(A == B._1) (B._1,B._2+1,B._3)
      else
        (A,1,B._1::B._2::B._3)
    }
    @tailrec
    def seaquence(state : Int, ls : List[Int]) : String = {
      if(state == rang) ls.mkString(" ")
      else {
        val prem_tuple : (Int,Int,List[Int]) = (ls.head,1,Nil)
        val after = ls.tail.foldLeft(prem_tuple)(f)
        seaquence(state +1 , (after._1 :: after._2 :: after._3).reverse)
      }
     }
    seaquence(1,init :: Nil)
  }
}
