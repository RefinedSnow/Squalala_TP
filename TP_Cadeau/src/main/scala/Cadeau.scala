import scala.annotation.tailrec

object Cadeau {

  def repartition(somme : Int, capacite : List[Int]) : Option[List[Int]] = {
    @tailrec
    def eq(somme : Int, capacite: List[Int], repart : List[Int], index : Int): Option[List[Int]] = {
      if(capacite.sum < somme)  None
      else if(somme == 0) Some(repart.sortWith(_<_))
      else if(capacite(index) == 0) eq(somme,capacite,repart,(index+1) %3)
      else eq(somme-1, capacite.updated(index,capacite(index) - 1),repart.updated(index,repart(index)+1),(index+1) % 3)
    }
    eq(somme,capacite,List(0,0,0),0)
  }
}
