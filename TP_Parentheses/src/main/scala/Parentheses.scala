object Parentheses {

  def equilibre(phrase: List[Char]) :Boolean = {
    def eqPar(phrase: List[Char], nbPar : Int): Boolean ={
      if(phrase.isEmpty) nbPar == 0
      else {
        val diff = phrase.head match {
          case '(' => 1
          case ')' => -1
          case _ => 0
        }
        if(nbPar <= 0 && diff == -1 ) false
        else eqPar(phrase.tail,nbPar + diff)
      }
    }
     eqPar(phrase, 0)
  }


  def equilibreGenerique(co: Char, cf: Char)(phrase: List[Char]) : Boolean = {
    def eqPar(phrase: List[Char], nbPar : Int): Boolean ={
      if(phrase.isEmpty) nbPar == 0
      else {
        val diff = phrase.head match {
          case `co` => 1
          case `cf` => -1
          case _ => 0
        }
        if(nbPar <= 0 && diff == -1 ) false
        else eqPar(phrase.tail,nbPar + diff)
      }
    }
    eqPar(phrase, 0)
  }
  def equilibreXml : List[Char] => Boolean = equilibreGenerique('<','>')

}
