object EnsemblesFonctions {

  // On représente des ensembles à l'aide de fonctions Int => Boolean
  type Ensemble = Int => Boolean
  // Renvoie un ensemble sous forme de chaîne de caractères
  val limite = 100

  // affiche un ensemble
  def afficheEnsemble(s: Ensemble) {
    println(chaine(s))
  }

  def chaine(s: Ensemble): String = {
    val xs = for (i <- -limite to limite if contient(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  // indique si un ensemble contient un élément
  def contient(s: Ensemble, elem: Int): Boolean = s(elem)


  // méthodes à définir ci-dessous

  // définir une méthode qui renvoie un singleton
  def singleton(elem: Int): Ensemble = {
    (x:Int) => x==elem
  }

  // définir une méthode qui renvoie l'union de deux ensembles
  def union(s: Ensemble, t: Ensemble): Ensemble = {
    (x:Int) => contient(s,x)||contient(t,x)
  }

  // définir une méthode renvoyant l'intersection de deux ensembles
  def intersection(s: Ensemble, t: Ensemble): Ensemble = {
    (x:Int) => contient(s,x)&&contient(t,x)
  }

  // définir une méthode diff qui renvoie la différence de deux ensembles (dans s, pas dans t)
  def difference(s: Ensemble, t: Ensemble): Ensemble = {
    (x:Int) => contient(s,x) && !contient(t,x)
  }

  //définir une méthode filtre, qui renvoie le sous ensemble pour lequel p est vraie
  def filtrer(s: Ensemble, p: Int => Boolean): Ensemble = {
    (x:Int) => contient(s,x)&&contient(p,x)
  }

  // définir une méthode pourTout, qui vérifie si p est vrai pour tout élément de s
  def pourTout(s: Ensemble, p: Int => Boolean): Boolean = {
    (-limite to limite).filter(x => contient(s,x)).forall(x => contient(p,x))
  }

  // définir une méthode ilExistequi renvoie vrai si un élément renvoie vraie pour p
  def ilExiste(s: Ensemble, p: Int => Boolean): Boolean = {
    (-limite to limite).filter(x => contient(s,x)).exists(p)
  }

  // définir une fonction image qui renvoie l'ensemble image de s
  def image(s: Ensemble, f: Int => Int): Ensemble = {
    (y:Int) => ilExiste(s,(x:Int) => f(x) == y)
  }

}
