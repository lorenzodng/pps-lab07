package ex3

object Nqueen extends App:
  type Queen = (Int, Int)
  type Solution = Iterable[Queen]
  type IterableFactory = Solution => Iterable[Solution] //alias
  val size = 10
  given IterableFactory = LazyList(_)

  def placeQueens(n: Int = size)(using factory: IterableFactory): Iterable[Solution] = n match //LazyList(_) è una funzione che rispetta Solution => Iterable[Solution]
    case 0 => factory(Set()) // se n=0, ho terminato e restituisco una LazyList(Set())
    case _ =>
      for
        queens <- placeQueens(n - 1) //decremento size, cosi da valutare tutte le possibili caselle (il secondo parametro è "implicito", quindi non deve essere specificato). anche se questa chiamata ricorsiva è prima del codice che consente di verificare la correttezza della posizione della regina, il codice successivo verrà comunque eseguito tante volte per quante è stata effettuata la chiamata ricorsiva, e a ritroso (ovvero dall'ultima chiamata alla prima)
        y <- 1 to size //per ogni colonna della riga (di volta in volta ottenuta da size-1)
        queen = (n, y) //posiziono la regina nella cella
        if isSafe(queen, queens) // se la posizione è "safe", allora la includo nella lista da formare
      yield
        queens.toSeq :+ queen //costruisco la lista finale contenente le regine nelle rispettive posizioni "safe"

  def isSafe(queen: Queen, others: Iterable[Queen]) =
    others forall (!isAttacked(queen, _))

  def isAttacked(q1: Queen, q2: Queen) =
    //le regine si attaccano se sono sulla:
    q1._1 == q2._1 || //stessa riga
    q1._2 == q2._2 || //stessa colonna
    (q2._1 - q1._1).abs == (q2._2 - q1._2).abs //stessa diagonale

  def printSolution(si: (Solution, Int)): Unit =
    println();
    println(s"sol ${si._2}")
    for queen <- si._1; x <- 1 to size do
      print(if queen._2 == x then "Q " else ". ")
      if x == size then println()

@main def runSolution() =
  import Nqueen.*
  import Nqueen.given

  placeQueens().zipWithIndex foreach (printSolution(_)) //associo un indice a ogni possibile soluzione