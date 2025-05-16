package ex3

object Solitaire extends App:
  type Mark = (Int, Int)
  type Solution = Seq[Mark]
  type IterableFactory = Solution => Iterable[Solution]
  val width = 5
  val height = 5
  val totalCells = width * height
  val start = (width / 2, height / 2) //posizione iniziale (centrale) da cui iniziare la costruzione della griglia
  given IterableFactory = LazyList(_)

  //verifico che gli spostamenti siano compiuti effettivamente dentro la griglia
  def isInside(pos: Mark): Boolean =
    val (x, y) = pos //suddivido la posizione in input in due coordinate
    x >= 0 && x < width && y >= 0 && y < height //se sono nella griglia, restituisco true, altrimenti false

  def nextMoves(pos: Mark): Seq[Mark] =
    val (x, y) = pos //suddivido la posizione in input in due coordinate
    Seq(
      (x + 2, y), (x - 2, y), //spostamento orizzontale superiore e inferiore
      (x, y + 2), (x, y - 2), //spostamento verticale superiore e inferiore
      (x + 1, y + 1), (x - 1, y - 1), //spostamento diagonale principale superiore e inferiore
      (x + 1, y - 1), (x - 1, y + 1) //spostamento diagonale secondaria superiore e inferiore
    ).filter(isInside) //eseguo solo gli spostamenti all'interno della griglia

  def placeMarks(path: Solution = Seq(start))(using factory: IterableFactory): Iterable[Solution] =
    if path.size == 13 //caso base: utilizzo solo 13 celle perchè altrimenti non trova risultati
    then
      factory(path) //alla fine, genero una lista contenente tutte le posizioni
    else
      for
        next <- nextMoves(path.last) //ricavo tutti i possibili spostamenti dell'ultimo elemento presente nella sequenza path (inizialmente c'è solo la coordinata centrale) (N.B.: next è vero che conterrà una lista di tutti gli spostamenti calcolati, ma il for yield ne considera uno alla volta, quindi il codice successivo viene eseguito su un solo spostamento alla volta)
        if !path.contains(next) //se path non contiene già lo spostamento calcolato, e quindi la posizione non è stata già visitata
        solution <- placeMarks(path :+ next) //allora lo inserisco nella lista finale e continuo
      yield solution

  /* funzionamento:
        per ogni riga:
          per ogni colonna:
            c'è (colonna, riga) nella lista di input?
              si: stampo sulla griglia *indice corrispondente alla coppia + 1*
              no: stampo sulla griglia *X*
     */
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse //inverto la lista data in input in modo da avere una visualizzazione in cui il 2 in alto
    val rows =
      for y <- 0 until height //per ogni riga, fino all'altezza specificata per la griglia
          row =
            for x <- 0 until width //costruisco una row iterando, man mano, su ogni colonna
                number = reversed.indexOf((x, y)) + 1 //cerco se il punto (x, y) (x e y sono ottenuti dalla definizione dei cicli for) è nella lista di input
            yield
              if number > 0 then //se l'indice esiste
                "%-2d ".format(number) //lo posiziono nella griglia
              else //altrimenti mostro una X sulla griglia
                "X  "
      yield row.mkString
    rows.mkString("\n") //combino tutte le righe in una sola stringa, separandole con \n

@main def run() =
  import Solitaire.*
  import Solitaire.given

  placeMarks().zipWithIndex.foreach {
    (sol, i) => //associo a ogni risultato un id
    println("Solution " + i)
    println(render(sol, width, height))
    println()
  }


