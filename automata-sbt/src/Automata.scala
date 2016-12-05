class DFA[Q, A](
  val states: Set[Q],
  val alpha: Set[A],
  val transition: Map[(Q,A),Q],
  val q0: Q,
  val finalStates: Set[Q]) {

  def trans(q: Q, w: List[A]): Q =
    w match {
      case Nil => q
      case a::w => trans(transition(q,a), w)
    }

  def accept(w: List[A]) =
    try
      finalStates.contains(trans(q0, w))
    catch {
      case e: NoSuchElementException => false
    }
}

class NFA[Q,A](
  val states: Set[Q],
  val alpha: Set[A],
  t: Map[(Q,Option[A]),Set[Q]],          // εをNoneで表現
  val q0: Q,
  val finalStates: Set[Q]) {

  val transition = t.withDefaultValue(Set())
                   // キーに対して値が定義されていないときに空集合を返す

  def eclosure(aQs: Set[Q]) = {
    var qs = Set[Q]()
    var newQs = aQs
    while (newQs != qs) {
      qs = newQs
      for (q <- qs) newQs = newQs ++ transition((q, None))
    } 
    qs
  }

  def transSet(qs: Set[Q], w: List[A]): Set[Q] =
    w match {
      case Nil => eclosure(qs)
      case a::w => transSet(eclosure(qs).flatMap(q => transition((q,Some(a)))),w)
    }

  def trans(q: Q, w: List[A]): Set[Q] = transSet(Set(q), w)

  def accept(w: List[A]) =
      !(trans(q0, w) & finalStates).isEmpty

  def toDFA(): DFA[Set[Q],A] = {
    val q0DFA =eclosure(Set(q0))
    var statesDFA = Set(q0DFA)
    var u = List(q0DFA)            // リストをスタックとして使用
    var transitionDFA = Map[(Set[Q], A), Set[Q]]()

//    while (!u.isEmpty) {
//       ...
//     }
    new DFA(statesDFA, alpha, transitionDFA, q0DFA,
      statesDFA.filter(qs => !(qs & finalStates).isEmpty))
  }
}






