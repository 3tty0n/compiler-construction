def append(l1: List[Int], l2: List[Int]): List[Int] =
  if (l1.isEmpty)
    l2
  else
    l1.head::append(l1.tail, l2)

def ltList(x: Int, l: List[Int]): List[Int] =
  if (l.isEmpty)
    Nil
  else if (l.head < x)
    l.head::ltList(x, l.tail)
  else
    ltList(x, l.tail)

def geList(x: Int, l: List[Int]): List[Int] =
  if (l.isEmpty)
    Nil
  else if (x == l.head)
    l.head::geList(x, l.tail)
  else if (x < l.head)
    l.head::geList(x, l.tail)
  else
    geList(x, l.tail)

def qsort(l: List[Int]): List[Int] =
  if (l.isEmpty)
    Nil
  else
    append(qsort(ltList(l.head, l.tail)), l.head::qsort(geList(l.head, l.tail)))

