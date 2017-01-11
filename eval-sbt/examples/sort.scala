def insert(x: Int, l: List[Int]): List[Int] =
  if (l.isEmpty)
    x::Nil
  else if (x<l.head)
    x::l
  else
    l.head::insert(x, l.tail)

def sort(l: List[Int]): List[Int] =
  if (l.isEmpty)
    Nil
  else
    insert(l.head, sort(l.tail))

def test(): List[Int] = sort(4::2::5::1::3::Nil)

