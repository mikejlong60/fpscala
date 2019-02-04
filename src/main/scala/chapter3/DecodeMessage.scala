package decode

class Decoder() {

  val m = Map(
    "1" -> "a",
    "2" -> "b",
    "3" -> "c",
    "4" -> "d",
    "5" -> "e",
    "6" -> "f",
    "7" -> "g",
    "8" -> "h",
    "9" -> "i",
    "10" -> "j",
    "11" -> "k",
    "12" -> "l",
    "13" -> "m",
    "14" -> "n",
    "15" -> "o",
    "16" -> "p",
    "17" -> "q",
    "18" -> "r",
    "19" -> "s",
    "20" -> "t",
    "21" -> "u",
    "22" -> "v",
    "23" -> "w",
    "24" -> "x",
    "25" -> "y",
    "26" -> "z"
  )

  def size(i: List[String]): Int = i.length match {
    case 0 => 0
    case 1 => decode(i)
    case _ =>
      val v = (i sliding 2).foldLeft(0){ (acc, x) => acc + decode(x)}
      if(i.length % 2 > 0) {
        decode(List(i.last)) + v
      } else {
        v
      }
      
  }

  def decode(x: List[String]): Int = x.size match {
    case 1 =>
      if(m.contains(x.head)) {
        1
      } else {
        0
      }
    case _ =>
      List(m.get(x.head), m.get(x.mkString)).flatten.size
  }
  
}

