import scala.io.Source

object Main {
  val filepath = "c:\\test\\testsum.txt"
  val matches = "[^0-9,]+(,(100|-1|\\d{1,2})){3}"

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile(filepath, "SJIS")
    // 正規表現に合ったもの抜き出し。配列の2文字目以降がintに変換しても問題無いか確認。Stringリスト化する
    val target = source.getLines.withFilter(_.matches(matches)).map(_.split(",").toList).filter(filterStrToInt).toList
    val testStudentList = target.map(_ match {
      case List(a, b, c, d) => new TestStudent(a, b.toInt, c.toInt, d.toInt)
    })
    // 全ての結果表示
    println("------------結果------------")
    testStudentList.foreach(_.kekka)
    // 補修者表示
    println("-----------補修者-----------")
    testStudentList.withFilter(_.hosyuu).foreach(x => println(x.getName))
  }

  def safeStrToInt(str: String) = scala.util.Try(str.toInt).toOption
  def filterStrToInt(strlist: Seq[String]): Boolean = {
    strlist match {
      case _ :: tail => {
        tail.map(safeStrToInt).forall(_ match {case Some(_) => true; case None => false})
      }
      case _ => false
    }
  }
}

class TestStudent(name:String, koku:Int, suu:Int, ei:Int) {
  val sum = if(koku >= 0 && suu >= 0 && ei >= 0) koku + suu + ei else -1
  def kekka = println(s"名前「${name}」 / 国語:${koku} / 数学:${suu} / 英語:${ei} / 合計:${sum}")
  def hosyuu: Boolean = sum == -1
  def getName: String = name
}
