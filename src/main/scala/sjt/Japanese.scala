package sjt

trait Japanese[A] {
  def isHiragana(value: A): Boolean
}
object JapaneseInstances{
  implicit val japaneseChar = new Japanese[Char] {
    def isHiragana(value: Char): Boolean = ('\u3041' <= value) && (value <= '\u309e')
  }
}
object Japanese{
  def isHiragana[A](input: A)(implicit p: Japanese[A]): Boolean = p.isHiragana(input)
}
object JapaneseSyntax{
  implicit class JapaneseOps[A](value: A) {
    def isHiragana(implicit p: Japanese[A]): Boolean = p.isHiragana(value)
  }
}
object Main {
  def main(args: Array[String]): Unit = {
    import JapaneseInstances._
    import JapaneseSyntax._
    Japanese.isHiragana('c')
    println('j'.isHiragana)

  }
}