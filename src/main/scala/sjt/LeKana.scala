package sjt

import JapaneseInstances._
import JapaneseSyntax._

sealed trait LeKana {
  def toHi: LeKana
  def toKa: LeKana
  def toRo: LeKana

  def extendVowel(s: String = this.toString): String

  def extendConsonant(s: String = this.toString): String

  /*
    def apply(hiragana: String, katakana: String, romaji: String):LeKana = this match {
      case Hiragana(hiragana, katakana, romaji) => new Hiragana(hiragana, katakana, romaji)
      case Katakana(hiragana, katakana, romaji) => new Katakana(hiragana, katakana, romaji)
      case Romaji(hiragana, katakana, romaji) => new Romaji(hiragana, katakana, romaji)
    }
  */
  def extendVowelAndConsonant(s: String = this.toString) = extendVowel(extendConsonant(s))

  def ==(that: String) = this.toString == that

}

object LeKana {

  import LeKana._

  val nonDiacritics = Set[LeKana](Hiragana("あ", "ア", "a"), Hiragana("か", "カ", "ka"), Hiragana("さ", "サ", "sa"), Hiragana("た", "タ", "ta"), Hiragana("な", "ナ", "na"), Hiragana("は", "ハ", "ha"), Hiragana("ま", "マ", "ma"), Hiragana("や", "ヤ", "ya"), Hiragana("ら", "ラ", "ra"), Hiragana("わ", "ワ", "wa")
    , Hiragana("い", "イ", "i"), Hiragana("き", "キ", "ki"), Hiragana("し", "シ", "shi"), Hiragana("ち", "チ", "chi"), Hiragana("に", "ニ", "ni"), Hiragana("ひ", "ヒ", "hi"), Hiragana("み", "ミ", "mi"), Hiragana("り", "リ", "ri")
    , Hiragana("う", "ウ", "u"), Hiragana("く", "ク", "ku"), Hiragana("す", "ス", "su"), Hiragana("つ", "ツ", "tsu"), Hiragana("ぬ", "ヌ", "nu"), Hiragana("ふ", "フ", "fu"), Hiragana("む", "ム", "mu"), Hiragana("ゆ", "ユ", "yu"), Hiragana("る", "ル", "ru")
    , Hiragana("え", "エ", "e"), Hiragana("け", "ケ", "ke"), Hiragana("せ", "セ", "se"), Hiragana("て", "テ", "te"), Hiragana("ね", "ネ", "ne"), Hiragana("へ", "ヘ", "he"), Hiragana("め", "メ", "me"), Hiragana("れ", "レ", "re")
    , Hiragana("お", "オ", "o"), Hiragana("こ", "コ", "ko"), Hiragana("そ", "ソ", "so"), Hiragana("と", "ト", "to"), Hiragana("の", "ノ", "no"), Hiragana("ほ", "ホ", "ho"), Hiragana("も", "モ", "mo"), Hiragana("よ", "ヨ", "yo"), Hiragana("ろ", "ロ", "ro"), Hiragana("を", "ヲ", "wo")
    , Hiragana("ん", "ン", "n"))


  private val preferedDiacritics = Set[LeKana](Hiragana("が", "ガ", "ga"), Hiragana("ざ", "ザ", "za"), Hiragana("だ", "ダ", "da"), Hiragana("ば", "バ", "ba"), Hiragana("ぱ", "パ", "pa")
    , Hiragana("ぎ", "ギ", "gi"), Hiragana("じ", "ジ", "ji"), Hiragana("び", "ビ", "bi"), Hiragana("ぴ", "ピ", "pi")
    , Hiragana("ぐ", "グ", "gu"), Hiragana("ず", "ズ", "zu"), Hiragana("づ", "ヅ", "dzu"), Hiragana("ぶ", "ブ", "bu"), Hiragana("ぷ", "プ", "pu")
    , Hiragana("げ", "ゲ", "ge"), Hiragana("ぜ", "ゼ", "ze"), Hiragana("で", "デ", "de"), Hiragana("べ", "ベ", "be"), Hiragana("ぺ", "ペ", "pe")
    , Hiragana("ご", "ゴ", "go"), Hiragana("ぞ", "ゾ", "zo"), Hiragana("ど", "ド", "do"), Hiragana("ぼ", "ボ", "bo"), Hiragana("ぽ", "ポ", "po"))
  private val unpreferedDiacritics = List(Hiragana("ぢ", "ヂ", "ji"))
  val diacritics = preferedDiacritics ++ unpreferedDiacritics

  private val preferedKana = nonDiacritics ++ preferedDiacritics
  val kana = nonDiacritics ++ diacritics


  val hiragana = kana.map(_.toHi)
  val katakana = kana.map(_.toKa)
  val romaji = kana.map(_.toRo)


  val yoonNonDiacritics = Set[LeKana](Hiragana("きゃ", "キャ", "kya"), Hiragana("しゃ", "シャ", "sha"), Hiragana("ちゃ", "チャ", "cha"), Hiragana("にゃ", "ニャ", "nya"), Hiragana("ひゃ", "ヒャ", "hya"), Hiragana("みゃ", "ミャ", "mya"), Hiragana("りゃ", "リャ", "rya")
                                     ,Hiragana("きゅ", "キュ", "kyu"), Hiragana("しゅ", "シュ", "shu"), Hiragana("ちゅ", "チュ", "chu"), Hiragana("にゅ", "ニュ", "nyu"), Hiragana("ひゅ", "ヒュ", "hyu"), Hiragana("みゅ", "ミュ", "myu"), Hiragana("りゅ", "リュ", "ryu")
                                     ,Hiragana("きょ", "キョ", "kyo"), Hiragana("しょ", "ショ", "sho"), Hiragana("ちょ", "チョ", "cho"), Hiragana("にょ", "ニョ", "nyo"), Hiragana("ひょ", "ヒョ", "hyo"), Hiragana("みょ", "ミョ", "myo"), Hiragana("りょ", "リョ", "ryo"))

  private val preferedYoonDiacritics = Set[LeKana](Hiragana("ぎゃ", "ギャ", "gya"), Hiragana("じゃ", "ジャ", "ja"), Hiragana("びゃ", "ビャ", "bya")
                                                  ,Hiragana("ぎゅ", "ギュ", "gyu"), Hiragana("じゅ", "ジュ", "ju"), Hiragana("びゅ", "ビュ", "byu")
                                                  ,Hiragana("ぎょ", "ギョ", "gyo"), Hiragana("じょ", "ジョ", "jo"), Hiragana("びょ", "ビョ", "byo"))

  private val unpreferedYoonDiacritics = Set(Hiragana("ぢゃ", "ヂャ", "ja"),Hiragana("ぢゅ", "ヂュ", "ju"),Hiragana("ぢょ", "ヂョ", "jo"), Hiragana("じぇ", "ジェ", "je"))

  val yoonDiacritics = preferedYoonDiacritics ++ unpreferedYoonDiacritics

  val yoon: Set[LeKana] = yoonDiacritics ++ yoonNonDiacritics
  private val preferedYoon: Set[LeKana] = preferedYoonDiacritics ++ yoonNonDiacritics

  val translateableSymbols:Set[LeKana] = Set(Hiragana("、", "、", ","),Hiragana("。", "。", "."),Hiragana("！", "！", "!"),Hiragana("？", "？", "?"))

  val allKana: Set[LeKana] = kana ++ yoon
  val allHiragana: Set[LeKana] = allKana.map(_.toHi)
  val allKatakana: Set[LeKana] = allKana.map(_.toKa)
  val allRomaji: Set[LeKana] = allKana.map(_.toRo)

  val allKanaStr: Set[String] = allKana.map(_.toString)
  val allHiraganaStr: Set[String] = allHiragana.map(_.toString)
  val allKatakanaStr: Set[String] = allKatakana.map(_.toString)
  val allRomajiStr: Set[String] = allRomaji.map(_.toString)

  val allHiraganaEV: Set[String] = allHiragana.map(_.extendVowel())
  val allKatakanaEV: Set[String] = allKatakana.map(_.extendVowel())
  val allRomajiEV: Set[String] = allRomaji.map(_.extendVowel())
  val allKanaEV: Set[String] = allHiraganaEV ++ allKatakanaEV ++ allRomajiEV

  val allHiraganaEC: Set[String] = allHiragana.map(_.extendConsonant())
  val allKatakanaEC: Set[String] = allKatakana.map(_.extendConsonant())
  val allRomajiEC: Set[String] = allRomaji.map(_.extendConsonant())
  val allKanaEC: Set[String] = allHiraganaEC ++ allKatakanaEC ++ allRomajiEC

  val allHiraganaECEV: Set[String] = allHiragana.map(_.extendVowelAndConsonant())
  val allKatakanaECEV: Set[String] = allKatakana.map(_.extendVowelAndConsonant())
  val allRomajiECEV: Set[String] = allRomaji.map(_.extendVowelAndConsonant())
  val allKanaECEV: Set[String] = allHiraganaECEV ++ allKatakanaECEV ++ allRomajiECEV

  val everyHiraganaSyllable: Set[String] = allHiraganaStr ++ allHiraganaEC ++ allHiraganaEV ++ allHiraganaECEV
  val everyKatakanaSyllable: Set[String] = allKatakanaStr ++ allKatakanaEC ++ allKatakanaEV ++ allKatakanaECEV
  val everyRomajiSyllable: Set[String] = allRomajiStr ++ allRomajiEC ++ allRomajiEV ++ allRomajiECEV
  val everyKanaSyllable: Set[String] = everyRomajiSyllable ++ everyKatakanaSyllable ++ everyHiraganaSyllable

  def featuresByLength(k: LeKana) = List(k.toHi.toString, k.toKa.toString, k.toRo.toString, k.toHi.extendVowel(), k.toHi.extendConsonant(), k.toHi.extendVowelAndConsonant(), k.toKa.extendVowel(), k.toKa.extendConsonant(), k.toKa.extendVowelAndConsonant(),k.toRo.extendVowel(), k.toRo.extendConsonant(), k.toRo.extendVowelAndConsonant()).sortWith(_.length > _.length)
  private val ka:Map[LeKana, List[String]] = preferedKana.map(k => (k -> featuresByLength(k))).toMap
  private val yo:Map[LeKana, List[String]] = preferedYoon.map(k => (k -> featuresByLength(k))).toMap
  private val yoCHs:Map[LeKana, List[String]] = unpreferedYoonDiacritics.map(k => (k -> featuresByLength(k))).toMap
  private val kaUP:Map[LeKana, List[String]] = unpreferedDiacritics.map(k => (k -> featuresByLength(k))).toMap
  private val sym:Map[LeKana, List[String]] = translateableSymbols.map(k => (k -> featuresByLength(k))).toMap


  def nextSyllable(s: String): (LeKana,String) = {
    for (index <- 0 to 11) {
      for (entry <- yo) {
        val element: String = entry._2(index)
        if (s.length >= element.length && s.take(element.length) == element) return (entry._1, element)
      }
      for (entry <- yoCHs) {
        val element: String = entry._2(index)
        if (s.length >= element.length && s.take(element.length) == element) return (entry._1, element)
      }
      for (entry <- ka) {
        val element: String = entry._2(index)
        if (s.length >= element.length && s.take(element.length) == element) return (entry._1, element)
      }
      for (entry <- kaUP) {
        val element: String = entry._2(index)
        if (s.length >= element.length && s.take(element.length) == element) return (entry._1, element)
      }
      for (entry <- sym) {
        val element: String = entry._2(index)
        if (s.length >= element.length && s.take(element.length) == element) return (entry._1, element)
      }
    }
    val noValue = if (s.headOption.isDefined) s.head.toString else ""
    (NotKana(noValue,noValue,noValue), noValue)
  }

  def transliterate(k: (LeKana, String), f: LeKana => LeKana):String ={
    if (k._1.toHi == k._2 || k._1.toKa == k._2 || k._1.toRo == k._2) f(k._1).toString
    else if (k._1.toHi.extendVowel() == k._2 || k._1.toKa.extendVowel() == k._2 || k._1.toRo.extendVowel() == k._2) f(k._1).extendVowel()
    else if (k._1.toHi.extendConsonant() == k._2 || k._1.toKa.extendConsonant() == k._2 || k._1.toRo.extendConsonant() == k._2) f(k._1).extendConsonant()
    else f(k._1).extendVowelAndConsonant()
  }
  def toRomaji(ks:List[(LeKana, String)]):String = ks.reverse.foldLeft("")((k,v) => k + transliterate(v, l => l.toRo)).replaceAll("・", " ")
  def toHiragana(ks:List[(LeKana, String)]):String = ks.reverse.foldLeft("")((k,v) => k + transliterate(v, l => l.toHi)).replaceAll("・", "")
  def toKatakana(ks:List[(LeKana, String)]):String = ks.reverse.foldLeft("")((k,v) => k + transliterate(v, l => l.toKa))

}

case class Hiragana(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {
  override def extendVowel(s: String = this.hiragana): String = s + "う"
  override def extendConsonant(s: String = this.hiragana): String = "っ" + s

  override def toString = hiragana

  override def toHi: Hiragana = this
  override def toKa: Katakana = Katakana(hiragana, katakana, romaji)
  override def toRo: Romaji = Romaji(hiragana, katakana, romaji)

}

case class Katakana(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {
  override def extendVowel(s: String = this.katakana): String = s + "ー"
  override def extendConsonant(s: String = this.katakana): String = "ッ" + s

  override def toString = katakana

  override def toHi: Hiragana = Hiragana(hiragana, katakana, romaji)
  override def toKa: Katakana = this
  override def toRo: Romaji = Romaji(hiragana, katakana, romaji)
}

case class Romaji(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {
  override def extendVowel(s: String = this.romaji): String = s.init + Map('a' -> 'ā', 'e' -> 'ē', 'i' -> 'ī', 'o' -> 'ō', 'u' -> 'ū').getOrElse(s.last, s.last)
  override def extendConsonant(s: String = this.romaji): String = if (!japaneseChar.isVowel(s.head)) s.head + s else s

  override def toString = romaji

  override def toHi: Hiragana = Hiragana(hiragana, katakana, romaji)
  override def toKa: Katakana = Katakana(hiragana, katakana, romaji)
  override def toRo: Romaji = this
}
case class NotKana(val hiragana:String, val katakana:String, val romaji: String) extends LeKana{
  override def toHi = this
  override def toKa = this
  override def toRo = this

  override def extendVowel(s: String): String = hiragana
  override def extendConsonant(s: String): String = hiragana
}
