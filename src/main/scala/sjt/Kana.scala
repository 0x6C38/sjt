package sjt

import JapaneseInstances._
import JapaneseSyntax._
import com.atilika.kuromoji.ipadic.Tokenizer

case class Kana(hiragana:String, katakana:String, romaji:String){
  /*
  def toHi: String
  def toKa: String
  def toRo: String
*/
  def extendVowel(s: String = this.toString): Kana = Kana(extendVowelHiragana(hiragana), extendVowelKatakana(katakana), extendVowelRomaji(romaji))
  def extendConsonant(s: String = this.toString): Kana = Kana(extendConsonantHiragana(hiragana), extendConsonantKatakana(katakana), extendConsonantRomaji(romaji))
  //def extendVowelAndConsonant(s: String = this.toString):Kana = Kana(extendVowel(extendConsonant(s).extendVowel().hiragana, extendVowel(extendConsonant(s).katakana).katakana, extendVowel(extendConsonant(s).romaji).romaji)
  def extendVowelAndConsonant(s: String = this.toString):Kana = Kana(extendConsonant(s).extendVowel().hiragana, extendConsonant(s).extendVowel().katakana, extendConsonant(s).extendVowel().romaji)

  def ==(that: String) = this.toString == that

  def extendVowelHiragana(s: String = this.hiragana): String = s + "う"
  def extendConsonantHiragana(s: String = this.hiragana): String = "っ" + s

  def extendVowelKatakana(s: String = this.katakana): String = s + "ー"
  def extendConsonantKatakana(s: String = this.katakana): String = "ッ" + s

  def extendVowelRomaji(s: String = this.romaji): String = s.init + Map('a' -> 'ā', 'e' -> 'ē', 'i' -> 'ī', 'o' -> 'ō', 'u' -> 'ū').getOrElse(s.last, s.last)
  def extendConsonantRomaji(s: String = this.romaji): String = if (!japaneseChar.isVowel(s.head)) s.head + s else s

}

object Kana {
  import Kana._
  lazy val tokenizer:Tokenizer = new Tokenizer()

  val nonDiacritics = Set[Kana](Kana("あ", "ア", "a"), Kana("か", "カ", "ka"), Kana("さ", "サ", "sa"), Kana("た", "タ", "ta"), Kana("な", "ナ", "na"), Kana("は", "ハ", "ha"), Kana("ま", "マ", "ma"), Kana("や", "ヤ", "ya"), Kana("ら", "ラ", "ra"), Kana("わ", "ワ", "wa")
    , Kana("い", "イ", "i"), Kana("き", "キ", "ki"), Kana("し", "シ", "shi"), Kana("ち", "チ", "chi"), Kana("に", "ニ", "ni"), Kana("ひ", "ヒ", "hi"), Kana("み", "ミ", "mi"), Kana("り", "リ", "ri")
    , Kana("う", "ウ", "u"), Kana("く", "ク", "ku"), Kana("す", "ス", "su"), Kana("つ", "ツ", "tsu"), Kana("ぬ", "ヌ", "nu"), Kana("ふ", "フ", "fu"), Kana("む", "ム", "mu"), Kana("ゆ", "ユ", "yu"), Kana("る", "ル", "ru")
    , Kana("え", "エ", "e"), Kana("け", "ケ", "ke"), Kana("せ", "セ", "se"), Kana("て", "テ", "te"), Kana("ね", "ネ", "ne"), Kana("へ", "ヘ", "he"), Kana("め", "メ", "me"), Kana("れ", "レ", "re")
    , Kana("お", "オ", "o"), Kana("こ", "コ", "ko"), Kana("そ", "ソ", "so"), Kana("と", "ト", "to"), Kana("の", "ノ", "no"), Kana("ほ", "ホ", "ho"), Kana("も", "モ", "mo"), Kana("よ", "ヨ", "yo"), Kana("ろ", "ロ", "ro"), Kana("を", "ヲ", "wo")
    , Kana("ん", "ン", "n"))


  private val preferedDiacritics = Set[Kana](Kana("が", "ガ", "ga"), Kana("ざ", "ザ", "za"), Kana("だ", "ダ", "da"), Kana("ば", "バ", "ba"), Kana("ぱ", "パ", "pa")
    , Kana("ぎ", "ギ", "gi"), Kana("じ", "ジ", "ji"), Kana("び", "ビ", "bi"), Kana("ぴ", "ピ", "pi")
    , Kana("ぐ", "グ", "gu"), Kana("ず", "ズ", "zu"), Kana("づ", "ヅ", "dzu"), Kana("ぶ", "ブ", "bu"), Kana("ぷ", "プ", "pu")
    , Kana("げ", "ゲ", "ge"), Kana("ぜ", "ゼ", "ze"), Kana("で", "デ", "de"), Kana("べ", "ベ", "be"), Kana("ぺ", "ペ", "pe")
    , Kana("ご", "ゴ", "go"), Kana("ぞ", "ゾ", "zo"), Kana("ど", "ド", "do"), Kana("ぼ", "ボ", "bo"), Kana("ぽ", "ポ", "po"))
  private val unpreferedDiacritics = List(Kana("ぢ", "ヂ", "ji"))
  val diacritics = preferedDiacritics ++ unpreferedDiacritics

  private val preferedKana = nonDiacritics ++ preferedDiacritics
  val kana = nonDiacritics ++ diacritics


  val hiragana = kana.map(_.hiragana)
  val katakana = kana.map(_.katakana)
  val romaji = kana.map(_.romaji)


  val yoonNonDiacritics = Set[Kana](Kana("きゃ", "キャ", "kya"), Kana("しゃ", "シャ", "sha"), Kana("ちゃ", "チャ", "cha"), Kana("にゃ", "ニャ", "nya"), Kana("ひゃ", "ヒャ", "hya"), Kana("みゃ", "ミャ", "mya"), Kana("りゃ", "リャ", "rya")
                                     ,Kana("きゅ", "キュ", "kyu"), Kana("しゅ", "シュ", "shu"), Kana("ちゅ", "チュ", "chu"), Kana("にゅ", "ニュ", "nyu"), Kana("ひゅ", "ヒュ", "hyu"), Kana("みゅ", "ミュ", "myu"), Kana("りゅ", "リュ", "ryu")
                                     ,Kana("きょ", "キョ", "kyo"), Kana("しょ", "ショ", "sho"), Kana("ちょ", "チョ", "cho"), Kana("にょ", "ニョ", "nyo"), Kana("ひょ", "ヒョ", "hyo"), Kana("みょ", "ミョ", "myo"), Kana("りょ", "リョ", "ryo"))

  private val preferedYoonDiacritics = Set[Kana](Kana("ぎゃ", "ギャ", "gya"), Kana("じゃ", "ジャ", "ja"), Kana("びゃ", "ビャ", "bya")
                                                  ,Kana("ぎゅ", "ギュ", "gyu"), Kana("じゅ", "ジュ", "ju"), Kana("びゅ", "ビュ", "byu")
                                                  ,Kana("ぎょ", "ギョ", "gyo"), Kana("じょ", "ジョ", "jo"), Kana("びょ", "ビョ", "byo"))

  private val unpreferedYoonDiacritics = Set(Kana("ぢゃ", "ヂャ", "ja"),Kana("ぢゅ", "ヂュ", "ju"),Kana("ぢょ", "ヂョ", "jo"), Kana("じぇ", "ジェ", "je"))

  val yoonDiacritics = preferedYoonDiacritics ++ unpreferedYoonDiacritics

  val yoon: Set[Kana] = yoonDiacritics ++ yoonNonDiacritics
  private val preferedYoon: Set[Kana] = preferedYoonDiacritics ++ yoonNonDiacritics

  val translateableSymbols:Set[Kana] = Set(Kana("、", "、", ","),Kana("。", "。", "."),Kana("！", "！", "!"),Kana("？", "？", "?"))
  val translateableSymbolsStr:Set[String] = translateableSymbols.flatMap(k => List(k.hiragana, k.katakana, k.romaji))
  def isTranslateableSymbol(s:String) = translateableSymbolsStr.contains(s)

  val allKana: Set[Kana] = kana ++ yoon
  /*
  val allHiragana: Set[Kana] = allKana.map(_.toHi)
  val allKatakana: Set[Kana] = allKana.map(_.toKa)
  val allRomaji: Set[Kana] = allKana.map(_.toRo)
*/
  val allKanaStr: Set[String] = allKana.map(_.toString)
  val allHiraganaStr: Set[String] = allKana.map(_.hiragana)
  val allKatakanaStr: Set[String] = allKana.map(_.katakana)
  val allRomajiStr: Set[String] = allKana.map(_.romaji)

  val allHiraganaEV: Set[String] = allKana.map(_.extendVowel().hiragana)
  val allKatakanaEV: Set[String] = allKana.map(_.extendVowel().katakana)
  val allRomajiEV: Set[String] = allKana.map(_.extendVowel().romaji)
  val allKanaEV: Set[String] = allHiraganaEV ++ allKatakanaEV ++ allRomajiEV

  val allHiraganaEC: Set[String] = allKana.map(_.extendConsonant().hiragana)
  val allKatakanaEC: Set[String] = allKana.map(_.extendConsonant().katakana)
  val allRomajiEC: Set[String] = allKana.map(_.extendConsonant().romaji)
  val allKanaEC: Set[String] = allHiraganaEC ++ allKatakanaEC ++ allRomajiEC

  val allHiraganaECEV: Set[String] = allKana.map(_.extendVowelAndConsonant().hiragana)
  val allKatakanaECEV: Set[String] = allKana.map(_.extendVowelAndConsonant().katakana)
  val allRomajiECEV: Set[String] = allKana.map(_.extendVowelAndConsonant().romaji)
  val allKanaECEV: Set[String] = allHiraganaECEV ++ allKatakanaECEV ++ allRomajiECEV

  val everyHiraganaSyllable: Set[String] = allHiraganaStr ++ allHiraganaEC ++ allHiraganaEV ++ allHiraganaECEV
  val everyKatakanaSyllable: Set[String] = allKatakanaStr ++ allKatakanaEC ++ allKatakanaEV ++ allKatakanaECEV
  val everyRomajiSyllable: Set[String] = allRomajiStr ++ allRomajiEC ++ allRomajiEV ++ allRomajiECEV
  val everyKanaSyllable: Set[String] = everyRomajiSyllable ++ everyKatakanaSyllable ++ everyHiraganaSyllable

  def featuresByLength(k: Kana) = List(k.hiragana, k.katakana, k.romaji, k.extendVowel().hiragana, k.extendConsonant().hiragana, k.extendVowelAndConsonant().hiragana, k.extendVowel().katakana, k.extendConsonant().katakana, k.extendVowelAndConsonant().katakana,k.extendVowel().romaji, k.extendConsonant().romaji, k.extendVowelAndConsonant().romaji).sortWith(_.length > _.length)
  private val ka:Map[Kana, List[String]] = preferedKana.map(k => (k -> featuresByLength(k))).toMap
  private val yo:Map[Kana, List[String]] = preferedYoon.map(k => (k -> featuresByLength(k))).toMap
  private val yoCHs:Map[Kana, List[String]] = unpreferedYoonDiacritics.map(k => (k -> featuresByLength(k))).toMap
  private val kaUP:Map[Kana, List[String]] = unpreferedDiacritics.map(k => (k -> featuresByLength(k))).toMap
  private val sym:Map[Kana, List[String]] = translateableSymbols.map(k => (k -> featuresByLength(k))).toMap


  def nextSyllable(s: String): (Kana,String) = {
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
    (Kana(noValue,noValue,noValue), noValue)
  }

  def transliterate(k: (Kana, String), f: Kana => String):String = {
    if (k._1.hiragana == k._2 || k._1.katakana == k._2 || k._1.romaji == k._2) f(k._1).toString
    else if (k._1.extendVowel().hiragana == k._2 || k._1.extendVowel().katakana == k._2 || k._1.extendVowel().romaji == k._2) f(k._1.extendVowel())
    else if (k._1.extendConsonant().hiragana == k._2 || k._1.extendConsonant().katakana == k._2 || k._1.extendConsonant().romaji == k._2) f(k._1.extendConsonant())
    else f(k._1.extendVowelAndConsonant())
  }
  def toRomaji(ks:List[(Kana, String)]):String = ks.reverse.foldLeft("")((k, v) => k + transliterate(v, l => l.romaji)).replaceAll("・", " ")
  def toHiragana(ks:List[(Kana, String)]):String = ks.reverse.foldLeft("")((k, v) => k + transliterate(v, l => l.hiragana)).replaceAll("・", "")
  def toKatakana(ks:List[(Kana, String)]):String = ks.reverse.foldLeft("")((k, v) => k + transliterate(v, l => l.katakana))

}
/*
case class Hiragana(val hiragana: String, val katakana: String, val romaji: String) extends Kana {
  override def extendVowel(s: String = this.hiragana): String = s + "う"
  override def extendConsonant(s: String = this.hiragana): String = "っ" + s

  override def toString = hiragana

  override def toHi: Hiragana = this
  override def toKa: Katakana = Katakana(hiragana, katakana, romaji)
  override def toRo: Romaji = Romaji(hiragana, katakana, romaji)

}

case class Katakana(val hiragana: String, val katakana: String, val romaji: String) extends Kana {
  override def extendVowel(s: String = this.katakana): String = s + "ー"
  override def extendConsonant(s: String = this.katakana): String = "ッ" + s

  override def toString = katakana

  override def toHi: Hiragana = Hiragana(hiragana, katakana, romaji)
  override def toKa: Katakana = this
  override def toRo: Romaji = Romaji(hiragana, katakana, romaji)
}

case class Romaji(val hiragana: String, val katakana: String, val romaji: String) extends Kana {
  override def extendVowel(s: String = this.romaji): String = s.init + Map('a' -> 'ā', 'e' -> 'ē', 'i' -> 'ī', 'o' -> 'ō', 'u' -> 'ū').getOrElse(s.last, s.last)
  override def extendConsonant(s: String = this.romaji): String = if (!japaneseChar.isVowel(s.head)) s.head + s else s

  override def toString = romaji

  override def toHi: Hiragana = Hiragana(hiragana, katakana, romaji)
  override def toKa: Katakana = Katakana(hiragana, katakana, romaji)
  override def toRo: Romaji = this
}

case class NotKana(val hiragana:String, val katakana:String, val romaji: String) extends Kana{
  override def toHi = this
  override def toKa = this
  override def toRo = this

  override def extendVowel(s: String): String = hiragana
  override def extendConsonant(s: String): String = hiragana
}
*/