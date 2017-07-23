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

  val nonDiacritics = Set[LeKana](Hiragana("あ", "ア", "a"), Hiragana("か", "カ", "ka"), Hiragana("さ", "サ", "sa"), Hiragana("た", "タ", "ta"), Hiragana("な", "ナ", "na"), Hiragana("は", "八", "ha"), Hiragana("ま", "マ", "ma"), Hiragana("や", "ヤ", "ya"), Hiragana("ら", "ラ", "ra"), Hiragana("わ", "ワ", "wa")
    , Hiragana("い", "イ", "i"), Hiragana("き", "キ", "ki"), Hiragana("し", "シ", "shi"), Hiragana("ち", "チ", "chi"), Hiragana("に", "ニ", "ni"), Hiragana("ひ", "ヒ", "hi"), Hiragana("み", "ミ", "mi"), Hiragana("り", "リ", "ri")
    , Hiragana("う", "ワ", "u"), Hiragana("く", "ク", "ku"), Hiragana("す", "ス", "su"), Hiragana("つ", "シ", "tsu"), Hiragana("ぬ", "ヌ", "nu"), Hiragana("ふ", "フ", "fu"), Hiragana("む", "ム", "mu"), Hiragana("ゆ", "ユ", "yu"), Hiragana("る", "ル", "ru")
    , Hiragana("え", "エ", "e"), Hiragana("け", "ケ", "ke"), Hiragana("せ", "セ", "se"), Hiragana("て", "テ", "te"), Hiragana("ね", "ネ", "ne"), Hiragana("へ", "ヘ", "he"), Hiragana("め", "メ", "me"), Hiragana("れ", "レ", "re")
    , Hiragana("お", "オ", "o"), Hiragana("こ", "コ", "ko"), Hiragana("そ", "ソ", "so"), Hiragana("と", "ト", "to"), Hiragana("の", "ノ", "no"), Hiragana("ほ", "ホ", "ho"), Hiragana("も", "モ", "mo"), Hiragana("よ", "ヨ", "yo"), Hiragana("ろ", "ロ", "ro"), Hiragana("を", "ヲ", "wo")
    , Hiragana("ん", "ン", "n"))


  val diacritics = Set[LeKana](Hiragana("が", "ガ", "ga"), Hiragana("ざ", "ザ", "za"), Hiragana("だ", "ダ", "da"), Hiragana("ば", "バ", "ba"), Hiragana("ぱ", "パ", "pa")
    , Hiragana("ぎ", "ギ", "gi"), Hiragana("じ", "ジ", "ji"), Hiragana("ぢ", "ヂ", "ji"), Hiragana("び", "ビ", "bi"), Hiragana("ぴ", "ピ", "pi")
    , Hiragana("ぐ", "グ", "gu"), Hiragana("ず", "ズ", "zu"), Hiragana("づ", "ヅ", "dzu"), Hiragana("ぶ", "ブ", "bu"), Hiragana("ぷ", "プ", "pu")
    , Hiragana("げ", "ゲ", "ge"), Hiragana("ぜ", "ゼ", "ze"), Hiragana("で", "デ", "de"), Hiragana("べ", "ベ", "be"), Hiragana("ぺ", "ペ", "pe")
    , Hiragana("ご", "ゴ", "go"), Hiragana("ぞ", "ゾ", "zo"), Hiragana("ど", "ド", "do"), Hiragana("ぼ", "ボ", "bo"), Hiragana("ぽ", "ポ", "po"))

  val kana = nonDiacritics ++ diacritics

  val hiragana = kana.map(_.toHi)
  val katakana = kana.map(_.toKa)
  val romaji = kana.map(_.toRo)


  val yoonNonDiacritics = Set[LeKana](Hiragana("きゃ", "キャ", "kya"), Hiragana("しゃ", "シャ", "sha"), Hiragana("ちゃ", "チャ", "cha"), Hiragana("にゃ", "ニャ", "nya"), Hiragana("ひゃ", "ヒャ", "hya"), Hiragana("みゃ", "ミャ", "mya"), Hiragana("りゃ", "リャ", "rya")
    , Hiragana("きゅ", "キュ", "kyu"), Hiragana("しゅ", "シュ", "shu"), Hiragana("ちゅ", "チュ", "chu"), Hiragana("にゅ", "ニュ", "nyu"), Hiragana("ひゅ", "ヒュ", "hyu"), Hiragana("みゅ", "ミュ", "myu"), Hiragana("りゅ", "リュ", "ryu")
    , Hiragana("きょ", "キョ", "kyo"), Hiragana("しょ", "ショ", "sho"), Hiragana("ちょ", "チョ", "cho"), Hiragana("にょ", "ニョ", "nyo"), Hiragana("ひょ", "ヒョ", "hyo"), Hiragana("みょ", "ミョ", "myo"), Hiragana("りょ", "リョ", "ryo"))

  val yoonDiacritics = Set[LeKana](Hiragana("ぎゃ", "ギャ", "gya"), Hiragana("じゃ", "ジャ", "ja"), Hiragana("ぢゃ", "ヂャ", "ja"), Hiragana("びゃ", "ビャ", "bya")
    , Hiragana("ぎゅ", "ギュ", "gyu"), Hiragana("じゅ", "ジュ", "ju"), Hiragana("ぢゅ", "ヂュ", "ju"), Hiragana("びゅ", "ビュ", "byu")
    , Hiragana("ぎょ", "ギョ", "gyo"), Hiragana("じょ", "ジョ", "jo"), Hiragana("ぢょ", "ヂョ", "jo"), Hiragana("びょ", "ビョ", "byo"))

  val yoon: Set[LeKana] = yoonDiacritics ++ yoonNonDiacritics

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

  //untested
  def nextSyllable(s: String): (LeKana,String) = {
    def featuresByLength(k: LeKana) = List(k.toHi.toString, k.toKa.toString, k.toRo.toString, k.toHi.extendVowel(), k.toHi.extendConsonant(), k.toHi.extendVowelAndConsonant(), k.toKa.extendVowel(), k.toKa.extendConsonant(), k.toKa.extendVowelAndConsonant(),k.toRo.extendVowel(), k.toRo.extendConsonant(), k.toRo.extendVowelAndConsonant()).sortWith(_.length > _.length)
    val entries:Map[LeKana, List[String]] = allKana.map(k => (k -> featuresByLength(k))).toMap

    for (entry <- entries){
      for (index <- 0 to 11) {
        val element:String = entry._2(index)
        if (s.length >= element.length && s.take(element.length) == element){
          println(s.take(element.length) + " == " + element)
          return (entry._1, element)
        }
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
  def toRomaji(ks:List[(LeKana, String)]):String = ks.reverse.foldLeft("")((k,v) => k + transliterate(v, l => l.toRo))
  def toHiragana(ks:List[(LeKana, String)]):String = ks.reverse.foldLeft("")((k,v) => k + transliterate(v, l => l.toHi))
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

/*
package sjt

import JapaneseInstances._
import JapaneseSyntax._

sealed trait LeKana {
  def toHi: LeKana
  def toKa: LeKana
  def toRo: LeKana

  def extendVowel(s: String = this.toString): LeKana
  def extendConsonant(s: String = this.toString): LeKana

  def extendVowelAndConsonant(s: String = this.toString):LeKana = extendVowel(s).extendConsonant()
  def ==(that: String) = this.toString == that

  def extendHiraganaVowel(s: String = this.toString): String = if (!(s.length == 1 && japaneseChar.isVowel(s.head))) s + "う" else s
  def extendHiraganaConsonant(s: String= this.toString): String = "っ" + s

  def extendKatakanaVowel(s: String = this.toString): String = if (!(s.length == 1 && japaneseChar.isVowel(s.head))) s + "ー" else s
  def extendKatakanaConsonant(s: String= this.toString): String = if (!japaneseChar.isVowel(s.head)) "ッ" + s else s

  def extendRomajiVowel(s: String = this.toString): String = s.init + Map('a' -> 'ā', 'e' -> 'ē', 'i' -> 'ī', 'o' -> 'ō', 'u' -> 'ū').getOrElse(s.last, s.last)
  def extendRomajiConsonant(s: String= this.toString): String = if (!japaneseChar.isVowel(s.head)) s.head + s else s

  /*
    def apply(hiragana: String, katakana: String, romaji: String):LeKana = this match {
      case Hiragana(hiragana, katakana, romaji) => new Hiragana(hiragana, katakana, romaji)
      case Katakana(hiragana, katakana, romaji) => new Katakana(hiragana, katakana, romaji)
      case Romaji(hiragana, katakana, romaji) => new Romaji(hiragana, katakana, romaji)
    }
  */

}

object LeKana {

  import LeKana._

  val nonDiacritics = Set[LeKana](Hiragana("あ", "ア", "a"), Hiragana("か", "カ", "ka"), Hiragana("さ", "サ", "sa"), Hiragana("た", "タ", "ta"), Hiragana("な", "ナ", "na"), Hiragana("は", "八", "ha"), Hiragana("ま", "マ", "ma"), Hiragana("や", "ヤ", "ya"), Hiragana("ら", "ラ", "ra"), Hiragana("わ", "ワ", "wa")
    , Hiragana("い", "イ", "i"), Hiragana("き", "キ", "ki"), Hiragana("し", "シ", "shi"), Hiragana("ち", "チ", "chi"), Hiragana("に", "ニ", "ni"), Hiragana("ひ", "ヒ", "hi"), Hiragana("み", "ミ", "mi"), Hiragana("り", "リ", "ri")
    , Hiragana("う", "ワ", "u"), Hiragana("く", "ク", "ku"), Hiragana("す", "ス", "su"), Hiragana("つ", "シ", "tsu"), Hiragana("ぬ", "ヌ", "nu"), Hiragana("ふ", "フ", "fu"), Hiragana("む", "ム", "mu"), Hiragana("ゆ", "ユ", "yu"), Hiragana("る", "ル", "ru")
    , Hiragana("え", "エ", "e"), Hiragana("け", "ケ", "ke"), Hiragana("せ", "セ", "se"), Hiragana("て", "テ", "te"), Hiragana("ね", "ネ", "ne"), Hiragana("へ", "ヘ", "he"), Hiragana("め", "メ", "me"), Hiragana("れ", "レ", "re")
    , Hiragana("お", "オ", "o"), Hiragana("こ", "コ", "ko"), Hiragana("そ", "ソ", "so"), Hiragana("と", "ト", "to"), Hiragana("の", "ノ", "no"), Hiragana("ほ", "ホ", "ho"), Hiragana("も", "モ", "mo"), Hiragana("よ", "ヨ", "yo"), Hiragana("ろ", "ロ", "ro"), Hiragana("を", "ヲ", "wo")
    , Hiragana("ん", "ン", "n"))


  val diacritics = Set[LeKana](Hiragana("が", "ガ", "ga"), Hiragana("ざ", "ザ", "sa"), Hiragana("だ", "ダ", "da"), Hiragana("ば", "バ", "ba"), Hiragana("ぱ", "パ", "pa")
    , Hiragana("ぎ", "ギ", "gi"), Hiragana("じ", "ジ", "ji"), Hiragana("ぢ", "ヂ", "ji"), Hiragana("び", "ビ", "bi"), Hiragana("ぴ", "ピ", "pi")
    , Hiragana("ぐ", "グ", "gu"), Hiragana("ず", "ズ", "zu"), Hiragana("づ", "ヅ", "dzu"), Hiragana("ぶ", "ブ", "bu"), Hiragana("ぷ", "プ", "pu")
    , Hiragana("げ", "ゲ", "ge"), Hiragana("ぜ", "ゼ", "ze"), Hiragana("で", "デ", "de"), Hiragana("べ", "ベ", "be"), Hiragana("ぺ", "ペ", "pe")
    , Hiragana("ご", "ゴ", "go"), Hiragana("ぞ", "ゾ", "zo"), Hiragana("ど", "ド", "do"), Hiragana("ぼ", "ボ", "bo"), Hiragana("ぽ", "ポ", "po"))

  val kana = nonDiacritics ++ diacritics

  val hiragana = kana.map(_.toHi)
  val katakana = kana.map(_.toKa)
  val romaji = kana.map(_.toRo)


  val yoonNonDiacritics = Set[LeKana](Hiragana("きゃ", "キャ", "kya"), Hiragana("しゃ", "シャ", "sha"), Hiragana("ちゃ", "チャ", "cha"), Hiragana("にゃ", "ニャ", "nya"), Hiragana("ひゃ", "ヒャ", "hya"), Hiragana("みゃ", "ミャ", "mya"), Hiragana("りゃ", "リャ", "rya")
    , Hiragana("きゅ", "キュ", "kyu"), Hiragana("しゅ", "シュ", "shu"), Hiragana("ちゅ", "チュ", "chu"), Hiragana("にゅ", "ニュ", "nyu"), Hiragana("ひゅ", "ヒュ", "hyu"), Hiragana("みゅ", "ミュ", "myu"), Hiragana("りゅ", "リュ", "ryu")
    , Hiragana("きょ", "キョ", "kyo"), Hiragana("しょ", "ショ", "sho"), Hiragana("ちょ", "チョ", "cho"), Hiragana("にょ", "ニョ", "nyo"), Hiragana("ひょ", "ヒョ", "hyo"), Hiragana("みょ", "ミョ", "myo"), Hiragana("りょ", "リョ", "ryo"))

  val yoonDiacritics = Set[LeKana](Hiragana("ぎゃ", "ギャ", "gya"), Hiragana("じゃ", "ジャ", "ja"), Hiragana("ぢゃ", "ヂャ", "ja"), Hiragana("びゃ", "ビャ", "bya")
    , Hiragana("ぎゅ", "ギュ", "gyu"), Hiragana("じゅ", "ジュ", "ju"), Hiragana("ぢゅ", "ヂュ", "ju"), Hiragana("びゅ", "ビュ", "byu")
    , Hiragana("ぎょ", "ギョ", "gyo"), Hiragana("じょ", "ジョ", "jo"), Hiragana("ぢょ", "ヂョ", "jo"), Hiragana("びょ", "ビョ", "byo"))

  val yoon: Set[LeKana] = yoonDiacritics ++ yoonNonDiacritics

  val allKana: Set[LeKana] = kana ++ yoon
  val allHiragana: Set[LeKana] = allKana.map(_.toHi)
  val allKatakana: Set[LeKana] = allKana.map(_.toKa)
  val allRomaji: Set[LeKana] = allKana.map(_.toRo)

  val allKanaStr: Set[String] = allKana.map(_.toString)
  val allHiraganaStr: Set[String] = allHiragana.map(_.toString)
  val allKatakanaStr: Set[String] = allKatakana.map(_.toString)
  val allRomajiStr: Set[String] = allRomaji.map(_.toString)

  val allHiraganaEV: Set[LeKana] = allHiragana.map(_.extendVowel())
  val allKatakanaEV: Set[LeKana] = allKatakana.map(_.extendVowel())
  val allRomajiEV: Set[LeKana] = allRomaji.map(_.extendVowel())
  val allKanaEV: Set[LeKana] = allHiraganaEV ++ allKatakanaEV ++ allRomajiEV

  val allHiraganaEC: Set[LeKana] = allHiragana.map(_.extendConsonant())
  val allKatakanaEC: Set[LeKana] = allKatakana.map(_.extendConsonant())
  val allRomajiEC: Set[LeKana] = allRomaji.map(_.extendConsonant())
  val allKanaEC: Set[LeKana] = allHiraganaEC ++ allKatakanaEC ++ allRomajiEC

  val allHiraganaECEV: Set[LeKana] = allHiragana.map(_.extendVowelAndConsonant())
  val allKatakanaECEV: Set[LeKana] = allKatakana.map(_.extendVowelAndConsonant())
  val allRomajiECEV: Set[LeKana] = allRomaji.map(_.extendVowelAndConsonant())
  val allKanaECEV: Set[LeKana] = allHiraganaECEV ++ allKatakanaECEV ++ allRomajiECEV

  val everyHiraganaSyllable: Set[LeKana] = allHiragana ++ allHiraganaEC ++ allHiraganaEV ++ allHiraganaECEV
  val everyKatakanaSyllable: Set[LeKana] = allKatakana ++ allKatakanaEC ++ allKatakanaEV ++ allKatakanaECEV
  val everyRomajiSyllable: Set[LeKana] = allRomaji ++ allRomajiEC ++ allRomajiEV ++ allRomajiECEV
  val everyKanaSyllable: Set[LeKana] = everyRomajiSyllable ++ everyKatakanaSyllable ++ everyHiraganaSyllable

  val everyHiraganaSyllableStr: Set[String] = everyHiraganaSyllable.map(_.toString)
  val everyKatakanaSyllableStr: Set[String] = everyKatakanaSyllable.map(_.toString)
  val everyRomajiSyllableStr: Set[String] = everyRomajiSyllable.map(_.toString)
  val everyKanaSyllableStr: Set[String] = everyKanaSyllable.map(_.toString)

  //untested
  def nextSyllable(s: String): LeKana = {
    def featuresByLength(k: LeKana) = List(k.toHi.toString, k.toKa.toString, k.toRo.toString, k.extendVowel().toString, k.extendConsonant().toString, k.extendVowelAndConsonant().toString).sortWith(_.length > _.length)
    val entries:Map[LeKana, List[String]] = allKana.map(k => (k -> featuresByLength(k))).toMap

    for (entry <- entries){
      for (index <- 0 to 5) {
        val element:String = entry._2(index)
        if (s.length >= element.length && s.take(element.length) == element){
          println(s.take(element.length) + " == " + element)
          println("Index " + index)
          println("entry " + entry)
          index match{
            case 0 => return entry._1.toHi
            case 1 => return entry._1.toKa
            case 2 => return entry._1.toRo
            case 3 => return entry._1.extendVowel()
            case 4 => return entry._1.extendConsonant()
            case 5 => return entry._1.extendVowelAndConsonant()
          }

        }
      }
    }
    NotKana(s,s,s)
  }


}

case class Hiragana(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {

  def unextendVowel(s:String = this.hiragana):String = if (!(s.length == 1 && japaneseChar.isVowel(s.head))) s.init else s
  def unextendConsonant(s:String = this.hiragana): String = if (!japaneseChar.isVowel(s.head)) s.tail else s

  override def toString = hiragana

  override def toHi: Hiragana = this
  override def toKa: Katakana = Katakana(hiragana, katakana, romaji)
  override def toRo: Romaji = Romaji(hiragana, katakana, romaji)

  override def extendVowel(s: String = hiragana): LeKana = HiraganaEV(extendHiraganaVowel(s), extendKatakanaVowel(katakana),extendRomajiVowel(romaji))
  override def extendConsonant(s: String = hiragana): LeKana = HiraganaEC(extendHiraganaConsonant(s), extendKatakanaConsonant(katakana),extendRomajiConsonant(romaji))

  //override def extendVowelAndConsonant(s: String = hiragana): LeKana = extendVowel(s).extendConsonant()
}

case class Katakana(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {

  def unextendVowel(s:String = this.hiragana):String = if (!(s.length == 1 && japaneseChar.isVowel(s.head))) s.init else s
  def unextendConsonant(s:String = this.hiragana): String = if (!japaneseChar.isVowel(s.head)) s.tail else s

  override def toString = katakana

  override def toHi: Hiragana = Hiragana(hiragana, katakana, romaji)
  override def toKa: Katakana = this
  override def toRo: Romaji = Romaji(hiragana, katakana, romaji)

  override def extendVowel(s: String = katakana): LeKana = KatakanaEV(extendHiraganaVowel(hiragana), extendKatakanaVowel(s),extendRomajiVowel(romaji))
  override def extendConsonant(s: String = katakana): LeKana = KatakanaEC(extendHiraganaConsonant(hiragana), extendKatakanaConsonant(s),extendRomajiConsonant(romaji))
}

case class Romaji(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {

  def unextendVowel(s: String = this.romaji): String = s.init + Map('ā' -> 'a', 'ē' -> 'e', 'ī' -> 'i', 'ō' -> 'o', 'ū' -> 'u').getOrElse(s.last, s.last)
  def unextendConsonant(s:String = this.hiragana): String = if (s.length >= 2 && !japaneseChar.isVowel(s.head) && s.head == s(1)) s.tail else s

  override def toString = romaji

  override def toHi: Hiragana = Hiragana(hiragana, katakana, romaji)
  override def toKa: Katakana = Katakana(hiragana, katakana, romaji)
  override def toRo: Romaji = this

  override def extendVowel(s: String = romaji): LeKana = RomajiEV(extendHiraganaVowel(hiragana), extendKatakanaVowel(katakana),extendRomajiVowel(s))
  override def extendConsonant(s: String = romaji): LeKana = RomajiEC(extendHiraganaConsonant(hiragana), extendKatakanaConsonant(katakana),extendRomajiConsonant(s))
}
case class NotKana(val hiragana:String, val katakana:String, val romaji: String) extends LeKana{
  override def toHi = this
  override def toKa = this
  override def toRo = this

  override def extendVowel(s: String): NotKana = this
  override def extendConsonant(s: String): NotKana = this
}
case class HiraganaEV(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {
  override def toString = hiragana
  override def toHi: HiraganaEV = this
  override def toKa: KatakanaEV = KatakanaEV(hiragana, katakana, romaji)
  override def toRo: RomajiEV = RomajiEV(hiragana, katakana, romaji)

  override def extendVowel(s: String = hiragana): LeKana = this
  override def extendConsonant(s: String = hiragana): LeKana = HiraganaEVEC(extendHiraganaConsonant(s), extendKatakanaConsonant(katakana),extendRomajiConsonant(romaji))
}
case class KatakanaEV(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {
  override def toHi: HiraganaEV = HiraganaEV(hiragana, katakana, romaji)
  override def toKa: KatakanaEV = this
  override def toRo: RomajiEV = RomajiEV(hiragana, katakana, romaji)

  override def extendVowel(s: String = hiragana): LeKana = this
  override def extendConsonant(s: String = hiragana): LeKana = KatakanaEVEC(extendHiraganaConsonant(hiragana), extendKatakanaConsonant(s),extendRomajiConsonant(romaji))
}
case class RomajiEV(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {
  override def toHi: HiraganaEV = HiraganaEV(hiragana, katakana, romaji)
  override def toKa: KatakanaEV = KatakanaEV(hiragana, katakana, romaji)
  override def toRo: RomajiEV = this

  override def extendVowel(s: String = hiragana): LeKana = this
  override def extendConsonant(s: String = hiragana): LeKana = RomajiEVEC(extendHiraganaConsonant(hiragana), extendKatakanaConsonant(katakana),extendRomajiConsonant(s))
}
case class HiraganaEC(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {
  override def toHi: HiraganaEC = this
  override def toKa: KatakanaEC = KatakanaEC(hiragana, katakana, romaji)
  override def toRo: RomajiEC = RomajiEC(hiragana, katakana, romaji)

  override def extendVowel(s: String = hiragana): LeKana = HiraganaEVEC(extendHiraganaVowel(s), extendKatakanaVowel(katakana),extendRomajiVowel(romaji))
  override def extendConsonant(s: String = hiragana): LeKana = this
}
case class KatakanaEC(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {
  override def toHi: HiraganaEC = HiraganaEC(hiragana, katakana, romaji)
  override def toKa: KatakanaEC = this
  override def toRo: RomajiEC = RomajiEC(hiragana, katakana, romaji)

  override def extendVowel(s: String = hiragana): LeKana = KatakanaEVEC(extendHiraganaVowel(hiragana), extendKatakanaVowel(s),extendRomajiVowel(romaji))
  override def extendConsonant(s: String = hiragana): LeKana = this
}
case class RomajiEC(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {
  override def toHi: HiraganaEC = HiraganaEC(hiragana, katakana, romaji)
  override def toKa: KatakanaEC = KatakanaEC(hiragana, katakana, romaji)
  override def toRo: RomajiEC = this

  override def extendVowel(s: String = hiragana): LeKana = RomajiEVEC(extendHiraganaVowel(hiragana), extendKatakanaVowel(katakana),extendRomajiVowel(s))
  override def extendConsonant(s: String = hiragana): LeKana = this
}
case class HiraganaEVEC(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {
  override def toHi: HiraganaEVEC = this
  override def toKa: KatakanaEVEC = KatakanaEVEC(hiragana, katakana, romaji)
  override def toRo: RomajiEVEC = RomajiEVEC(hiragana, katakana, romaji)

  override def extendVowel(s: String = hiragana): LeKana = this
  override def extendConsonant(s: String = hiragana): LeKana = this
}
case class KatakanaEVEC(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {
  override def toHi: HiraganaEVEC = HiraganaEVEC(hiragana, katakana, romaji)
  override def toKa: KatakanaEVEC= this
  override def toRo: RomajiEVEC = RomajiEVEC(hiragana, katakana, romaji)

  override def extendVowel(s: String = hiragana): LeKana = this
  override def extendConsonant(s: String = hiragana): LeKana = this
}
case class RomajiEVEC(val hiragana: String, val katakana: String, val romaji: String) extends LeKana {
  override def toHi: HiraganaEVEC = HiraganaEVEC(hiragana, katakana, romaji)
  override def toKa: KatakanaEVEC = KatakanaEVEC(hiragana, katakana, romaji)
  override def toRo: RomajiEVEC = this

  override def extendVowel(s: String = hiragana): LeKana = this
  override def extendConsonant(s: String = hiragana): LeKana = this
}
 */