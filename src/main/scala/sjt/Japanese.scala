package sjt

import java.nio.charset.Charset

import com.atilika.kuromoji.ipadic.Token
import com.atilika.kuromoji.ipadic.Tokenizer

import collection.JavaConverters._
import scala.annotation.tailrec

trait Japanese[A] {
  def isHiragana(value: A): Boolean
  def isHalfWidthKatakana(value: A):Boolean
  def isFullWidthKatakana(value: A):Boolean
  def isKatakana(value: A): Boolean = isFullWidthKatakana(value) || isHalfWidthKatakana(value)
  def isKana(value: A): Boolean = isHiragana(value) || isKatakana(value)
  def isKanji(value: A): Boolean
  def containsHiragana(value: A): Boolean
  def containsKatakana(value: A): Boolean
  def containsKana(value: A): Boolean = containsHiragana(value) || containsKatakana(value)
  def containsKanji(value: A): Boolean
  def extractKanji(value: A):List[Char]
  def containsJapanese(value: A): Boolean = containsHiragana(value) || containsKatakana(value)
  def isLatin(value: A): Boolean
  def toRomaji(value: A, tokenizer:Option[Tokenizer] = None): String
  def toKatakana(value: A, tokenizer:Option[Tokenizer] = None): String
  def toHiragana(value: A, tokenizer:Option[Tokenizer] = None): String
  def splitIntoSyllables(input:A, l: List[(Kana, String)] = Nil): List[(Kana,String)]
}

object JapaneseInstances{

  implicit val japaneseChar = new Japanese[Char] {
    def isHiragana(value: Char): Boolean = ('\u3041' <= value) && (value <= '\u309e') || isExtension(value)
    def isHalfWidthKatakana(value: Char):Boolean = ('\uff66' <= value) && (value <= '\uff9d')
    def isFullWidthKatakana(value: Char):Boolean = ('\u30a1' <= value) && (value <= '\u30fe')
    def isKanji(value: Char): Boolean = (('\u4e00' <= value) && (value <= '\u9fa5')) || (('\u3005' <= value) && (value <= '\u3007'))
    def extractKanji(value:Char):List[Char] = if (isKanji(value)) List[Char](value) else Nil
    def containsHiragana(value: Char): Boolean = isHiragana(value)
    def containsKatakana(value: Char): Boolean = isKatakana(value)
    def containsKanji(value: Char): Boolean = isKanji(value) //replace for a call to extract kanji?
    def isLatin(value: Char): Boolean = Charset.forName("US-ASCII").newEncoder().canEncode(value)

    def isVowel(value:Char):Boolean = isLatinVowel(value) || isHiraganaVowel(value) || isKatakanaVowel(value) || isKatakanaMiniVowel(value)
    def isLatinVowel(value:Char):Boolean = value == 'a' ||value == 'e' ||value == 'i' || value == 'o' || value == 'u'
    def isHiraganaVowel(value:Char):Boolean = value == 'あ' ||value == 'え' ||value == 'い' || value == 'お' || value == 'う'
    def isKatakanaVowel(value:Char):Boolean = value == 'ア' ||value == 'エ' ||value == 'イ' || value == 'オ' || value == 'ウ'
    def isKatakanaMiniVowel(value:Char):Boolean = value == 'ェ' || value == 'ョ'

    def extendChar(value:Char):Char = Map('a' -> 'ā', 'e' -> 'ē', 'i' -> 'ī', 'o'-> 'ō', 'u' -> 'ū').get(value).getOrElse(value)
    def isExtension(value:Char):Boolean = Map('ゃ' -> 'a', 'ゅ'->'u', 'ょ'->'o', 'ャ' -> 'a', 'ュ' -> 'u', 'ョ' -> 'o').get(value).isDefined

    override def toRomaji(value: Char, tokenizer:Option[Tokenizer] = None): String = Kana.toRomaji(splitIntoSyllables(value))
    override def toKatakana(value: Char, tokenizer:Option[Tokenizer] = None): String = if (value == 'っ') "ッ" else Kana.toKatakana(splitIntoSyllables(value))
    override def toHiragana(value: Char, tokenizer:Option[Tokenizer] = None): String = if (value == 'ッ') "っ" else Kana.toHiragana(splitIntoSyllables(value))

    override def splitIntoSyllables(input: Char, l: List[(Kana, String)]): List[(Kana, String)] =  List(Kana.nextSyllable(input.toString))
  }

  implicit val japaneseString = new Japanese[String] {

    def isHiragana(value: String): Boolean = value.toCharArray.forall(japaneseChar.isHiragana)
    def isHalfWidthKatakana(value: String):Boolean = value.toCharArray.forall(japaneseChar.isHalfWidthKatakana)
    def isFullWidthKatakana(value: String):Boolean = value.toCharArray.forall(japaneseChar.isFullWidthKatakana)
    def isKanji(value: String): Boolean = value.toCharArray.forall(japaneseChar.isKanji)
    def extractKanji(value:String):List[Char] = value.toCharArray.filter(c => japaneseChar.isKanji(c)).toList

    def containsHiragana(value: String): Boolean = value.toCharArray.exists(japaneseChar.containsHiragana)
    def containsKatakana(value: String): Boolean = value.toCharArray.exists(japaneseChar.containsKatakana)
    def containsKanji(value: String): Boolean = value.toCharArray.exists(japaneseChar.containsKanji)
    def isLatin(value: String): Boolean = value.toCharArray.forall(japaneseChar.isLatin)

    @tailrec
    override def splitIntoSyllables(input: String, l: List[(Kana, String)] = Nil): List[(Kana,String)] = {
      val nS = Kana.nextSyllable(input)
      if (input.isEmpty) l
      else splitIntoSyllables(input.drop(nS._2.length), nS :: l)
    }

    def toRomaji(value: String, tokenizer:Option[Tokenizer] = Some(Kana.tokenizer)):String = {
      if (!containsKanji(value)) Kana.toRomaji(splitIntoSyllables(value))
      else Kana.toRomaji(splitIntoSyllables(tokensToRomajiString(tokenizer.get.tokenize(value).asScala.toList)))
    }
    def toHiragana(value: String, tokenizer:Option[Tokenizer] = Some(Kana.tokenizer)):String = {
      if (!containsKanji(value)) Kana.toHiragana(splitIntoSyllables(value))
      else Kana.toHiragana(splitIntoSyllables(tokensToHiraganaString(tokenizer.get.tokenize(value).asScala.toList)))
    }
    def toKatakana(value: String, tokenizer:Option[Tokenizer] = Some(Kana.tokenizer)):String = {
      if (!containsKanji(value)) Kana.toKatakana(splitIntoSyllables(value))
      else Kana.toKatakana(splitIntoSyllables(tokensToKatakanaString(tokenizer.get.tokenize(value).asScala.toList))).tail
    }

    private def hiraganaSpacing(t:Token) = ""
    private def katakanaSpacing(t:Token) = if(Kana.isTranslateableSymbol(t.getSurface())) "" else "・"
    private def romajiSpacing(t:Token) = if(t.getAllFeaturesArray()(1) == "接続助詞" || Kana.isTranslateableSymbol(t.getSurface())) "" else " "

    private def tokensToRomajiString(ts:List[Token]) = ts.foldLeft(""){(r,t:Token) => r + romajiSpacing(t) +  (if (t.getPronunciation != "*") t.getPronunciation else t.getReading)}.trim
    private def tokensToHiraganaString(ts:List[Token]) = ts.foldLeft(""){(r,t:Token) => r + hiraganaSpacing(t) +  t.getReading}.trim
    private def tokensToKatakanaString(ts:List[Token]) = ts.foldLeft(""){(r,t:Token) => r + katakanaSpacing(t) +  (if (t.getPronunciation != "*") t.getPronunciation else t.getReading)}.trim

  }
  implicit val kuromojiToken = new Japanese[Token] {
    def isHiragana(value: Token): Boolean = japaneseString.isHiragana(value.getSurface)
    def isHalfWidthKatakana(value: Token):Boolean = japaneseString.isHalfWidthKatakana(value.getSurface)
    def isFullWidthKatakana(value: Token):Boolean = japaneseString.isFullWidthKatakana(value.getSurface)
    def isKanji(value: Token): Boolean = japaneseString.isKanji(value.getSurface)
    def extractKanji(value:Token):List[Char] = japaneseString.extractKanji(value.getSurface)

    def containsHiragana(value: Token): Boolean = japaneseString.containsHiragana(value.getSurface)
    def containsKatakana(value: Token): Boolean = japaneseString.containsKatakana(value.getSurface)
    def containsKanji(value: Token): Boolean = japaneseString.containsKanji(value.getSurface)
    def isLatin(value: Token): Boolean = japaneseString.isLatin(value.getSurface)
    def toRomaji(value: Token, tokenizer:Option[Tokenizer] = None): String = Kana.toRomaji(splitIntoSyllables(value))
    def toKatakana(value: Token, tokenizer:Option[Tokenizer] = None): String = Kana.toKatakana(splitIntoSyllables(value))
    def toHiragana(value: Token, tokenizer:Option[Tokenizer] = None): String = Kana.toHiragana(splitIntoSyllables(value))

    override def splitIntoSyllables(input: Token, l: List[(Kana, String)]): List[(Kana, String)] = japaneseString.splitIntoSyllables(if (input.getPronunciation != "*") input.getPronunciation else input.getSurface)
  }
}
object Japanese {
  def isHiragana[A](input: A)(implicit p: Japanese[A]): Boolean = p.isHiragana(input)
  def isHalfWidthKatakana[A](input: A)(implicit p: Japanese[A]):Boolean = p.isHalfWidthKatakana(input)
  def isFullWidthKatakana[A](input: A)(implicit p: Japanese[A]):Boolean = p.isFullWidthKatakana(input)
  def isKatakana[A](input: A)(implicit p: Japanese[A]): Boolean  = p.isKatakana(input)
  def isKana[A](input: A)(implicit p: Japanese[A]): Boolean = p.isKana(input)
  def isKanji[A](input: A)(implicit p: Japanese[A]): Boolean = p.isKanji(input)
  def extractKanji[A](input: A)(implicit p: Japanese[A]): List[Char] = p.extractKanji(input)

  def containsHiragana[A](input: A)(implicit p: Japanese[A]): Boolean = p.containsHiragana(input)
  def containsKatakana[A](input: A)(implicit p: Japanese[A]): Boolean = p.containsKatakana(input)
  def containsKana[A](input: A)(implicit p: Japanese[A]): Boolean = p.containsKana(input)
  def containsKanji[A](input: A)(implicit p: Japanese[A]): Boolean =  p.containsKanji(input)
  def containsJapanese[A](input: A)(implicit p: Japanese[A]): Boolean  = p.containsJapanese(input)
  def isLatin[A](input: A)(implicit p: Japanese[A]): Boolean = p.isLatin(input)
  def toRomaji[A](input: A)(implicit p: Japanese[A]): String = p.toRomaji(input)
  def toKatakana[A](input:A)(implicit p: Japanese[A]): String = p.toKatakana(input)
  def toHiragana[A](input:A)(implicit p: Japanese[A]): String = p.toHiragana(input)
  def splitIntoSyllables[A](input:A)(implicit p: Japanese[A]):List[(Kana, String)] = p.splitIntoSyllables(input)
}

object JapaneseSyntax {
  implicit class JapaneseOps[A](value: A) {
    def isHiragana(implicit p: Japanese[A]): Boolean = p.isHiragana(value)
    def isHalfWidthKatakana(implicit p: Japanese[A]):Boolean = p.isHalfWidthKatakana(value)
    def isFullWidthKatakana(implicit p: Japanese[A]):Boolean = p.isFullWidthKatakana(value)
    def isKatakana(implicit p: Japanese[A]): Boolean  = p.isKatakana(value)
    def isKana(implicit p: Japanese[A]): Boolean = p.isKana(value)
    def isKanji(implicit p: Japanese[A]): Boolean = p.isKanji(value)
    def extractKanji(implicit p: Japanese[A]): List[Char] = p.extractKanji(value)
    def containsHiragana(implicit p: Japanese[A]): Boolean = p.containsHiragana(value)
    def containsKatakana(implicit p: Japanese[A]): Boolean = p.containsKatakana(value)
    def containsKana(implicit p: Japanese[A]): Boolean = p.containsKana(value)
    def containsKanji(implicit p: Japanese[A]): Boolean =  p.containsKanji(value)
    def containsJapanese(implicit p: Japanese[A]): Boolean  = p.containsJapanese(value)
    def isLatin(implicit p: Japanese[A]): Boolean = p.isLatin(value)
    //Weird but necessary use of null. Have to check if its defined either way let each instance use its default so it reduces boilerplate
    def toRomaji(t:Tokenizer = null)(implicit p: Japanese[A]): String = if (t != null) p.toRomaji(value, Some(t)) else p.toRomaji(value)
    def toKatakana(t:Tokenizer = null)(implicit p: Japanese[A]): String = if (t != null) p.toKatakana(value, Some(t)) else p.toKatakana(value)
    def toHiragana(t:Tokenizer = null)(implicit p: Japanese[A]): String = if (t != null) p.toHiragana(value, Some(t)) else p.toHiragana(value)
    def splitIntoSyllables(implicit p: Japanese[A]):List[(Kana, String)] = p.splitIntoSyllables(value)
  }
}
sealed trait SpacingConfig{
  def apply(t:Token):String = this match {
    case HiraganaSpacing() => ""
    case KatakanaSpacing() => if(Kana.isTranslateableSymbol(t.getSurface())) "" else "・"
    case RomajiSpacing() => if(t.getAllFeaturesArray()(1) == "接続助詞" || Kana.isTranslateableSymbol(t.getSurface())) "" else " "
  }
}
final case class HiraganaSpacing() extends SpacingConfig()
final case class KatakanaSpacing() extends SpacingConfig()
final case class RomajiSpacing() extends SpacingConfig()

object Main {
  def main(args: Array[String]): Unit = {
    import JapaneseInstances._
    import JapaneseSyntax._
    //TODO: Refactor the Hiragan/Katankana/Romaji clases to simply be Kana
    //TODO: Make function to get all kana transliterations at the same time & implement all transliterations in terms of that
    //TODO: Rename Kana fields and privitize them if necessary
    //TODO: Add extractHiragana + extractKatakana + extractKana methods to SJT
    //TODO: Add extractKanjiReading (?)

/*


    val t1 = System.currentTimeMillis()
    val a = Kana.diacritics
    val b = Kana.yoonNonDiacritics
    val leTO1 = Kana.tokenizer
    val t2 = System.currentTimeMillis()
    val deltaT = t2-t1
    println(s"Δt = $deltaT")

    val t3 = System.currentTimeMillis()
    val leTO2 = Kana.tokenizer
    val t4 = System.currentTimeMillis()
    val deltaT2 = t4-t3
    println(s"Δt = $deltaT2")


    val t5 = System.currentTimeMillis()
    println("毎日私は午前十時に起きます。十時から十二まで勉強します。午後一時に昼ごはんを食べます。あとでまた勉強をします。六時に地下鉄で大学へ行きます。十一時に私の家へ帰ります。そして晩ご飯を食べます。次に少し仕事をします。プログラミンをします。難しいです。それでも、私は大好きです。なぜならとても楽しいですから。朝の五時にねます。".toRomaji())
    println("診察行ってきました。最終目標は心臓移植というのは変わらないそうで、自分の心臓じゃ生きられないみたいです。お金もかかるし、手術するのも入院するのももう嫌だな～…なんてことを考えながらvitaで朧村正をプレイしてました！　牛鬼と馬鬼ってボスより強くない！？　ぜんぜん勝てないんだけど…".toRomaji())
    println("「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる".toRomaji())
    println("皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。".toRomaji())
    val t6 = System.currentTimeMillis()
    val deltaT3 = t6-t5
    println(s"Δt = $deltaT3")


    val t7 = System.currentTimeMillis()
    println("毎日私は午前十時に起きます。十時から十二まで勉強します。午後一時に昼ごはんを食べます。あとでまた勉強をします。六時に地下鉄で大学へ行きます。十一時に私の家へ帰ります。そして晩ご飯を食べます。次に少し仕事をします。プログラミンをします。難しいです。それでも、私は大好きです。なぜならとても楽しいですから。朝の五時にねます。".toRomaji())
    println("診察行ってきました。最終目標は心臓移植というのは変わらないそうで、自分の心臓じゃ生きられないみたいです。お金もかかるし、手術するのも入院するのももう嫌だな～…なんてことを考えながらvitaで朧村正をプレイしてました！　牛鬼と馬鬼ってボスより強くない！？　ぜんぜん勝てないんだけど…".toRomaji())
    println("「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる".toRomaji())
    println("皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。".toRomaji())
    val t8 = System.currentTimeMillis()
    val deltaT4 = t8-t7
    println(s"Δt = $deltaT4")

    val t9 = System.currentTimeMillis()
    println("毎日私は午前十時に起きます。十時から十二まで勉強します。午後一時に昼ごはんを食べます。あとでまた勉強をします。六時に地下鉄で大学へ行きます。十一時に私の家へ帰ります。そして晩ご飯を食べます。次に少し仕事をします。プログラミンをします。難しいです。それでも、私は大好きです。なぜならとても楽しいですから。朝の五時にねます。".toRomaji(leTO1))
    println("診察行ってきました。最終目標は心臓移植というのは変わらないそうで、自分の心臓じゃ生きられないみたいです。お金もかかるし、手術するのも入院するのももう嫌だな～…なんてことを考えながらvitaで朧村正をプレイしてました！　牛鬼と馬鬼ってボスより強くない！？　ぜんぜん勝てないんだけど…".toRomaji(leTO1))
    println("「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる".toRomaji(leTO1))
    println("皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。".toRomaji(leTO1))
    val t10 = System.currentTimeMillis()
    val deltaT5 = t10-t9
    println(s"Δt = $deltaT5")
*/

  }
}