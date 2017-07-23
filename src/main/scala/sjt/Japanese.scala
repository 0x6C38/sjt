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
  def containsJapanese(value: A): Boolean = containsHiragana(value) || containsKatakana(value)
  def isLatin(value: A): Boolean
  def toRomaji(value: A, tokenizer:Option[Tokenizer] = None): String
  def toKatakana(value: A, tokenizer:Option[Tokenizer] = None): String
  def toHiragana(value: A, tokenizer:Option[Tokenizer] = None): String
  //def splitIntoSyllables(value:A, l: List[Syllable] = Nil): List[Syllable]
  def splitIntoSyllables(input:A, l: List[(LeKana, String)] = Nil): List[(LeKana,String)]

}
object JapaneseInstances{
  /*
  implicit val japaneseSyllable = new Japanese[Syllable] {

    override def isHiragana(value: Syllable): Boolean = Kana.allHiraganaToRomajiM.keySet.contains(value.text)
    override def isHalfWidthKatakana(value: Syllable): Boolean = false //calculated by a fair dice roll
    override def isFullWidthKatakana(value: Syllable): Boolean = Kana.allKatakanaToRomajiM.keySet.contains(value.text)

    override def isKanji(value: Syllable): Boolean = value.text.toCharArray.forall(value => (('\u4e00' <= value) && (value <= '\u9fa5')) || (('\u3005' <= value) && (value <= '\u3007')))

    override def containsHiragana(value: Syllable): Boolean = value.text.exists(c => Kana.isHiragana(c))
    override def containsKatakana(value: Syllable): Boolean = value.text.exists(c => Kana.isKatakana(c))
    override def containsKanji(value: Syllable): Boolean = value.text.exists(value => (('\u4e00' <= value) && (value <= '\u9fa5')) || (('\u3005' <= value) && (value <= '\u3007')))

    override def isLatin(value: Syllable): Boolean = value.text.matches("[a-zA-Z].*")

    override def toRomaji(value: Syllable, tokenizer:Option[Tokenizer] = None): String = Syllable.kanaSilableToRomaji(value.text)
    override def toKatakana(value: Syllable, tokenizer:Option[Tokenizer] = None): String = Syllable.hiraganaOrRomajiToKatakana(value.text)
    override def toHiragana(value: Syllable, tokenizer:Option[Tokenizer] = None): String = Syllable.katakanaOrRomajiToHiragana(value.text)

    //override def splitIntoSyllables(value: Syllable, l: List[Syllable]): List[Syllable] = List(value)

    //override def splitIntoSyllables2(input: String, l: List[(LeKana, String)]): List[(LeKana, String)] = ???
    //def splitIntoSyllables(value:A, l: List[Syllable] = Nil): List[Syllable]
    override def splitIntoSyllables(input: Syllable, l: List[(LeKana, String)]): List[(LeKana, String)] = ???
  }
*/
  implicit val japaneseChar = new Japanese[Char] {
    def isHiragana(value: Char): Boolean = ('\u3041' <= value) && (value <= '\u309e') || isExtension(value)
    def isHalfWidthKatakana(value: Char):Boolean = ('\uff66' <= value) && (value <= '\uff9d')
    def isFullWidthKatakana(value: Char):Boolean = ('\u30a1' <= value) && (value <= '\u30fe')
    def isKanji(value: Char): Boolean = (('\u4e00' <= value) && (value <= '\u9fa5')) || (('\u3005' <= value) && (value <= '\u3007'))
    def containsHiragana(value: Char): Boolean = isHiragana(value)
    def containsKatakana(value: Char): Boolean = isKatakana(value)
    def containsKanji(value: Char): Boolean = isKanji(value)
    def isLatin(value: Char): Boolean = Charset.forName("US-ASCII").newEncoder().canEncode(value)

    def isVowel(value:Char):Boolean = isLatinVowel(value) || isHiraganaVowel(value) || isKatakanaVowel(value) || isKatakanaMiniVowel(value)
    def isLatinVowel(value:Char):Boolean = value == 'a' ||value == 'e' ||value == 'i' || value == 'o' || value == 'u'
    def isHiraganaVowel(value:Char):Boolean = value == 'あ' ||value == 'え' ||value == 'い' || value == 'お' || value == 'う'
    def isKatakanaVowel(value:Char):Boolean = value == 'ア' ||value == 'エ' ||value == 'イ' || value == 'オ' || value == 'ウ'
    def isKatakanaMiniVowel(value:Char):Boolean = value == 'ェ' || value == 'ョ'

    def extendChar(value:Char):Char = Map('a' -> 'ā', 'e' -> 'ē', 'i' -> 'ī', 'o'-> 'ō', 'u' -> 'ū').get(value).getOrElse(value)
    def isExtension(value:Char):Boolean = Map('ゃ' -> 'a', 'ゅ'->'u', 'ょ'->'o', 'ャ' -> 'a', 'ュ' -> 'u', 'ョ' -> 'o').get(value).isDefined

    override def toRomaji(value: Char, tokenizer:Option[Tokenizer] = None): String = LeKana.toRomaji(splitIntoSyllables(value))
    override def toKatakana(value: Char, tokenizer:Option[Tokenizer] = None): String = if (value == 'っ') "ッ" else LeKana.toKatakana(splitIntoSyllables(value))
    override def toHiragana(value: Char, tokenizer:Option[Tokenizer] = None): String = if (value == 'ッ') "っ" else LeKana.toHiragana(splitIntoSyllables(value))

    //override def splitIntoSyllables(value: Char, l: List[Syllable] = Nil): List[Syllable] = List(Syllable.nextSyllable(value.toString))

    override def splitIntoSyllables(input: Char, l: List[(LeKana, String)]): List[(LeKana, String)] =  List(LeKana.nextSyllable(input.toString))

  }

  implicit val japaneseString = new Japanese[String] {

    def isHiragana(value: String): Boolean = value.toCharArray.forall(japaneseChar.isHiragana)
    def isHalfWidthKatakana(value: String):Boolean = value.toCharArray.forall(japaneseChar.isHalfWidthKatakana)
    def isFullWidthKatakana(value: String):Boolean = value.toCharArray.forall(japaneseChar.isFullWidthKatakana)
    def isKanji(value: String): Boolean = value.toCharArray.forall(japaneseChar.isKanji)
    def containsHiragana(value: String): Boolean = value.toCharArray.exists(japaneseChar.containsHiragana)
    def containsKatakana(value: String): Boolean = value.toCharArray.exists(japaneseChar.containsKatakana)
    def containsKanji(value: String): Boolean = value.toCharArray.exists(japaneseChar.containsKanji)
    def isLatin(value: String): Boolean = value.toCharArray.forall(japaneseChar.isLatin)

    @tailrec
    override def splitIntoSyllables(input: String, l: List[(LeKana, String)] = Nil): List[(LeKana,String)] = {
      val nS = LeKana.nextSyllable(input)
      if (input.isEmpty) l
      else splitIntoSyllables(input.drop(nS._2.length), nS :: l)
    }

    def toRomaji(value: String, tokenizer:Option[Tokenizer] = Some(new Tokenizer())):String = {
      if (!containsKanji(value)) LeKana.toRomaji(splitIntoSyllables(value))
      else LeKana.toRomaji(splitIntoSyllables(tokensToRomajiString(tokenizer.get.tokenize(value).asScala.toList, RomajiSpacing())))
    }
    def toHiragana(value: String, tokenizer:Option[Tokenizer] = Some(new Tokenizer())):String = {
      if (!containsKanji(value)) LeKana.toHiragana(splitIntoSyllables(value))
      else LeKana.toHiragana(splitIntoSyllables(tokensToHiraganaString(tokenizer.get.tokenize(value).asScala.toList, HiraganaSpacing())))
    }
    def toKatakana(value: String, tokenizer:Option[Tokenizer] = Some(new Tokenizer())):String = {
      if (!containsKanji(value)) LeKana.toKatakana(splitIntoSyllables(value))
      else LeKana.toKatakana(splitIntoSyllables(tokensToKatakanaString(tokenizer.get.tokenize(value).asScala.toList, KatakanaSpacing()))).tail
    }

    /*
    def toRomaji(value: String, tokenizer:Option[Tokenizer] = Some(new Tokenizer())):String = {
      if (!containsKanji(value)) splitIntoSyllables(value).foldLeft("")((a,s) => a + japaneseSyllable.toRomaji(s))
      else splitIntoSyllables(tokensToRomajiString(tokenizer.get.tokenize(value).asScala.toList, RomajiSpacing())).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toRomaji(s)).trim
    }
    def toHiragana(value: String, tokenizer:Option[Tokenizer] = Some(new Tokenizer())):String = {
      if (!containsKanji(value)) splitIntoSyllables(value).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toHiragana(s))
      else splitIntoSyllables(tokensToHiraganaString(tokenizer.get.tokenize(value).asScala.toList, HiraganaSpacing())).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toHiragana(s)).trim
    }
    def toKatakana(value: String, tokenizer:Option[Tokenizer] = Some(new Tokenizer())):String = {
      if (!containsKanji(value)) splitIntoSyllables(value).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toKatakana(s))
      else splitIntoSyllables(tokensToKatakanaString(tokenizer.get.tokenize(value).asScala.toList, KatakanaSpacing())).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toKatakana(s)).tail
    }
    */

    private def tokensToRomajiString(ts:List[Token], s:SpacingConfig) = ts.foldLeft(""){(r,t:Token) => r + s(t) +  (if (t.getPronunciation != "*") t.getPronunciation else t.getReading)}.trim //prev: getSurface
    private def tokensToHiraganaString(ts:List[Token], s:SpacingConfig) = ts.foldLeft(""){(r,t:Token) => r + s(t) +  t.getReading}.trim
    private def tokensToKatakanaString(ts:List[Token], s:SpacingConfig) = ts.foldLeft(""){(r,t:Token) => r + s(t) +  (if (t.getPronunciation != "*") t.getPronunciation else t.getReading)}.trim


    /*
    @tailrec
    def splitIntoSyllables(input: String, l: List[(LeKana, String)] = Nil): List[(LeKana,String)] = {
      val nS = LeKana.nextSyllable(input)
      if (input.isEmpty) l
      else splitIntoSyllables(input.drop(nS._1.toString.length), nS :: l)
    }


    @tailrec
    override def splitIntoSyllables(input: String, l: List[Syllable] = Nil): List[Syllable] = {
      val nS = Syllable.nextSyllable(input)
      if (input.isEmpty) l
      else splitIntoSyllables(input.drop(nS.text.length), nS :: l)
    }
 */
  }
  implicit val kuromojiToken = new Japanese[Token] {
    def isHiragana(value: Token): Boolean = japaneseString.isHiragana(value.getSurface)
    def isHalfWidthKatakana(value: Token):Boolean = japaneseString.isHalfWidthKatakana(value.getSurface)
    def isFullWidthKatakana(value: Token):Boolean = japaneseString.isFullWidthKatakana(value.getSurface)
    def isKanji(value: Token): Boolean = japaneseString.isKanji(value.getSurface)
    def containsHiragana(value: Token): Boolean = japaneseString.containsHiragana(value.getSurface)
    def containsKatakana(value: Token): Boolean = japaneseString.containsKatakana(value.getSurface)
    def containsKanji(value: Token): Boolean = japaneseString.containsKanji(value.getSurface)
    def isLatin(value: Token): Boolean = japaneseString.isLatin(value.getSurface)
    def toRomaji(value: Token, tokenizer:Option[Tokenizer] = None): String = LeKana.toRomaji(splitIntoSyllables(value))
    def toKatakana(value: Token, tokenizer:Option[Tokenizer] = None): String = LeKana.toKatakana(splitIntoSyllables(value))
    def toHiragana(value: Token, tokenizer:Option[Tokenizer] = None): String = LeKana.toHiragana(splitIntoSyllables(value))
    //override def splitIntoSyllables(value: Token, l: List[Syllable]): List[Syllable] = japaneseString.splitIntoSyllables(if (value.getPronunciation != "*") value.getPronunciation else value.getSurface)

    override def splitIntoSyllables(input: Token, l: List[(LeKana, String)]): List[(LeKana, String)] = japaneseString.splitIntoSyllables(if (input.getPronunciation != "*") input.getPronunciation else input.getSurface)
  }
}
object Japanese {
  def isHiragana[A](input: A)(implicit p: Japanese[A]): Boolean = p.isHiragana(input)
  def isHalfWidthKatakana[A](input: A)(implicit p: Japanese[A]):Boolean = p.isHalfWidthKatakana(input)
  def isFullWidthKatakana[A](input: A)(implicit p: Japanese[A]):Boolean = p.isFullWidthKatakana(input)
  def isKatakana[A](input: A)(implicit p: Japanese[A]): Boolean  = p.isKatakana(input)
  def isKana[A](input: A)(implicit p: Japanese[A]): Boolean = p.isKana(input)
  def isKanji[A](input: A)(implicit p: Japanese[A]): Boolean = p.isKanji(input)
  def containsHiragana[A](input: A)(implicit p: Japanese[A]): Boolean = p.containsHiragana(input)
  def containsKatakana[A](input: A)(implicit p: Japanese[A]): Boolean = p.containsKatakana(input)
  def containsKana[A](input: A)(implicit p: Japanese[A]): Boolean = p.containsKana(input)
  def containsKanji[A](input: A)(implicit p: Japanese[A]): Boolean =  p.containsKanji(input)
  def containsJapanese[A](input: A)(implicit p: Japanese[A]): Boolean  = p.containsJapanese(input)
  def isLatin[A](input: A)(implicit p: Japanese[A]): Boolean = p.isLatin(input)
  def toRomaji[A](input: A)(implicit p: Japanese[A]): String = p.toRomaji(input)
  def toKatakana[A](input:A)(implicit p: Japanese[A]): String = p.toKatakana(input)
  def toHiragana[A](input:A)(implicit p: Japanese[A]): String = p.toHiragana(input)
  def splitIntoSyllables[A](input:A)(implicit p: Japanese[A]):List[(LeKana, String)] = p.splitIntoSyllables(input)
}

object JapaneseSyntax {
  implicit class JapaneseOps[A](value: A) {
    def isHiragana(implicit p: Japanese[A]): Boolean = p.isHiragana(value)
    def isHalfWidthKatakana(implicit p: Japanese[A]):Boolean = p.isHalfWidthKatakana(value)
    def isFullWidthKatakana(implicit p: Japanese[A]):Boolean = p.isFullWidthKatakana(value)
    def isKatakana(implicit p: Japanese[A]): Boolean  = p.isKatakana(value)
    def isKana(implicit p: Japanese[A]): Boolean = p.isKana(value)
    def isKanji(implicit p: Japanese[A]): Boolean = p.isKanji(value)
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
    def splitIntoSyllables(implicit p: Japanese[A]):List[(LeKana, String)] = p.splitIntoSyllables(value)
  }
}
sealed trait SpacingConfig{
  def apply(t:Token):String = this match{
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


    println("Kanji    [String] to romaji = " + "皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。".toRomaji()) //467 vs 495
    val t1 = System.currentTimeMillis()
    println("毎日私は午前十時に起きます。十時から十二まで勉強します。午後一時に昼ごはんを食べます。あとでまた勉強をします。六時に地下鉄で大学へ行きます。十一時に私の家へ帰ります。そして晩ご飯を食べます。次に少し仕事をします。プログラミンをします。難しいです。それでも、私は大好きです。なぜならとても楽しいですから。朝の五時にねます。".toRomaji())
    println("診察行ってきました。最終目標は心臓移植というのは変わらないそうで、自分の心臓じゃ生きられないみたいです。お金もかかるし、手術するのも入院するのももう嫌だな～…なんてことを考えながらvitaで朧村正をプレイしてました！　牛鬼と馬鬼ってボスより強くない！？　ぜんぜん勝てないんだけど…".toRomaji())
    println("「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる".toRomaji())
    println("皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。".toRomaji())
    val t2 = System.currentTimeMillis()
    val deltaT = t2-t1
    println(s"Δt = $deltaT")

    val cachedTokenizer = new Tokenizer()
    val t3 = System.currentTimeMillis()
    println("毎日私は午前十時に起きます。十時から十二まで勉強します。午後一時に昼ごはんを食べます。あとでまた勉強をします。六時に地下鉄で大学へ行きます。十一時に私の家へ帰ります。そして晩ご飯を食べます。次に少し仕事をします。プログラミンをします。難しいです。それでも、私は大好きです。なぜならとても楽しいですから。朝の五時にねます。".toRomaji(cachedTokenizer))
    println("診察行ってきました。最終目標は心臓移植というのは変わらないそうで、自分の心臓じゃ生きられないみたいです。お金もかかるし、手術するのも入院するのももう嫌だな～…なんてことを考えながらvitaで朧村正をプレイしてました！　牛鬼と馬鬼ってボスより強くない！？　ぜんぜん勝てないんだけど…".toRomaji(cachedTokenizer))
    println("「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる".toRomaji(cachedTokenizer))
    println("皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。".toRomaji(cachedTokenizer))

    val t4 = System.currentTimeMillis()
    val deltaT2 = t4-t3
    println(s"Δt = $deltaT2 ~ 479 chars. 100k chars ~ 3.5 seconds")

    println("皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。".toKatakana(cachedTokenizer))
    println("皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。".toHiragana(cachedTokenizer))

    println("-----------------")
    printAllFeatures("リーグ@オブ@レジェンド")
    println("-----------------")
    printAllFeatures("見つけよう")
    println("-----------------")
    printAllFeatures("皆さんは日本の四つの大きな島の名前を知っていますか")

    def transliterateAll(s:String, t:Tokenizer): Unit ={
      println("------------------------")
      println(s)
      println(s.toRomaji(t))
      println(s.toHiragana(t))
      println(s.toKatakana(t))
    }

    val t5 = System.currentTimeMillis()
    transliterateAll("毎日私は午前十時に起きます。十時から十二まで勉強します。午後一時に昼ごはんを食べます。あとでまた勉強をします。六時に地下鉄で大学へ行きます。十一時に私の家へ帰ります。そして晩ご飯を食べます。次に少し仕事をします。プログラミンをします。難しいです。それでも、私は大好きです。なぜならとても楽しいですから。朝の五時にねます。", cachedTokenizer)
    transliterateAll("診察行ってきました。最終目標は心臓移植というのは変わらないそうで、自分の心臓じゃ生きられないみたいです。お金もかかるし、手術するのも入院するのももう嫌だな～…なんてことを考えながらvitaで朧村正をプレイしてました！　牛鬼と馬鬼ってボスより強くない！？　ぜんぜん勝てないんだけど…", cachedTokenizer)
    transliterateAll("「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる", cachedTokenizer)
    transliterateAll("皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。", cachedTokenizer)
    transliterateAll("お寿司が食べたい", cachedTokenizer)
    val t6 = System.currentTimeMillis()
    val deltaT3 = t6-t5
    println(s"Δt = $deltaT3 ~ 479 chars. 100k chars ~ 3.5 seconds")

  }

  def printAllFeatures(s:String):Unit = new Tokenizer().tokenize(s).asScala.toArray.foreach(t => println(t.getSurface() + ": " + t.getAllFeatures + "| " + t.getAllFeaturesArray()(1) + " | Pronunciation: " + t.getPronunciation))

  def strToRomaji(s: String):Unit = {
    new Tokenizer().tokenize(s).asScala.toArray.foreach(_.getAllFeatures)
  }
}