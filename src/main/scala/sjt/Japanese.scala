package sjt

import java.nio.Buffer
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
  def toRomaji(value: A): String
  def toKatakana(value: A): String
  def toHiragana(value: A): String
  def splitIntoSyllables(value:A, l: List[Syllable] = Nil): List[Syllable]
}
object JapaneseInstances{
  implicit val japaneseSyllable = new Japanese[Syllable] {

    override def isHiragana(value: Syllable): Boolean = Kana.allHiraganaToRomajiM.keySet.contains(value.text)
    override def isHalfWidthKatakana(value: Syllable): Boolean = false //calculated by a fair dice roll
    override def isFullWidthKatakana(value: Syllable): Boolean = Kana.allKatakanaToRomajiM.keySet.contains(value.text)

    override def isKanji(value: Syllable): Boolean = value.text.toCharArray.forall(value => (('\u4e00' <= value) && (value <= '\u9fa5')) || (('\u3005' <= value) && (value <= '\u3007')))

    override def containsHiragana(value: Syllable): Boolean = value.text.exists(c => Kana.isHiragana(c))
    override def containsKatakana(value: Syllable): Boolean = value.text.exists(c => Kana.isKatakana(c))
    override def containsKanji(value: Syllable): Boolean = value.text.exists(value => (('\u4e00' <= value) && (value <= '\u9fa5')) || (('\u3005' <= value) && (value <= '\u3007')))

    override def isLatin(value: Syllable): Boolean = value.text.matches("[a-zA-Z].*")

    override def toRomaji(value: Syllable): String = Syllable.kanaSilableToRomaji(value.text)
    override def toKatakana(value: Syllable): String = Syllable.hiraganaOrRomajiToKatakana(value.text)
    override def toHiragana(value: Syllable): String = Syllable.katakanaOrRomajiToHiragana(value.text)

    override def splitIntoSyllables(value: Syllable, l: List[Syllable]): List[Syllable] = List(value)

  }

  implicit val japaneseChar = new Japanese[Char] {
    def isHiragana(value: Char): Boolean = ('\u3041' <= value) && (value <= '\u309e')
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

    override def toRomaji(value: Char): String = japaneseSyllable.toRomaji(splitIntoSyllables(value).head)
    override def toKatakana(value: Char): String = japaneseSyllable.toKatakana(splitIntoSyllables(value).head)
    override def toHiragana(value: Char): String = japaneseSyllable.toHiragana(splitIntoSyllables(value).head)

    override def splitIntoSyllables(value: Char, l: List[Syllable] = Nil): List[Syllable] = List(Syllable.nextSyllable(value.toString))

  }

  implicit val japaneseString = new Japanese[String] {
    def isHiragana(value: String): Boolean = value.toCharArray.forall(japaneseChar.isHiragana) //replace for syllable implementation?
    def isHalfWidthKatakana(value: String):Boolean = value.toCharArray.forall(japaneseChar.isHalfWidthKatakana)
    def isFullWidthKatakana(value: String):Boolean = value.toCharArray.forall(japaneseChar.isFullWidthKatakana)
    def isKanji(value: String): Boolean = value.toCharArray.forall(japaneseChar.isKanji)
    def containsHiragana(value: String): Boolean = value.toCharArray.exists(japaneseChar.containsHiragana)
    def containsKatakana(value: String): Boolean = value.toCharArray.exists(japaneseChar.containsKatakana)
    def containsKanji(value: String): Boolean = value.toCharArray.exists(japaneseChar.containsKanji)
    def isLatin(value: String): Boolean = value.toCharArray.forall(japaneseChar.isLatin)

    def localTokenToRomaji(value: Token): String = {if (value.getPronunciation != "*") value.getPronunciation else  value.getSurface}.foldLeft("") { (acc: String, currentV: Char) =>
      if (!acc.isEmpty && (acc.last == 'ッ' || acc.last == 'っ')) acc.init + japaneseChar.toRomaji(currentV).charAt(0) + japaneseChar.toRomaji(currentV)
      else if (currentV == 'ー' || (!acc.isEmpty && japaneseChar.isLatinVowel(acc.last) && currentV == 'ー' || currentV == 'う')) {if (acc.init.lastOption.getOrElse('∑') == 'i') acc.init.init + "y" else acc.init} + japaneseChar.extendChar(japaneseChar.toRomaji(acc.last).charAt(0))
      else if (japaneseChar.isExtension(currentV) && (!acc.isEmpty && (japaneseChar.isLatinVowel(acc.last)))){
        if(acc.init.lastOption != 'j') {
          acc + extendString(currentV)
        } else { //acc.init
          acc.init + extendString(currentV)
        }
      }
      else if (japaneseChar.isVowel(currentV) && (!acc.isEmpty && !acc.init.isEmpty && acc.init.last == 'j'))acc.init + japaneseChar.toRomaji(currentV)
      else acc + japaneseChar.toRomaji(currentV)
    }
    def extendString(current:Char):Char = Map('ゃ' -> 'a', 'ゅ'->'u', 'ょ'->'o', 'ャ' -> 'a', 'ュ' -> 'u', 'ョ' -> 'o').get(current).getOrElse(current)


    def toRomaji(value: String): String  = if (!containsKanji(value)) splitIntoSyllables(value).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toRomaji(s))
                                            else new Tokenizer().tokenize(value).asScala.foldLeft(""){(r,t:Token) => r + splitIntoSyllables(if (t.getPronunciation != "*") t.getPronunciation else t.getSurface).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toRomaji(s))  + " "}.trim
    def toHiragana(value: String): String  = if (!containsKanji(value)) splitIntoSyllables(value).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toHiragana(s))
                                              else new Tokenizer().tokenize(value).asScala.foldLeft(""){(r,t:Token) => r + splitIntoSyllables(if (t.getPronunciation != "*") t.getPronunciation else t.getSurface).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toHiragana(s))  + " "}.trim
    def toKatakana(value: String): String  = if (!containsKanji(value)) splitIntoSyllables(value).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toKatakana(s))
                                              else new Tokenizer().tokenize(value).asScala.foldLeft(""){(r,t:Token) => r + splitIntoSyllables(if (t.getPronunciation != "*") t.getPronunciation else t.getSurface).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toKatakana(s))  + " "}.trim

    @tailrec
    override def splitIntoSyllables(input: String, l: List[Syllable] = Nil): List[Syllable] = {
      val nS = Syllable.nextSyllable(input)
      if (input.isEmpty) l
      else splitIntoSyllables(input.drop(nS.text.length), nS :: l)
    }

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
    def toRomaji(value: Token): String = splitIntoSyllables(value).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toRomaji(s))
    def toKatakana(value: Token): String = splitIntoSyllables(value).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toKatakana(s))
    def toHiragana(value: Token): String = splitIntoSyllables(value).reverse.foldLeft("")((a,s) => a + japaneseSyllable.toHiragana(s))
    override def splitIntoSyllables(value: Token, l: List[Syllable]): List[Syllable] = japaneseString.splitIntoSyllables(if (value.getPronunciation != "*") value.getPronunciation else value.getSurface)
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
  def splitIntoSyllables[A](input:A)(implicit p: Japanese[A]):List[Syllable] = p.splitIntoSyllables(input)
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
    def toRomaji(implicit p: Japanese[A]): String = p.toRomaji(value)
    def toKatakana(implicit p: Japanese[A]): String = p.toKatakana(value)
    def toHiragana(implicit p: Japanese[A]): String = p.toHiragana(value)
    def splitIntoSyllables(implicit p: Japanese[A]):List[Syllable] = p.splitIntoSyllables(value)
  }
}
object Main {
  def main(args: Array[String]): Unit = {
    import JapaneseInstances._
    import JapaneseSyntax._

    println("--Chars: Hiragana--")
    println("Hiragana [Char] す to romaji = " + 'す'.toRomaji)
    println("Hiragana [Char] す to katakana = " + 'す'.toKatakana)
    println("--Chars: Katakana--")
    println("Katakana [Char] ス to romaji = " + 'ス'.toRomaji)
    println("Katakana [Char] ス to Hiragana = " + 'ス'.toHiragana)
    println("--Chars: Special--")
    println("Katakana [Char] ッ to romaji = " + 'ッ'.toRomaji)
    println("Katakana [Char] ー to romaji = " + 'ー'.toHiragana)
    println("Katakana [Char] ッ to hiragana = " + 'ッ'.toHiragana)
    println("Katakana [Char] ー to hiragana = " + 'ー'.toHiragana)
    println("--Chars: Romaji--")
    println("Romaji   [Char] n to Hiragana = " + 'n'.toHiragana)
    println("Romaji   [Char] o to Katakana = " + 'o'.toKatakana)
    println("Romaji   [Char] j to Katakana = " + 'j'.toKatakana)

    println("--Strings: Kanji Sentences--")
    println("Kanji    [String] to romaji = " + "私は可愛いです".toRomaji)
    println("Kanji    [String] to romaji = " + "お寿司が食べたいです".toRomaji)
    println("--Strings: Hiragana Sentences--")
    println("Hiragana [String] to romaji = " + "おすしがたべたいです".toRomaji)
    println("Hiragana [String] to romaji = " + "わたしはかわいいです".toRomaji)
    println("--Strings: Katakana Sentences--")
    println("Katakana [String] to romaji = " + "オスシガタベタイデス".toRomaji)
    println("Katakana [String] to romaji = " + "ワタシハカワイイデス".toRomaji)
    println("--Strings: Single Katakana Words--")
    println("Katakana [String] to romaji = " + "ニュース".toRomaji)
    println("--Strings: Single Hiragana Words--")
    println("Hiragana [String] to romaji = " + "ぎゅうにゅう".toRomaji)
    println("--Strings: Single Words Kanji--")
    println("Kanji    [String] to romaji = " + "可愛い".toRomaji)
    println("Kanji    [String] to romaji = " + "私".toRomaji)
    println("Kanji    [String] to romaji = " + "大きな".toRomaji)
    println("--Strings: Single Words Kanji Diphthong--")
    println("Kanji    [String] to romaji = " + "東京".toRomaji)
    println("Kanji    [String] to romaji = " + "牛乳".toRomaji)
    println("Kanji    [String] to romaji = " + "喋ります".toRomaji)
    println("Katakana [String] to romaji = " + "ジャク".toRomaji)
    println("Katakana [String] to romaji = " + "ニャンコ".toRomaji)
    println("--Strings: Single Words Hiragana--")
    println("Kanji    [String] to romaji = " + "こと".toRomaji)
    println("--Strings: Single Words Hiragana--")
    println("Katakana [String] to romaji = " + "コーヒー".toRomaji)
    println("Katakana [String] to romaji = " + "リグオブレジェンド".toRomaji)
    println("--Strings: Special--")
    println("Katakana [String] to romaji = " + "リーグ・オブ・レジェンド".toRomaji)
    println("Katakana [String] to romaji = " + "リーグ@オブ@レジェンド".toRomaji)

    //println("Kanji    [String] to hiragana = " + "大きな".toHiragana)
    //println("Kanji    [String] to katakana = " + "大きな".toKatakana)

    val t1 = System.currentTimeMillis()
    println("Kanji    [String] to romaji = " + "皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。".toRomaji) //467 vs 495
    val t2 = System.currentTimeMillis()
    val deltaT = t2-t1
    println(s"Δt = $deltaT")

    println("毎日私は午前十時に起きます。十時から十二まで勉強します。午後一時に昼ごはんを食べます。あとでまた勉強をします。六時に地下鉄で大学へ行きます。十一時に私の家へ帰ります。そして晩ご飯を食べます。次に少し仕事をします。プログラミンをします。難しいです。それでも、私は大好きです。なぜならとても楽しいですから。朝の五時にねます。".toRomaji)
    println("「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる".toRomaji)

    println("-----------------")
    printAllFeatures("リーグ@オブ@レジェンド")
    println("-----------------")
    printAllFeatures("見つけよう")
    println("-----------------")
    printAllFeatures("皆さんは日本の四つの大きな島の名前を知っていますか")

  }
  def printAllFeatures(s:String):Unit = new Tokenizer().tokenize(s).asScala.toArray.foreach(t => println(t.getSurface() + ": " + t.getAllFeatures + " | Pronunciation: " + t.getPronunciation))

  def strToRomaji(s: String):Unit = {
    new Tokenizer().tokenize(s).asScala.toArray.foreach(_.getAllFeatures)
  }
}