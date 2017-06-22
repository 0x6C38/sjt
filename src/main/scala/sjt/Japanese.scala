package sjt

import java.nio.Buffer
import java.nio.charset.Charset

import com.atilika.kuromoji.ipadic.Token
import com.atilika.kuromoji.ipadic.Tokenizer

import collection.JavaConverters._

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
  def toKatakana(value: A): A
  def toHiragana(value: A): A
}
object JapaneseInstances{
  implicit val japaneseChar = new Japanese[Char] {
    def isHiragana(value: Char): Boolean = ('\u3041' <= value) && (value <= '\u309e')
    def isHalfWidthKatakana(value: Char):Boolean = ('\uff66' <= value) && (value <= '\uff9d')
    def isFullWidthKatakana(value: Char):Boolean = ('\u30a1' <= value) && (value <= '\u30fe')
    def isKanji(value: Char): Boolean = (('\u4e00' <= value) && (value <= '\u9fa5')) || (('\u3005' <= value) && (value <= '\u3007'))
    def containsHiragana(value: Char): Boolean = isHiragana(value)
    def containsKatakana(value: Char): Boolean = isKatakana(value)
    def containsKanji(value: Char): Boolean = isKanji(value)
    def isLatin(value: Char): Boolean = Charset.forName("US-ASCII").newEncoder().canEncode(value)
    def toRomaji(value: Char): String = {
      val romaji = Array("a", "a","i", "i","u", "u","e", "e","o", "o","ka", "ga","ki", "gi","ku", "gu","ke", "ge","ko", "go","sa", "za","shi", "ji","su", "zu","se", "ze","so", "zo","ta", "da","chi", "ji","tsu", "tsu", "zu","te", "de","to", "do","na","ni","nu","ne","no","ha", "ba", "pa","hi", "bi", "pi","fu", "bu", "pu","he", "be", "pe","ho", "bo", "po","ma","mi","mu","me","mo","a", "ya","u", "yu","o", "yo","ra","ri","ru","re","ro","wa", "wa","wi", "we","o","n","v","ka","ke")
      if (isKatakana(value) && value != '゛' && value != ',') { //In case of katakana
        return toRomaji(toHiragana(value))
      } else if (isHiragana(value)) { //in case of hiragana
        return romaji(value - 0x3041)
      } else { //nothing else can be translated
        return String.valueOf(value)
      }
    }
    def toKatakana(value:Char):Char = {
      val translatableRomajiChars = Map('a' -> 'ア','e' -> 'エ','i' -> 'イ','o' -> 'オ' ,'u' -> 'ウ','n' -> 'ン')
      if (isHiragana(value) && value != 'n') (value + 0x60).toChar
      else translatableRomajiChars.get(value).getOrElse(value)
    }
    def toHiragana(value:Char):Char= value match{
      case isFullWidthKatakana => (value - 0x60).toChar //if katakana, translate it
      case isHalfWidthKatakana => (value - 0xcf25).toChar
      case _ => Map('a' -> 'あ','e' -> 'え','i' -> 'い','o' -> 'お' ,'u' -> 'う','n' -> 'ん').get(value).getOrElse(value) //if translateable romaji, translate, else do nothing
    }

    def extendChar(value:Char):Char = value.toLower match {
      case 'a' => 'ā'
      case 'e' => 'ē'
      case 'i' => 'ī'
      case 'o' => 'ō'
      case 'u' => 'ū'
      //case  "[a-z]" =>
      case _ => value
    }
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

    //def toRomaji(value: String): String =  new Tokenizer().tokenize(value).asScala.toArray.flatMap(_.getPronunciation.map(japaneseChar.toRomaji(_))).mkString
    def localTokenToRomaji(value: Token) = value.getPronunciation.foldLeft("") { (acc: String, currentV: Char) =>
      if (!acc.isEmpty && acc.last == 'ッ') {
        val nextChar  = japaneseChar.toRomaji(currentV)
        acc.init + nextChar + nextChar
      }
      else if (currentV == 'ー') acc.init + japaneseChar.extendChar(japaneseChar.toRomaji(currentV).charAt(0))
      else acc + japaneseChar.toRomaji(currentV)
    }
//    def grammarToString(grammar: String, value: String, isLast: Boolean): String = {
//      val result = value
//
//      val isPrefix = "接頭詞" == grammar
//      val isAuxiliaryVerb = "助動詞" == grammar
//      val isPunctuation = "記号" == grammar
//      val isDesu = "desu" == value
//
//      val shouldSeparateNext = !(isPrefix || isAuxiliaryVerb || isPunctuation || isLast)
//      val shouldPrefix = isDesu
//      val nextSeparator = if (shouldSeparateNext) " " else ""
//      val beforeSeparator = if (shouldPrefix) " " else ""
//      beforeSeparator + value + nextSeparator
//    }

    def toRomaji(value: String): String  = {
      val tokens = new Tokenizer().tokenize(value).asScala
      def romanize(acc: String, currentV: Token):String = {
        if (currentV.getAllFeaturesArray.head == "助動詞" && currentV.getAllFeaturesArray.head != "デス") acc + localTokenToRomaji(currentV)
        else acc + " " + localTokenToRomaji(currentV)
      }
      romanize(
        tokens.init.foldLeft("") { (acc: String, currentV: Token) =>
         romanize(acc, currentV)
        }
          + " ", tokens.last).trim
    }

//    def toRomaji(value: String): String = {
//      val tokens = new Tokenizer().tokenize(value).asScala
//      if (!isLatin(value)){
//        val romajis = tokens.map(t => localTokenToRomaji(t))
//
//        val grammars = tokens.map(t => (t.getAllFeaturesArray.head))
//        val zipped2 = romajis zip grammars
//
//        val finalValues = zipped2.init.map(tuple => grammarToString(tuple._2, tuple._1, false))
//
//        finalValues.mkString
//      }else{
//        value
//      }
//    }
    def toKatakana(value: String): String =  ???
    def toHiragana(value: String): String = ???
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
    //def toRomaji(value: Token): String = value.getSurface.flatMap(japaneseChar.toRomaji)
    def toRomaji(value: Token): String = value.getPronunciation.foldLeft("") { (acc: String, currentV: Char) =>
      if (acc.last == 'ッ') acc.init + japaneseChar.toRomaji(currentV) + japaneseChar.toRomaji(currentV)
      else if (currentV == 'ー') acc.init + japaneseChar.extendChar(japaneseChar.toRomaji(currentV).charAt(0))
      else acc + japaneseChar.toRomaji(currentV)
    }
    def toKatakana(value: Token): Token =  ??? //value.getReading.flatMap(japaneseChar.toRomaji)
    def toHiragana(value: Token): Token = ??? //value.getPronunciation.flatMap(japaneseChar.toHiragana) ???
  }
}
object Japanese{
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
  def toKatakana[A](input:A)(implicit p: Japanese[A]): A = p.toKatakana(input)
  def toHiragana[A](input:A)(implicit p: Japanese[A]): A = p.toHiragana(input)
}
object JapaneseSyntax{
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
    def toKatakana(implicit p: Japanese[A]): A = p.toKatakana(value)
    def toHiragana(implicit p: Japanese[A]): A = p.toHiragana(value)
  }
}
object Main {
  def main(args: Array[String]): Unit = {
    import JapaneseInstances._
    import JapaneseSyntax._

    println("--Chars--")
    println("Hiragana [Char] す to romaji = " + 'す'.toRomaji)
    println("Hiragana [Char] す to katakana = " + 'す'.toKatakana)
    println("Katakana [Char] ス to romaji = " + 'ス'.toRomaji)
    println("Katakana [Char] ス to Hiragana = " + 'ス'.toHiragana)
    println("Romaji   [Char] n to Hiragana = " + 'n'.toHiragana)
    println('ん' == 'n'.toHiragana)
    println("Romaji   [Char] o to Katakana = " + 'o'.toKatakana)

    println("--Strings--")
    println("Hiragana [String] to romaji = " + "おすしがたべたいです".toRomaji)
    println("Kanji    [String] to romaji = " + "わたしはかわいいです".toRomaji)
    println("Kanji    [String] to romaji = " + "お寿司が食べたいです".toRomaji)
    println("Kanji    [String] to romaji = " + "私は可愛いです".toRomaji)
    println("Katakana [String] to romaji = " + "オスシガタベタイデス".toRomaji)
    println("Katakana [String] to romaji = " + "ワタシハカワイイデス".toRomaji)

    //println("す".toKatakana)


    printAllFeatures("お寿司が食べたいです")
    println("-----------------")
    printAllFeatures("私は可愛いです")
  }
  def printAllFeatures(s:String):Unit = new Tokenizer().tokenize(s).asScala.toArray.foreach(t => println(t.getSurface() + ": " + t.getAllFeatures))
  //flatMap(_.toRomaji)
  def strToRomaji(s: String):Unit = {
    import JapaneseInstances._
    import JapaneseSyntax._
    //new Tokenizer().tokenize(s).asScala.toArray.flatMap(_.getPronunciation)
    new Tokenizer().tokenize(s).asScala.toArray.foreach(_.getAllFeatures)
  }
}