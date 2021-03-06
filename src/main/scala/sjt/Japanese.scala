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

  def extractHiragana(value:A):String
  def extractKatakana(value:A):String
  def extractKana(value:A):String = extractHiragana(value) ++ extractKatakana(value)
  def extractKanji(value:A):String

  def extractUniqueHiragana(value:A):Set[Char]
  def extractUniqueKatakana(value:A):Set[Char]
  def extractUniqueKana(value:A):Set[Char] = extractUniqueHiragana(value) union extractUniqueKatakana(value)
  def extractUniqueKanji(value: A):Set[Char]

  def containsJapanese(value: A): Boolean = containsHiragana(value) || containsKatakana(value)
  def isLatin(value: A): Boolean

  def toRomaji(value: A, tokenizer:Option[Tokenizer] = None): String = transliterate(value, tokenizer).kana.romaji
  def toKatakana(value: A, tokenizer:Option[Tokenizer] = None): String =  transliterate(value, tokenizer).kana.katakana
  def toHiragana(value: A, tokenizer:Option[Tokenizer] = None): String = transliterate(value, tokenizer).kana.hiragana
  def transliterate(value:A, tokenizer:Option[Tokenizer] = None):Transliteration

  def syllabify(input:A, l: List[(Kana, String)] = Nil): List[(Kana,String)]

  def tokenize(value:A, tokenizer:Option[Tokenizer] = None):Array[Token]

  def furigana(value:A, readingsMap:Map[Char, Array[String]] = Map(), tokenizer:Option[Tokenizer] = None):Array[Transliteration]
}

object JapaneseInstances{

  implicit val japaneseChar = new Japanese[Char] {
    def isHiragana(value: Char): Boolean = ('\u3041' <= value) && (value <= '\u309e') || isHiraganaExtension(value)
    def isHalfWidthKatakana(value: Char):Boolean = ('\uff66' <= value) && (value <= '\uff9d')
    def isFullWidthKatakana(value: Char):Boolean = ('\u30a1' <= value) && (value <= '\u30fe')
    def isKanji(value: Char): Boolean = (('\u4e00' <= value) && (value <= '\u9fa5')) || (('\u3005' <= value) && (value <= '\u3007'))
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
    def isHiraganaExtension(value:Char):Boolean = Map('ゃ' -> 'a', 'ゅ'->'u', 'ょ'->'o').get(value).isDefined
    def isKatakanaExtension(value:Char):Boolean = Map('ャ' -> 'a', 'ュ' -> 'u', 'ョ' -> 'o').get(value).isDefined
    def isExtension(value:Char):Boolean = isHiraganaExtension(value) || isKatakanaExtension(value)

    def transliterate(value: Char, tokenizer: Option[Tokenizer] = Some(Kana.tokenizer)): Transliteration = {
      Transliteration(value.toString, Kana(if (value == 'ッ') "っ" else Kana.toHiragana(syllabify(value)),
                                           if (value == 'っ') "ッ" else Kana.toKatakana(syllabify(value)),
                                                                        Kana.toRomaji(syllabify(value))))
    }

    override def syllabify(input: Char, l: List[(Kana, String)]): List[(Kana, String)] =  List(Kana.nextSyllable(input.toString))

    override def extractHiragana(value: Char):String = if (isHiragana(value)) value.toString else ""
    override def extractKatakana(value: Char):String = if (isKatakana(value)) value.toString else ""
    override def extractKanji(value: Char):String = if (isKanji(value)) value.toString else ""

    override def extractUniqueHiragana(value: Char):Set[Char] = if (isHiragana(value)) Set[Char](value) else Set.empty
    override def extractUniqueKatakana(value: Char):Set[Char] = if (isKatakana(value)) Set[Char](value) else Set.empty
    override def extractUniqueKanji(value:Char):Set[Char] = if (isKanji(value)) Set[Char](value) else Set.empty

    override def tokenize(value: Char, tokenizer: Option[Tokenizer]) = tokenizer.getOrElse(Kana.tokenizer).tokenize(value.toString).asScala.toArray

    //Can it even be implemented for a single char?
    //override def furigana(value: Char, readingsMap: Map[Char, List[String]], tokenizer:Option[Tokenizer] = Some(Kana.tokenizer)):Array[Transliteration]= tokenize(value, tokenizer).headOption.map(t => japaneseString.tokenToFurigana(t)).getOrElse(Array())
    override def furigana(value: Char, readingsMap: Map[Char, Array[String]], tokenizer:Option[Tokenizer] = Some(Kana.tokenizer)):Array[Transliteration]= Array(transliterate(value))

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
    override def syllabify(input: String, l: List[(Kana, String)] = Nil): List[(Kana,String)] = {
      val nS = Kana.nextSyllable(input)
      if (input.isEmpty) l
      else syllabify(input.drop(nS._2.length), nS :: l)
    }

    override def transliterate(value: String, tokenizer: Option[Tokenizer] = Some(Kana.tokenizer)): Transliteration = {
      if (!containsKanji(value)) {
        val syllables = syllabify(value)
        Transliteration(value, Kana(Kana.toHiragana(syllables), Kana.toKatakana(syllables), Kana.toRomaji(syllables)))
      } else {
        val tokens = tokenizer.getOrElse(Kana.tokenizer).tokenize(value).asScala.toList
        Transliteration(value, Kana(Kana.toHiragana(syllabify(tokensToHiraganaString(tokens))),
                                    Kana.toKatakana(syllabify(tokensToKatakanaString(tokens))).tail,
                                    Kana.toRomaji(syllabify(tokensToRomajiString(tokens)))))
      }
    }
    override def tokenize(value:String, tokenizer:Option[Tokenizer] = Some(Kana.tokenizer)):Array[Token] = tokenizer.getOrElse(Kana.tokenizer).tokenize(value).asScala.toArray

    private def hiraganaSpacing(t:Token) = ""
    private def katakanaSpacing(t:Token) = if(Kana.isTranslateableSymbol(t.getSurface())) "" else "・"
    private def romajiSpacing(t:Token) = if(t.getAllFeaturesArray()(1) == "接続助詞" || Kana.isTranslateableSymbol(t.getSurface())) "" else " "

    //missleading function names, doesn't actually transliterate
    private def tokensToRomajiString(ts:List[Token]) = ts.foldLeft(""){(r,t:Token) => r + romajiSpacing(t) +  (if (t.getPronunciation != "*") t.getPronunciation else t.getReading)}.trim
    private def tokensToHiraganaString(ts:List[Token]) = ts.foldLeft(""){(r,t:Token) => r + hiraganaSpacing(t) +  t.getReading}.trim
    private def tokensToKatakanaString(ts:List[Token]) = ts.foldLeft(""){(r,t:Token) => r + katakanaSpacing(t) +  (if (t.getPronunciation != "*") t.getPronunciation else t.getReading)}.trim

    override def extractHiragana(value: String) = value.toCharArray.filter(c => japaneseChar.isHiragana(c)).foldLeft("")(_.toString + _.toString)
    override def extractKatakana(value: String) = value.toCharArray.filter(c => japaneseChar.isKatakana(c)).foldLeft("")(_.toString + _.toString)
    override def extractKanji(value: String) = value.toCharArray.filter(c => japaneseChar.isKanji(c)).foldLeft("")(_.toString + _.toString)

    override def extractUniqueHiragana(value: String) = value.toCharArray.filter(c => japaneseChar.isHiragana(c)).toSet
    override def extractUniqueKatakana(value: String) = value.toCharArray.filter(c => japaneseChar.isKatakana(c)).toSet
    override def extractUniqueKanji(value:String):Set[Char] = value.toCharArray.filter(c => japaneseChar.isKanji(c)).toSet

    override def furigana(value: String, readingsMap: Map[Char, Array[String]], tokenizer:Option[Tokenizer] = Some(Kana.tokenizer)) = tokenize(value, tokenizer).flatMap(t => tokenToFurigana(t, readingsMap))
    def tokenToFurigana(token:Token, readingsMap:Map[Char, Array[String]] = Map(), tokenizer:Option[Tokenizer] = Some(Kana.tokenizer)):Array[Transliteration] = {
      def collapseOnNSyllables(syllables:List[(Kana,String)]):List[(Kana,String)] = {
        //val syllablesWithN = syllables.zipWithIndex.filter(_._1._1.hiragana == "ん")
        //val preSyllablesWithN = syllablesWithN.map(s => if (s._2 > 0) s._2 - 1 else s._2)
        syllables.reverse.foldLeft(List[(Kana,String)]()){
          (l:List[(Kana,String)],syllable:(Kana,String)) =>
            if (syllable._1.hiragana == "ん" && !l.isEmpty){
              val last = if (l.headOption.isDefined) l.head._1 else Kana("","","") //PROBLEM IS HERE
              (Kana(last.hiragana + syllable._1.hiragana, last.katakana + syllable._1.katakana, last.romaji + syllable._1.romaji)
                , last.hiragana + syllable._2)::l.tail
              //val result:Int = if (!l.isEmpty) (fusedKana, syllable._2)::l.tail else (fusedKana, syllable._2)::l
            } else syllable :: l
        }.reverse
      }
      def cut[A](xs: Seq[A], n: Int):Vector[Seq[A]] = {
        val m = xs.length
        val targets = (0 to n).map{x => math.round((x.toDouble*m)/n).toInt}
        def snip(xs: Seq[A], ns: Seq[Int], got: Vector[Seq[A]]): Vector[Seq[A]] = {
          if (ns.length<2) got
          else {
            val (i,j) = (ns.head, ns.tail.head)
            snip(xs.drop(j-i), ns.tail, got :+ xs.take(j-i))
          }
        }
        snip(xs, targets, Vector.empty)
      }
      def reduceUnkownsByReadingsMap(s:String, hiraganaReading:String, readingsMap:Array[(Char, Array[String])] = Array()): Array[(Char, String)] = {
        def calculateViableReadings(reading: String, kanjisInText: Set[Char], readingsMap: Array[(Char, Array[String])]): Array[(Char, Array[String])] = readingsMap.filter(m => kanjisInText.contains(m._1)).map(m => m._1 -> m._2.filter(reading.contains(_)))
        def groupViableReadings(readings: Array[(Char, Array[String])]): (Array[(Char, Array[String])], Array[(Char, Array[String])]) = readings.partition(m => m._2.size > 1)
        def onlyOneWay(grViableReadings: (Map[Char, scala.Array[String]], Map[Char, scala.Array[String]])): Boolean = grViableReadings._1.isEmpty

        val viableReadings: Array[(Char, Array[String])] = calculateViableReadings(hiraganaReading, extractUniqueKanji(s), readingsMap)

        val groupedViableReadings:(Array[(Char, Array[String])], Array[(Char, Array[String])]) = groupViableReadings(viableReadings) //find duplicates?
        val multipleAlternatives:Array[(Char, Array[String])] = groupedViableReadings._1
        //val test:Long = groupedViableReadings._2
        val noAlternatives:Array[(Char, Array[String])] = groupedViableReadings._2.filter(i => i._2.isEmpty).toArray
        val singleAlternatives:Array[(Char, String)] = groupedViableReadings._2.filterNot(i => i._2.isEmpty).map(i => (i._1, i._2.head)).toArray

        val unknownPartHiragana: String = singleAlternatives.foldLeft(hiraganaReading) { (z, i) => z.replaceFirst(i._2, "") }
        val unknownKanjis: String = singleAlternatives.foldLeft(s){ (z, i) => z.replaceFirst(i._1.toString, "") }

        val oneWay = multipleAlternatives.isEmpty && noAlternatives.isEmpty && unknownKanjis == "" && unknownPartHiragana == ""//added noalternatives

        if (oneWay) singleAlternatives.reverse //prev no reverse
        else if (!oneWay && s != unknownKanjis) (singleAlternatives ++ reduceUnkownsByReadingsMap(unknownKanjis, unknownPartHiragana, multipleAlternatives))
        else singleAlternatives ++ (unknownKanjis ++ noAlternatives.map(_._1).mkString("") zip cut(collapseOnNSyllables(syllabify(unknownPartHiragana)).map(_._2), unknownKanjis.size).map(_.mkString(""))).toMap //unknownPartHiragana.extractKanji
      }
      def extractKanjiSyllables(s:String) = syllabify(toHiragana(s).diff(extractKana(s)))

      val kanjis:String = extractKanji(token.getSurface)
      val numKanjis:Int = kanjis.size
      val kanjiUnique:Set[Char] = extractUniqueKanji(token.getSurface)
      val kanjiSyllables:List[(Kana, String)] = extractKanjiSyllables(tokensToHiraganaString(List(token)))

      val kanjiPartInHiragana:String = Kana.toHiragana(syllabify(tokensToHiraganaString(List(token)))).diff(extractKana(token.getSurface)) //potencial bug if surface isn't in hiragana

      val kanjiSyllablesNFolded = collapseOnNSyllables(kanjiSyllables).map(_._2)

      val cutGroupings = cut(kanjiSyllablesNFolded, kanjis.size).map(_.mkString(""))

      def tupleToTransliteration(k:Char, s:String) = transliterate(s).copy(original=k.toString)

      if (numKanjis == 0) Array()
      else if (numKanjis == 1) Array(tupleToTransliteration(kanjis.head,kanjiPartInHiragana))
      else if (numKanjis == kanjiSyllablesNFolded.size) kanjis.zip(kanjiSyllablesNFolded).map(z => tupleToTransliteration(z._1,z._2)).toArray
      else if (numKanjis != kanjiSyllablesNFolded.size && !readingsMap.isEmpty) reduceUnkownsByReadingsMap(kanjis, kanjiPartInHiragana, readingsMap.toArray).map(z => tupleToTransliteration(z._1,z._2)) //no toArray
      else kanjis.zip(cutGroupings).map(z => tupleToTransliteration(z._1,z._2)).toArray
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

    override def syllabify(input: Token, l: List[(Kana, String)] = Nil): List[(Kana, String)] = japaneseString.syllabify(tokenToString(input))
    private def tokenToString(input:Token): String = if (input.getPronunciation != "*") input.getPronunciation else input.getSurface
    override def extractHiragana(value: Token):String = japaneseString.extractHiragana(value.getSurface)
    override def extractKatakana(value: Token):String = japaneseString.extractKatakana(value.getSurface)
    override def extractKanji(value: Token):String = japaneseString.extractKanji(value.getSurface)

    override def extractUniqueHiragana(value: Token):Set[Char] = japaneseString.extractUniqueHiragana(value.getSurface)
    override def extractUniqueKatakana(value: Token):Set[Char] = japaneseString.extractUniqueKatakana(value.getSurface)
    override def extractUniqueKanji(value:Token):Set[Char] = japaneseString.extractUniqueKanji(value.getSurface)

    override def transliterate(value: Token, tokenizer: Option[Tokenizer] = Some(Kana.tokenizer)): Transliteration = {
      val syllables = syllabify(value)
      Transliteration(value.getSurface, Kana(Kana.toHiragana(syllables),
                                                 Kana.toKatakana(syllables),
                                                  Kana.toRomaji(syllables)))
    }

    override def tokenize(value: Token, tokenizer: Option[Tokenizer]):Array[Token] = Array(value)
    override def furigana(token:Token, readingsMap:Map[Char, Array[String]] = Map(), tokenizer:Option[Tokenizer] = Some(Kana.tokenizer)):Array[Transliteration] = japaneseString.tokenToFurigana(token,readingsMap,tokenizer)

  }
}
object Japanese {
  def isHiragana[A](input: A)(implicit p: Japanese[A]): Boolean = p.isHiragana(input)
  def isHalfWidthKatakana[A](input: A)(implicit p: Japanese[A]):Boolean = p.isHalfWidthKatakana(input)
  def isFullWidthKatakana[A](input: A)(implicit p: Japanese[A]):Boolean = p.isFullWidthKatakana(input)
  def isKatakana[A](input: A)(implicit p: Japanese[A]): Boolean  = p.isKatakana(input)
  def isKana[A](input: A)(implicit p: Japanese[A]): Boolean = p.isKana(input)
  def isKanji[A](input: A)(implicit p: Japanese[A]): Boolean = p.isKanji(input)

  def extractHiragana[A](input:A)(implicit p: Japanese[A]):String = p.extractHiragana(input)
  def extractKatakana[A](input:A)(implicit p: Japanese[A]):String = p.extractKatakana(input)
  def extractKanji[A](input:A)(implicit p: Japanese[A]):String = p.extractKanji(input)

  def extractUniqueHiragana[A](input:A)(implicit p: Japanese[A]):Set[Char] = p.extractUniqueHiragana(input)
  def extractUniqueKatakana[A](input:A)(implicit p: Japanese[A]):Set[Char] = p.extractUniqueKatakana(input)
  def extractUniqueKanji[A](input: A)(implicit p: Japanese[A]): Set[Char] = p.extractUniqueKanji(input)

  def containsHiragana[A](input: A)(implicit p: Japanese[A]): Boolean = p.containsHiragana(input)
  def containsKatakana[A](input: A)(implicit p: Japanese[A]): Boolean = p.containsKatakana(input)
  def containsKana[A](input: A)(implicit p: Japanese[A]): Boolean = p.containsKana(input)
  def containsKanji[A](input: A)(implicit p: Japanese[A]): Boolean =  p.containsKanji(input)
  def containsJapanese[A](input: A)(implicit p: Japanese[A]): Boolean  = p.containsJapanese(input)
  def isLatin[A](input: A)(implicit p: Japanese[A]): Boolean = p.isLatin(input)
  def toRomaji[A](input: A)(implicit p: Japanese[A]): String = p.toRomaji(input)
  def toKatakana[A](input:A)(implicit p: Japanese[A]): String = p.toKatakana(input)
  def toHiragana[A](input:A)(implicit p: Japanese[A]): String = p.toHiragana(input)
  def syllabify[A](input:A)(implicit p: Japanese[A]):List[(Kana, String)] = p.syllabify(input)

  def tokenize[A](input:A)(implicit p: Japanese[A]):Array[Token] = p.tokenize(input)//missing tokenizer??
  def furigana[A](input:A)(implicit p: Japanese[A]):Array[Transliteration] = p.furigana(input) //missing tokenizer??
}

object JapaneseSyntax {
  implicit class JapaneseOps[A](value: A) {
    def isHiragana(implicit p: Japanese[A]): Boolean = p.isHiragana(value)
    def isHalfWidthKatakana(implicit p: Japanese[A]):Boolean = p.isHalfWidthKatakana(value)
    def isFullWidthKatakana(implicit p: Japanese[A]):Boolean = p.isFullWidthKatakana(value)
    def isKatakana(implicit p: Japanese[A]): Boolean  = p.isKatakana(value)
    def isKana(implicit p: Japanese[A]): Boolean = p.isKana(value)
    def isKanji(implicit p: Japanese[A]): Boolean = p.isKanji(value)

    def extractHiragana(implicit p: Japanese[A]): String = p.extractHiragana(value)
    def extractKatakana(implicit p: Japanese[A]): String = p.extractKatakana(value)
    def extractKana(implicit p: Japanese[A]): String = p.extractKana(value)
    def extractKanji(implicit p: Japanese[A]): String = p.extractKanji(value)

    def extractUniqueHiragana(implicit p: Japanese[A]): Set[Char] = p.extractUniqueHiragana(value)
    def extractUniqueKatakana(implicit p: Japanese[A]): Set[Char] = p.extractUniqueKatakana(value)
    def extractUniqueKana(implicit p: Japanese[A]): Set[Char] = p.extractUniqueKana(value)
    def extractUniqueKanji(implicit p: Japanese[A]): Set[Char] = p.extractUniqueKanji(value)

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
    def transliterate(t:Tokenizer = null)(implicit p: Japanese[A]):Transliteration = if (t != null) p.transliterate(value, Some(t)) else p.transliterate(value)
    def syllabify(implicit p: Japanese[A]):List[(Kana, String)] = p.syllabify(value)

    def tokenize(t:Tokenizer = null)(implicit p: Japanese[A]):Array[Token] = if (t != null) p.tokenize(value, Some(t)) else p.tokenize(value)

    def furigana(readingsMap: Map[Char, Array[String]] = Kana.readingsForKanji, t: Tokenizer = null)(implicit p: Japanese[A]): Array[Transliteration] =
      if (t != null) p.furigana(value, readingsMap, Some(t)) else p.furigana(value, readingsMap)

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
  import JapaneseInstances._
  import JapaneseSyntax._
  def main(args: Array[String]): Unit = {
    //TODO: Refactor messy type-class instances dependencies
    //TODO: Rename Kana fields and privitize them if necessary
    //TODO: Command line interaction
    //TODO: Make it so that furigana extraction maintains order when there is a dictionary
  }
 }