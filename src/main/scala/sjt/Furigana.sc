import com.atilika.kuromoji.ipadic.{Token, Tokenizer}
import sjt._
import sjt.JapaneseSyntax._
import sjt.JapaneseInstances._
import collection.JavaConverters._
import scala.annotation.tailrec
import JapaneseInstances._
import JapaneseSyntax._

val cachedTokenizer = new Tokenizer()
val nums = (1 to 6).toArray
println(nums.sliding(2,2).toList.mkString(","))

val sample = "猫が好きです".splitIntoSyllables
val sample2 = "図書館".splitIntoSyllables
val sample3 = "最近"

def collapseOnNSyllables(syllables:List[(Kana,String)]):List[(Kana,String)] = {
  val syllablesWithN = syllables.zipWithIndex.filter(_._1._1.hiragana == "ん")
  val preSyllablesWithN = syllablesWithN.map(s => if (s._2 > 0) s._2 - 1 else s._2)
  //syllables.foldLeft(List[(Kana,String)]())((z, i) => )
  syllables.reverse.foldLeft(List[(Kana,String)]()){
    (l:List[(Kana,String)],syllable:(Kana,String)) =>
      if (syllable._1.hiragana == "ん"){
        val last = l.head._1
        (Kana(last.hiragana + syllable._1.hiragana, last.katakana + syllable._1.katakana, last.romaji + syllable._1.romaji)
          , syllable._2)::l.tail
      } else syllable :: l
  }
}

//collapseOnNSyllables(sample)
sample3.toHiragana(cachedTokenizer)
sample3.toHiragana(cachedTokenizer).splitIntoSyllables.reverse
collapseOnNSyllables(sample3.toHiragana(cachedTokenizer).splitIntoSyllables)

def furiganaFor(token:Token, readingsMap:Map[Char, List[String]] = Map()):Map[Char, String] = {
  val kanjis = token.getSurface.extractKanji
  val kanjiSyllables = token.getSurface.splitIntoSyllables.diff(token.extractKana)
  val maxSyllablesPerKanji = Math.floor(kanjiSyllables.size / kanjis.size).toInt

  //Fold ん-syllables here
  val kanjiSyllablesNFolded = collapseOnNSyllables(kanjiSyllables).map(_._2)

  if (kanjis.size == 0) Map[Char, String]()
  else if (kanjis.size == 1) Map(kanjis.head -> token.getSurface.diff(token.extractKana))
  else if (kanjis.size == kanjiSyllablesNFolded.size) kanjis zip kanjiSyllablesNFolded toMap
  //else if (kanjis.size != kanjiSyllables.size && !readingsMap.isEmpty) we have readings
  //else if (kanjiSyllables.size % kanjis.size == 0) kanjiSyllables.sliding(kanjiSyllables.size / kanjis.size, kanjiSyllables.size / kanjis.size)
  else Map[Char,String]()
}
"行きます".tokenize()
furiganaFor("行きます".tokenize().head)
