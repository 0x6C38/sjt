import com.atilika.kuromoji.ipadic.{Token, Tokenizer}
import sjt._
import sjt.JapaneseSyntax._
import sjt.JapaneseInstances._
import collection.JavaConverters._
import scala.annotation.tailrec

val t = 'c'

t.isHiragana

import JapaneseInstances._
import JapaneseSyntax._

Japanese.isHiragana('c')

'j'.isHiragana
'j'.isKatakana
'カ'.isKatakana

Kana.allKatakanaToHiraganaM // doesn't have a single ン -> ん and has an incorrect ンー -> ん
Kana.allHiraganaToKatakanaM // doesn't have a single ン -> ん and has an incorrect ん   -> ンー
Kana.allKanaToRomajiM // has a ン -> n and a ん -> n
Kana.allKanaToRomajiM.map { case (k, v) => (k, Syllable.kanaSilableToRomaji(k)) }
// the problem is romajiToKatakana
Kana.allRomajiToKatakanaM // has no n -> ン, it has n -> ンー
Kana.allKanaAndAllKanaExtendedVowelsAndConsonantsM // has ン -> n
Kana.allKatakanaToRomajiM //also has  ン -> n


val exampleString = "お寿司が食べたい"
val exampleString2 = "おすしがたべたいです"
val tokenizer = new Tokenizer()
val tokens = tokenizer.tokenize(exampleString2).asScala.toArray
val token: Token = tokens.head
val romanizedTokens = tokens.map(_.toRomaji())
//println("し".toRomaji)
println(token.toRomaji())
println(romanizedTokens)
//val ts3:String = tokens.flatMap(_.getPronunciation.map(_.toRomaji)).mkString
val ts3: String = tokens.map(_.getPronunciation).mkString
tokens.head.getPronunciation
//def isHiragana(value:Char):Boolean = '\u3041' <= value
//println(isHiragana('C'))
import sjt._
import sjt.JapaneseSyntax._
import sjt.JapaneseInstances._

"shiっte".indexOf("っ")
'n'.isFullWidthKatakana



def printDebug(s: String): Unit = {
  //println("---------------------------")
  println(s"$s Original: $s, romaji: " + s.toRomaji() + " hiragana: " + s.toHiragana() + ", katakana: " +s.toKatakana()+ ", syllables: " + s.splitIntoSyllables)
  println(s"$s to romaji[New]: " + s.toRomaji())
  println(s"$s to Hiragana: " + s.toHiragana())
  println(s"$s syllables: " + s.splitIntoSyllables)
  println(s"$s syllables k: " + s.toKatakana().splitIntoSyllables)
  println(s"$s starts with consonant: " + Syllable.sWC(s))
  println(s"$s starts with i-consonant: " + Syllable.sWIC(s))
  println(s"$s starts With small Y: " + Syllable.sWSmallY(s))
  println(s"$s starts With Yoon: " +Syllable.swYoon(s))
  println(s"$s starts With Yoon + extended vowel: " + Syllable.swEVYoon(s))
  println(s"$s starts With extended consonant + Yoon: " + Syllable.swECYoon(s))
  println(s"$s starts With extended consonant + Yoon + vowel: " + Syllable.swECYoonEV(s))
  println(s"$s starts With extended consonant: " + Syllable.swEC(s))
  println(s"$s starts With extended i-consonant: " + Syllable.swEIC(s))
  println(s"$s starts With extended vowel: " + Syllable.swEV(s))
  println(s"$s starts With extended consonant + vowel: " + Syllable.swECEV(s))
  println
  //println("---------------------------")

}
def transliterateAll(s: String, t: Tokenizer): Unit = {
  println("------------------------")
  println(s)
  println(s.toRomaji(t))
  println(s.toHiragana(t))
  println(s.toKatakana(t))
}
val cachedTokenizer = new Tokenizer()
transliterateAll("聞く",cachedTokenizer)
transliterateAll("大きな",cachedTokenizer)
transliterateAll("東京",cachedTokenizer)
transliterateAll("にっぽん",cachedTokenizer)
transliterateAll("日本",cachedTokenizer)
transliterateAll("ニッポン",cachedTokenizer)
transliterateAll("ン",cachedTokenizer)

val example54 ="聞く"
val example56 ="大きな"
val example57 ="東京"
val example59 ="見つけよう"//kuromoji error

//Kana.allKanaAndAllKanaExtendedVowelsAndConsonantsM

printDebug(example54)

printDebug(example56)
printDebug(example57)
printDebug(example59)



//---TODO: Write tests
//---TODO: Publish
