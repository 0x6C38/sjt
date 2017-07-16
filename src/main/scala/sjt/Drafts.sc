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

val exampleString = "お寿司が食べたい。"
val exampleString2 = "おすしがたべたいです"
val tokenizer = new Tokenizer()
val tokens = tokenizer.tokenize(exampleString2).asScala.toArray
val token: Token = tokens.head
val romanizedTokens = tokens.map(_.toRomaji)
//println("し".toRomaji)
println(token.toRomaji)
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



//def isKatakanaMiniVowel(value: Char): Boolean = value == 'ェ' || value == 'ョ'
def printDebug(s: String): Unit = {
  println("---------------------------")
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
  println(s"$s to romaji[Old]: " + Kana.kanaSilableToRomaji(s))
  println(s"$s to romaji[New]: " + Kana.kanaToRomaji((s)))

  println("---------------------------")

}

//import sjt.{Syllable, NotSyllable, SimpleSilable, ExtendedConsonant, ExtendedVowel, ExtendedConsonantNVowel, Yoon, YoonEV, YoonEC, YoonECEV}

val example1 = "きょう"
val example2 = "とう"
val example3 = "ぎゅ"
val example4 = "ぎゅう"
val example5 = "った"
val example6 = "っぎゅう"
val example7 = "っとう"
val example8 = "とうきょう"
val example9 = "とうきょうきょうと"
val example10 = "とう#きょう"
val example11 = "とう#$きょう"

printDebug(example1)
printDebug(example2)
printDebug(example3)
printDebug(example4)
printDebug(example5)
printDebug(example6)
printDebug(example7)
printDebug(example8)
printDebug(example9)
printDebug(example10)
printDebug(example11)

//---TODO: Make every function in Syllable work for Romaji
//---TODO: Move every function in Kana into the appropriate Japanese type class
//---TODO: Option to pass a Tokenizer for better performance
//---TODO: Make the Japanese type class work with higher kinded types
//---TODO: Add non-calculated fields in Kana
//---TODO: Write tests
//---TODO: Publish
