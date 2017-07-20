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

"namae wo shiッ te i masッu ka".indexOf("ッ")


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
val example12 = "to"
val example13 = "k"
val example14 = ""
val example15 = " "
val example16 = "@"
val example17 = "ki"

val example18 = "kyō"
val example19 = "tō"
val example20= "gyu"
val example21 = "gyū"
val example22 = "tta"
val example23 = "ggyū"
val example24 = "ttō"
val example25 = "tōkyō"
val example26 = "tōkyōkyōto"
val example27 = "tō#kyō"
val example28 = "tō#$kyō"
val example29 = "osaka"
val example30 = "arasuka"
val example31 = "a"
val example32 = "o"
val example33 = "おさか"
val example34 = "アラスカ"
val example35 ="さ"
val example36 ="ji"
val example37 ="じ"
val example38 ="shasshin"
val example39 ="しゃっしん"
val example40 ="っしゃべる"
val example41 ="sshabemasu"
val example42 ="jjinkusu"
val example43 ="っじんくす"
val example44 ="ジャク"
val example45 ="jaku"
val example46 ="リーグ・オブ・レジェンド"
val example47 ="じょじょ"
val example48 ="jojo"
val example49 ="jōjō"
val example50 ="じょうじょう"
val example51 ="おすしがたべたいです"
val example52 ="ぎんこ"
val example53 ="かわいいです"
val example54 ="聞きた"
val example55 ="キキ"
val example56 ="大きな"
val example57 ="東京"
val example58 ="って"
val example59 ="見つけよう"//kuromoji error
val example60 ="みつけよう"
val example61 ="シッテ"



Syllable.swYoon("ジャク")
Syllable.sWIC("ジャク")
Syllable.sWSmallY("ャク")

//Kana.allKanaAndAllKanaExtendedVowelsAndConsonantsM

/*
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
printDebug(example12)
printDebug(example13)
printDebug(example14)
printDebug(example15)
printDebug(example16)
printDebug(example17)
printDebug(example18)
printDebug(example19)
printDebug(example20)
printDebug(example21)
printDebug(example22)
printDebug(example23)
printDebug(example24)
printDebug(example25)
printDebug(example26)
printDebug(example27)
printDebug(example28)
printDebug(example29)
printDebug(example30)
printDebug(example31)
printDebug(example32)
printDebug(example33)
printDebug(example34)
printDebug(example35)
printDebug(example36)
printDebug(example37)
printDebug(example38)
printDebug(example39)
printDebug(example40)
printDebug(example41)
printDebug(example42)
printDebug(example43)
printDebug(example44)
printDebug(example45)
printDebug(example46)
printDebug(example47)
printDebug(example48)
printDebug(example49)
printDebug(example50)
printDebug(example51)
printDebug(example52)
printDebug(example53)
printDebug(example54)
printDebug(example55)

printDebug(example56)
printDebug(example57)
printDebug(example58)
printDebug(example59)
printDebug(example60)
printDebug(example61)
*/
val example62 =" ミナサン ワ ニッポン ノ ヨッツ ノ オーキナ シマ ノ ナマエ ヲ シッテ イ マス カ 。"
val example63 =" ッテ"
val example64 =" ッ"
val example65 =" シッテ "
val example66 =" ナマエ ヲ シッテ イ マス カ 。"

printDebug(example62)
printDebug(example63)
printDebug(example64)
printDebug(example65)
printDebug(example66)

//---TODO: Correct issues with ッ at the end of a token --接続助詞 connective particles
//---TODO: Add non-calculated fields in Kana
//---TODO: Write tests
//---TODO: Publish
