import com.atilika.kuromoji.ipadic.{Token, Tokenizer}
import sjt._
import sjt.JapaneseSyntax._
import sjt.JapaneseInstances._
import collection.JavaConverters._
import scala.annotation.tailrec
/*
sjt.LeKana.hiragana
sjt.LeKana.katakana
sjt.LeKana.romaji

sjt.LeKana.allRomajiEC
sjt.LeKana.allKatakanaEC
sjt.LeKana.allHiraganaEC

sjt.LeKana.allKatakanaEV
sjt.LeKana.allHiraganaEV
sjt.LeKana.allRomajiEV

sjt.LeKana.allKatakanaECEV
sjt.LeKana.allHiraganaECEV
sjt.LeKana.allRomajiECEV

sjt.LeKana.everyKanaSyllable

*/
sjt.LeKana.allKana
//LeKana.hiragana
val cachedTokenizer = new Tokenizer()

val t = 'c'

t.isHiragana
import JapaneseInstances._
import JapaneseSyntax._


@tailrec
def splitIntoSyllables(input: String, l: List[(LeKana, String)] = Nil): List[(LeKana,String)] = {
  val nS = LeKana.nextSyllable(input)
  if (input.isEmpty) l
  else splitIntoSyllables(input.drop(nS._2.length), l.::(nS))
}

val r1 = splitIntoSyllables("ごく")
val r2 = splitIntoSyllables("ろそこ")
val r3 = splitIntoSyllables("しゃく")
val r4 = splitIntoSyllables("しょうしょう")
val r5 = splitIntoSyllables("ジョジョ")
val r6 = splitIntoSyllables("ジョージョー")
val r7 = splitIntoSyllables("ろんだk")

LeKana.toRomaji(r6)
LeKana.toRomaji(r3)
LeKana.toRomaji(r7)
LeKana.toKatakana(r7)
LeKana.toHiragana(r7)


"ろんだk".toHiragana(cachedTokenizer)
"ろんだk".toKatakana(cachedTokenizer)
//splitIntoSyllables(" オ スシ ガ タベ タイ")
" オ スシ ガ タベ タイ".toRomaji(cachedTokenizer)
splitIntoSyllables("とうきょう")
//" オ スシ ガ タベ タイ".toRomaji(cachedTokenizer)

Japanese.isHiragana('c')
'j'.isHiragana
'j'.isKatakana
'カ'.isKatakana
//Kana.allKatakanaToHiraganaM // doesn't have a single ン -> ん and has an incorrect ンー -> ん
//Kana.allHiraganaToKatakanaM // doesn't have a single ン -> ん and has an incorrect ん   -> ンー
//Kana.allKatakanaToRomajiM// has ja
//Kana.allKatakanaToRomajiFilteredM //has ja

//Kana.allKanaFilteredUnprefered //has ja
//Kana.allHiraganaToKatakanaFilteredM //doesn't have ja. Its lost inbetween these two

//Kana.preferedHiM //has wrong ju
//Kana.preferedKaM
//Kana.preferedKanaToRomajiM
"ジャ".isHiragana
"ャ".isHiragana
"ジ".isHiragana
Kana.allKanaAndAllKanaExtendedVowelsAndConsonantsM //has hyo
Kana.allKanaToRomajiM //has hyo
Syllable.kanaSilableToRomaji("じょ")
Syllable.kanaSilableToRomaji("ジョ")
Syllable.romajiToHiragana("jo")
Syllable.romajiToKatakana("jo")
Syllable.katakanaToHiragana("ジョ")
Syllable.hiraganaToKatakana("じょ")
Syllable.hiraganaToKatakana("ジョ")
Syllable.katakanaToHiragana("ヂョ")
Syllable.katakanaToHiragana("ジャ")

//Kana.allRomajiToKatakanaM.get("jo").getOrElse("jo") //romaji to katakana
//Kana.allKatakanaToHiraganaM.get("ジョ").getOrElse("jo")
"ジョジョ".toKatakana(cachedTokenizer)
Syllable.katakanaToHiragana("ジョ")
" オ スシ ガ タベ タイ".toRomaji(cachedTokenizer)


//Kana.allHiraganaToRomajiM
//Kana.allKatakanaToRomajiM
/*
//val exampleString = "お寿司が食べたい"
//val exampleString2 = "おすしがたべたいです"
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
*/
import sjt._
import sjt.JapaneseSyntax._
import sjt.JapaneseInstances._
"東京".toKatakana(cachedTokenizer)
"ッ".toHiragana(cachedTokenizer)
"jōjō".toKatakana(cachedTokenizer)
printDebug("kiki")
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
printDebug("じょじょ")
printDebug("目標")
printDebug("ヒョー")
/*
transliterateAll("聞く",cachedTokenizer)
transliterateAll("大きな",cachedTokenizer)
transliterateAll("東京",cachedTokenizer)
transliterateAll("にっぽん",cachedTokenizer)
transliterateAll("日本",cachedTokenizer)
transliterateAll("ニッポン",cachedTokenizer)
transliterateAll("ン",cachedTokenizer)
transliterateAll("お寿司が食べたい",cachedTokenizer)

val example54 ="聞く"
val example56 ="大きな"
val example57 ="東京"
val example59 ="見つけよう"//kuromoji error
//Kana.allKanaAndAllKanaExtendedVowelsAndConsonantsM
printDebug(example54)
printDebug(example56)
printDebug(example57)
printDebug(example59)
*/
//Kana.allKanaToRomajiM
//Kana.allRomajiToKanaM
//Kana.allHiraganaToRomajiM//has wrong ju
//Kana.allKanaToRomajiM //has wrong ju

//Kana.allKanaAndAllKanaExtendedVowelsAndConsonantsM //has wrong
//Kana.allKanaAndExtendedConsonantM// has wrong ju
//Kana.allKanaAndExtendedConsonantM
//Kana.allKatakanaToHiraganaM//no has ja
//Kana.unpreferedKaTsM
//Kana.preferedHiM
//Kana.preferedKaM
//Kana.allKanaToRomajiM // has a ン -> n and a ん -> n
//Kana.allKanaToRomajiM.map { case (k, v) => (k, Syllable.kanaSilableToRomaji(k)) }
// the problem is romajiToKatakana
//Kana.allRomajiToKatakanaM // has no n -> ン, it has n -> ンー
//Kana.allKanaAndAllKanaExtendedVowelsAndConsonantsM // has ン -> n
//Kana.allKatakanaToRomajiM //also has  ン -> n

//---TODO: Write tests
//---TODO: Publish
