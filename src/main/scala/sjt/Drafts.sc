import com.atilika.kuromoji.ipadic.{Token, Tokenizer}
import sjt._

import sjt.JapaneseSyntax._
import sjt.JapaneseInstances._
import collection.JavaConverters._

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

def isShiHiragana(c: Char): Boolean = c == 'し'
def isJiHiragana(c: Char): Boolean = c == 'じ'
def isShiKatakana(c: Char): Boolean = c == 'シ'
def isJiKatakana(c: Char): Boolean = c == 'ジ'
def isShiRomaji(s: String): Boolean = s == "shi"
def isShiKana(c: Char): Boolean = isShiHiragana(c) && isShiKatakana(c)
def isJiKana(c: Char): Boolean = isJiHiragana(c) && isJiKatakana(c)
def startsWithJi(s: String): Boolean = s.headOption.isDefined && isJiKana(s.head)
def startsWithShi(s: String): Boolean = s.headOption.isDefined && isShiKana(s.head)

/* General Notes:
 K stands for Katakana, H stands for Hiragana
 i consonant means a palatalized consonant
 M stands for map to Romaji
*/

// Standard kana
//-- Vowels
val katakanaVowelM:Map[Char, Char] = Map('ア'-> 'a','エ'-> 'e','イ'-> 'i','オ'-> 'o' ,'ウ'-> 'u') //,'n' -> 'ン', ',' -> '、'
val katakanaVowels:List[Char] = katakanaVowelM.keySet.toList
def isKatakanaVowel(c:Char):Boolean = katakanaVowels.contains(c)

val hiraganaVowelM:Map[Char, Char] = Map('あ'-> 'a','え'-> 'e','い'-> 'i','お'-> 'o' ,'う'-> 'u')
val hiraganaVowels:List[Char] = hiraganaVowelM.keySet.toList
def isHiraganaVowel(c:Char):Boolean = hiraganaVowels.contains(c)

val kanaVowelsM:Map[Char, Char] = katakanaVowelM ++ hiraganaVowelM
val kanaVowels:List[Char] = kanaVowelsM.keySet.toList
def isKanaVowel(c:Char):Boolean = isHiraganaVowel(c) || isKatakanaVowel(c)

val extendedRomajiVowelsM:Map[Char,Char] = Map('a' -> 'ā', 'e' -> 'ē', 'i' -> 'ī', 'o'-> 'ō', 'u' -> 'ū')
val extendedRomajiVowels:List[Char] = extendedRomajiVowelsM.values.toList

def extendConsonant(c:Char):String= if (isKanaVowel(c)) c.toString
else if (c.isHiragana) "っ" + c
else if (c.isKatakana) "ッ" + c
else c.toString + c.toString
def extendConsonant(s:String):String = extendConsonant(s.head) + s.tail


def extendVowel(c:Char):String= if (isKanaVowel(c)) c.toString
else if (c.isHiragana) c + "う"
else if (c.isKatakana) c + "ー"
else extendedRomajiVowelsM.get(c).getOrElse(c).toString
def extendVowel(s:String):String = s.init + extendVowel(s.last)

//-- Hiragana
//---- Hiragana non-Diacritics
val nonDiacriticHM: Map[Char, String] = Map('あ' -> "a", 'か' -> "ka", 'さ' -> "sa", 'た' -> "ta", 'な' -> "na", 'は' -> "ha", 'ま' -> "ma", 'や' -> "ya", 'ら' -> "ra", 'わ' -> "wa"
  , 'い' -> "i", 'き' -> "ki", 'し' -> "shi", 'ち' -> "chi", 'に' -> "ni", 'ひ' -> "hi", 'み' -> "mi", 'り' -> "ri", 'ゐ' -> ""
  , 'う' -> "u", 'く' -> "ku", 'す' -> "su", 'つ' -> "tsu", 'ぬ' -> "nu", 'ふ' -> "fu", 'む' -> "mu", 'ゆ' -> "yu", 'る' -> "ru", '※' -> ""
  , 'え' -> "e", 'け' -> "ke", 'せ' -> "se", 'て' -> "te", 'ね' -> "ne", 'へ' -> "he", 'め' -> "me", '※' -> "ye", 'れ' -> "re", 'ゑ' -> ""
  , 'お' -> "o", 'こ' -> "ko", 'そ' -> "so", 'と' -> "to", 'の' -> "no", 'ほ' -> "ho", 'も' -> "mo", 'よ' -> "yo", 'ろ' -> "ro", 'を' -> "wo"
  , 'ん' -> "n")
//---- Hiragana Diacritics
val diacriticsHM: Map[Char, String] = Map('が' -> "ga", 'ざ' -> "sa", 'だ' -> "da", 'ば' -> "ba", 'ぱ' -> "pa"
  , 'ぎ' -> "gi", 'じ' -> "ji", 'ぢ' -> "ji", 'び' -> "bi", 'ぴ' -> "pi"
  , 'ぐ' -> "gu", 'ず' -> "zu", 'づ' -> "dzu", 'ぶ' -> "bu", 'ぷ' -> "pu"
  , 'げ' -> "ge", 'ぜ' -> "ze", 'で' -> "de", 'べ' -> "be", 'ぺ' -> "pe"
  , 'ご' -> "go", 'ぞ' -> "zo", 'ど' -> "do", 'ぼ' -> "bo", 'ぽ' -> "po")

val hiraganaM: Map[Char, String] = nonDiacriticHM ++ diacriticsHM
val hiragana: List[Char] = hiraganaM.keySet.toList

//-- Katakana
//---- Katakana non-Diacritics
val nonDiacriticKM: Map[Char, String] = Map('ア' -> "a", 'カ' -> "ka", 'サ' -> "sa", 'タ' -> "ta", 'ナ' -> "na", 'ハ' -> "ha", 'マ' -> "ma", 'ヤ' -> "ya", 'ラ' -> "ra", 'ワ' -> "wa"
  , 'イ' -> "i", 'キ' -> "ki", 'シ' -> "shi", 'チ' -> "chi", 'ニ' -> "ni", 'ヒ' -> "hi", 'ミ' -> "mi", 'リ' -> "ri", 'ヰ' -> ""
  , 'ウ' -> "u", 'ク' -> "ku", 'ス' -> "su", 'ツ' -> "tsu", 'ヌ' -> "nu", 'フ' -> "fu", 'ム' -> "mu", 'ユ' -> "yu", 'ル' -> "ru", '※' -> ""
  , 'エ' -> "e", 'ケ' -> "ke", 'セ' -> "se", 'テ' -> "te", 'ネ' -> "ne", 'ヘ' -> "he", 'メ' -> "me", '※' -> "ye", 'レ' -> "re", 'ゑ' -> ""
  , 'オ' -> "o", 'コ' -> "ko", 'ソ' -> "so", 'ト' -> "to", 'ノ' -> "no", 'ホ' -> "ho", 'モ' -> "mo", 'ヨ' -> "yo", 'ロ' -> "ro", 'ヲ' -> "wo"
  , 'ン' -> "n")
val nonDiacriticKatakana: List[Char] = nonDiacriticKM.keySet.toList
//---- Katakana Diacritics
val diacriticsKM: Map[Char, String] = Map('ガ' -> "ga", 'ザ' -> "sa", 'ダ' -> "da", 'バ' -> "ba", 'パ' -> "pa"
  , 'ギ' -> "gi", 'ジ' -> "ji", 'ヂ' -> "ji", 'ビ' -> "bi", 'ピ' -> "pi"
  , 'グ' -> "gu", 'ズ' -> "zu", 'ヅ' -> "dzu", 'ブ' -> "bu", 'プ' -> "pu"
  , 'ゲ' -> "ge", 'ゼ' -> "ze", 'デ' -> "de", 'ベ' -> "be", 'ペ' -> "pe"
  , 'ゴ' -> "go", 'ゾ' -> "zo", 'ド' -> "do", 'ボ' -> "bo", 'ポ' -> "po")
val diacriticKatakana: List[Char] = diacriticsKM.keySet.toList

val katakanaM: Map[Char, String] = nonDiacriticKM ++ diacriticsKM
val katakana: List[Char] = katakanaM.keySet.toList

//-- Kana
val diacriticKanaM: Map[Char,String] = diacriticsKM ++ diacriticsHM
val diacriticKana: List[Char] = diacriticKanaM.keySet.toList
val nonDiacriticKanaM: Map[Char,String] = nonDiacriticKM ++ nonDiacriticHM
val nonDiacriticKana: List[Char] = nonDiacriticKanaM.keySet.toList

val kanaM: Map[Char, String] = hiraganaM ++ katakanaM
val kana: List[Char] = kanaM.keySet.toList

// Yoon
// --Small Ys
// --Hiragana
val smallHiraganaYsM: Map[Char, String]= Map('ゃ' -> "ya" , 'ゅ'->"yu" ,'ょ'->"yo" )
val smallHiraganaYs: List[Char]= smallHiraganaYsM.keySet.toList //List('ゃ' , 'ゅ' ,'ょ')
def isHiraganaSmallY(c: Char): Boolean = smallHiraganaYs.contains(c) //= c == 'ゃ' || c == 'ゅ' || c == 'ょ'
// --Katakana
val smallKatakanaYsM: Map[Char,String]= Map('ャ'->"ya" , 'ュ'->"yu" , 'ョ'->"yo" )
val smallKatakanaYs: List[Char]= smallKatakanaYsM.keySet.toList //List('ャ', 'ュ', 'ョ')
def isKatakanaSmallY(c: Char): Boolean = smallHiraganaYs.contains(c) //c == 'ャ' || c == 'ュ' || c == 'ョ'
// --General
def isSmallY(c: Char): Boolean = isHiraganaSmallY(c) || isKatakanaSmallY(c)
val smallYsM: Map[Char,String]= smallHiraganaYsM ++ smallKatakanaYsM
val smallYs: List[Char]= smallYsM.keySet.toList

// --I consonants
// --I consonants : non-diacritics
val iNormalConsonantsHM: Map[Char, String] = Map('き'->"ki",'し'->"shi",'ち'->"chi",'に'->"ni",'ひ'->"hi", 'み'->"mi", 'り'->"ri")
val iConsonantnonDiacriticHs: List[Char] = iNormalConsonantsHM.keySet.toList

val iNormalConsonantsKM: Map[Char,String] = Map('ギ'->"ki",'シ'->"shi",'チ'->"chi",'ニ'->"ni",'ヒ'->"hi", 'ミ'->"mi", 'リ'->"ri")
val iConsonantnonDiacriticKs: List[Char] = iNormalConsonantsKM.keySet.toList

val iConsonantnonDiacritics: List[Char] = iConsonantnonDiacriticHs ++ iConsonantnonDiacriticKs

// --I consonants : diacritics
def isIConsonantDiacriticH(c: Char):Boolean = c == 'ぎ' || c == 'じ' || c == 'ぢ' || c == 'び' || c == 'ぴ'
val iConsonantDiacriticsHM: Map[Char, String] = Map('ぎ'->"gi",'じ'->"ji",'ぢ'->"ji",'び'->"bi",'ぴ'->"pi")
val iConsonantDiacriticsH = iConsonantDiacriticsHM.keySet.toList

def isIConsonantDiacriticK(c: Char) = c == 'ギ' || c == 'ジ' || c == 'ヂ' || c == 'ビ' || c == 'ピ'
val iConsonantDiacriticsKM: Map[Char, String] = Map('ギ'->"gi",'ジ'->"ji",'ヂ'->"ji",'ビ'->"bi",'ピ'->"pi")
val iConsonantDiacriticsK = iConsonantDiacriticsKM.keySet.toList

def isIConsonantDiacritic(c: Char) = isIConsonantDiacriticH(c) || isIConsonantDiacriticK(c)
val iConsonantsDiacritic = iConsonantDiacriticsH ++ iConsonantDiacriticsK

// --I consonants : all - palatalized consonants
val iConsonantsHM: Map[Char, String] =iNormalConsonantsHM ++ iConsonantDiacriticsHM
val iConsonantsH: List[Char] =iConsonantsHM.keySet.toList

val iConsonantsKM: Map[Char, String] =iNormalConsonantsKM ++ iConsonantDiacriticsKM
val iConsonantsK: List[Char] = iConsonantsKM.keySet.toList
val iConsonantsM:Map[Char, String] = iConsonantsHM ++ iConsonantsKM
val iConsonants:List[Char] = iConsonantsM.keySet.toList

// -- Dyphthongs
// REVISE THESE 3
def makeDiphthongH(iConsonantH:Char, smallYH:Char) =iConsonantsHM.get(iConsonantH).getOrElse(iConsonantH.toString).toCharArray.head + smallYsM.get(smallYH).getOrElse(smallYH.toString)
def makeDiphthongK(iConsonantK:Char, smallYK:Char) =iConsonantsKM.get(iConsonantK).getOrElse(iConsonantK.toString).toCharArray.head + smallYsM.get(smallYK).getOrElse(smallYK.toString)
def makeDiphthong(iConsonant:Char, smallY:Char) = if (iConsonant.isHiragana) makeDiphthongH(iConsonant,smallY) else makeDiphthongK(iConsonant,smallY)


val stdDiphthongableHs: List[Char] = iNormalConsonantsHM.keySet.toList diff List('し', 'ち')
val stdDiphthongableKs: List[Char] = iNormalConsonantsKM.keySet.toList diff List('シ', 'チ')
val stdDiphthongableDiacriticsHs: List[Char] = iConsonantDiacriticsHM.keySet.toList diff List('し', 'ち')
val stdDiphthongableDiacriticsKs: List[Char] = iConsonantDiacriticsKM.keySet.toList diff List('シ', 'チ')

val stdYoonM: Map[String, String] = {for { x <- stdDiphthongableHs; y <- smallHiraganaYs }
                                            yield (x.toString+y.toString -> makeDiphthong(x,y))}.toMap

val stdYooniacriticsHM: Map[String, String] = {for { x <- stdDiphthongableDiacriticsHs; y <- smallHiraganaYs }
                                                      yield (x.toString+y.toString -> makeDiphthong(x,y))}.toMap

val nonStdYoonHM: Map[String, String] = Map("じ"-> "ji", "ぢ" -> "ji"
                                                  ,"しゃ"-> "sha","しゅ"-> "shu","しょ"-> "sho"
                                                  ,"ちゃ"-> "sha","ちゅ"-> "chu","ちょ"-> "sho"
                                                  ,"じゃ"-> "ja","じゅ"-> "ju","じょ"-> "jo"
                                                  ,"ぢゃ"-> "ja","ぢゅ"-> "ju","ぢょ"-> "jo")
val yoonHiraganaM:Map[String,String] = stdYoonM ++ nonStdYoonHM ++ stdYooniacriticsHM
val yoonHiragana:List[String] = yoonHiraganaM.keySet.toList

val stdYoonKatakanaM: Map[String, String] = {for { x <- stdDiphthongableKs; y <- smallKatakanaYs }
                                              yield (x.toString+y.toString -> makeDiphthong(x,y))}.toMap

val stdYoonKatakanaDiacriticsM: Map[String, String] = {for { x <- stdDiphthongableDiacriticsKs; y <- smallKatakanaYs }
                                                        yield (x.toString+y.toString -> makeDiphthong(x,y))}.toMap

val nonStdYoonKatakanaM: Map[String, String] = Map("ジ"-> "ji", "ヂ" -> "ji"
                                                  ,"シャ"-> "sha","シュ"-> "shu","ショ"-> "sho"
                                                  ,"チャ"-> "sha","チュ"-> "chu","チョ"-> "sho"
                                                  ,"ジャ"-> "ja","ジュ"-> "ju","ジョ"-> "jo"
                                                  ,"ヂャ"-> "ja","ヂュ"-> "ju","ヂョ"-> "jo")

val yoonKatakanaM:Map[String,String] = stdYoonKatakanaM ++ nonStdYoonKatakanaM ++ stdYoonKatakanaDiacriticsM
val yoonKatakana:List[String] = yoonKatakanaM.keySet.toList

val yoonKanaM = yoonKatakanaM ++ yoonHiraganaM
val yoonKana = yoonKanaM.keySet.toList

// Wrap up
val allKanaM:Map[String, String] = kanaM.map{case (k,v) => (k.toString, v)} ++ yoonKanaM
val allKana:List[String] = allKanaM.keySet.toList

//-- Extensions: Consonant & Vowel
val allKanaAndExtendedConsonantM: Map[String, String] = allKanaM ++ allKanaM.map{case (k,v) => (extendConsonant(k),v)}
val allKanaAndExtendedVowelsM: Map[String, String] = allKanaM ++ allKanaM.map{case (k,v) => (extendVowel(k),v)}

val allKanaAndAllKanaExtendedVowelsAndConsonantsM: Map[String, String] = (allKanaAndExtendedConsonantM ++ allKanaAndExtendedConsonantM.map{case (k,v) => (extendVowel(k),v)}) ++
                                                                allKanaAndExtendedVowelsM ++ allKanaAndExtendedVowelsM.map{case (k,v) => (extendConsonant(k),v)}

val allRomajiAndAllRomajiExtendedVowelsAndConsonantsM: Map[String, String] =allKanaAndAllKanaExtendedVowelsAndConsonantsM.map { case (k, v) => (v, k) }

def kanaSilableToRomaji(s: String): String = allKanaAndAllKanaExtendedVowelsAndConsonantsM.get(s).getOrElse(s.toString)
def romajiSilableToHiragana(s: String): String = allRomajiAndAllRomajiExtendedVowelsAndConsonantsM.get(s).getOrElse(s.toString)

def kanaToRomaji(c: Char): String = kanaM.get(c).getOrElse(c.toString)
def kanaToRomaji(s:String):String = ???


// Matching
def isPalatalizedHiraganaConsonant(c: Char): Boolean = c == 'き' || c == 'し' || c == 'ち' || c == 'に' || c == 'ひ' || c == 'み' || c == 'り'
def isPalatalizedKatakanaConsonant(c: Char): Boolean = c == 'キ' || c == 'シ' || c == 'チ' || c == 'ニ' || c == 'ヒ' || c == 'ミ' || c == 'リ'
def isPalatalizedConsonant(c: Char): Boolean = isPalatalizedHiraganaConsonant(c) || isPalatalizedKatakanaConsonant(c) || isIConsonantDiacritic(c)

def startsWithPalatalizedConsonant(s: String): Boolean = s.headOption.isDefined && isPalatalizedConsonant(s.head)
def isDigraph(s: String) = (s.length == 2 || s.length == 3) && startsWithPalatalizedConsonant(s) && isSmallY(s.charAt(0 + 1))
def isDigraphPlusVowel(s: String) = (s.length == 3) && isDigraph(s.take(2)) && isVowel(s.last)

def isExtendedVowelSillabousDiagraphPlusVowel(s: String) = (s.length == 3) && isDigraphPlusVowel(s)
def startsWithExtendedVowelSillabousDiagraphPlusVowel(s: String) = (s.length >= 3) && isExtendedVowelSillabousDiagraphPlusVowel(s.take(3))
def isSimpleExtendedVowelSillabous(s: String) = (s.length == 2) && isConsonant(s.head) && ((s.last.isHiragana && isU(s.last)) || (s.last.isKatakana && isKatakanaExtensionVowel(s.last)))
def isExtendedVowelSillabous(s: String) = isSimpleExtendedVowelSillabous(s) || isExtendedVowelSillabousDiagraphPlusVowel(s)

def isKatakanaExtensionVowel(c: Char) = c == 'ー'
def isU(value: Char): Boolean = value == 'う' || value == 'ウ'
def isHiraganaExtensionVowel(value: Char): Boolean = isU(value)
def isExtensionVowel(c:Char) = isHiraganaExtensionVowel(c) || isKatakanaExtensionVowel(c)

def isConsonant(value: Char): Boolean = value.isKana && !isVowel(value)
def startsWithSimpleExtendedVowelSillabous(s: String): Boolean = s.length >= 2 && isSimpleExtendedVowelSillabous(s.take(2))
def startsWithExtendedVowelSillabous(s: String): Boolean = startsWithSimpleExtendedVowelSillabous(s) || startsWithExtendedVowelSillabousDiagraphPlusVowel(s)


def isSmallTsuHiragana(value: Char): Boolean = value == 'っ'
def isSmallTsuKatakana(value: Char): Boolean = value == 'ッ'
def isSmallTsu(value: Char): Boolean = isSmallTsuHiragana(value) || isSmallTsuKatakana(value)
def isExtendedConsonantSillabous(s: String) = (s.length == 2) && isSmallTsu(s.head) && isConsonant(s.last)
def isExtendedConsonantSillabousVowel(s: String) = (s.length == 3) && isExtendedConsonantSillabous(s.init) && isExtensionVowel(s.last)
def isExtendedConsonantPalatalizedVowel(s: String) = (s.length == 4) && isSmallTsu(s.head) && isPalatalizedConsonant(s(1)) && isSmallY(s(2)) && isExtensionVowel(s.last)

def startsWithExtendedConsonantSillabous(s: String) = (s.length >= 2) && isExtendedConsonantSillabous(s.take(2))
def startsWithExtendedConsonantSillabousVowel(s: String) = (s.length >= 3) && isExtendedConsonantSillabousVowel(s.take(3))
def startsWithExtendedConsonantPalatalizedNVowel(s: String) = (s.length >= 4) && isExtendedConsonantPalatalizedVowel(s.take(4))


def isVowel(value: Char): Boolean = isLatinVowel(value) || isHiraganaVowel(value) || isKatakanaVowel(value) || isKatakanaMiniVowel(value)
def isLatinVowel(value: Char): Boolean = value == 'a' || value == 'e' || value == 'i' || value == 'o' || value == 'u'
//def isHiraganaVowel(value: Char): Boolean = value == 'あ' || value == 'え' || value == 'い' || value == 'お' || value == 'う'
//def isKatakanaVowel(value: Char): Boolean = value == 'ア' || value == 'エ' || value == 'イ' || value == 'オ' || value == 'ウ'
def isKatakanaMiniVowel(value: Char): Boolean = value == 'ェ' || value == 'ョ'

def extendChar(value:Char):Char = Map('a' -> 'ā', 'e' -> 'ē', 'i' -> 'ī', 'o'-> 'ō', 'u' -> 'ū').get(value).getOrElse(value)


def tryAllFor(s: String): Unit = {
  println("---------------------------")
  println(s"$s starts with palatalized consonant: " + startsWithPalatalizedConsonant(s))
  println(s"$s starts with extended sillabous: " + startsWithSimpleExtendedVowelSillabous(s))
  println(s"$s starts With extended diagraph: " + startsWithExtendedVowelSillabousDiagraphPlusVowel(s))
  println(s"$s starts With extended consonant sillabous: " + startsWithExtendedConsonantSillabous(s))
  println(s"$s starts With extended consonant sillabous vowel: " + startsWithExtendedConsonantSillabousVowel(s))
  println(s"$s starts With extended palatalized and vowel: " + startsWithExtendedConsonantPalatalizedNVowel(s))
  println(s"$s to romaji: " + kanaSilableToRomaji(s))

  println("---------------------------")

}

//There problem is im joining the wrong maps

val example1 = "きょう" //error
val example2 = "とう"
val example3 = "ぎゅ"//error
val example4 = "ぎゅう"//error
val example5 = "った"
val example6 = "っぎゅう"
val example7 = "っとう"

tryAllFor(example1)
tryAllFor(example2)
tryAllFor(example3)
tryAllFor(example4)
tryAllFor(example5)
tryAllFor(example6)
tryAllFor(example7)

sealed trait Kana{

}
final case class NotKana() extends Kana //@$#!
final case class SimpleSilable() extends Kana //と
final case class ExtendedConsonant() extends Kana //った
final case class ExtendedVowel() extends Kana //とう
final case class ExtendedConsonantNVowel() extends Kana //っとう
final case class Diafrag() extends Kana //palatelized: きゃ
final case class DiafragEV() extends Kana //きゅう
final case class DiafragEC() extends Kana //っきゅ
final case class DiafragECEV() extends Kana//っきゅう

//string => recursive starts with to return list of Kana and fold into a string
