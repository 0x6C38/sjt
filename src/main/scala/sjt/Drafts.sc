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

def isHiraganaSmallY(c: Char): Boolean = c == 'ゃ' || c == 'ゅ' || c == 'ょ'
def isKatakanaSmallY(c: Char): Boolean = c == 'ャ' || c == 'ュ' || c == 'ョ'
def isSmallY(c: Char): Boolean = isHiraganaSmallY(c) || isKatakanaSmallY(c)
val smallHiraganaYList: List[Char]= List('ゃ' , 'ゅ' ,'ょ')
val smallHiraganaYM: Map[Char, String]= Map('ゃ' -> "ya" , 'ゅ'->"yu" ,'ょ'->"yo" )
val smallKatakanaYList: List[Char]= List('ャ', 'ュ', 'ョ')
val smallKatakanaYM: Map[Char,String]= Map('ャ'->"ya" , 'ュ'->"yu" , 'ョ'->"yo" )
val smallYList: List[Char]= smallHiraganaYList ++ smallKatakanaYList
val smallYM: Map[Char,String]= smallHiraganaYM ++ smallKatakanaYM

def isYoonableHiraganaPalatalizedDiacritic(c: Char) = c == 'ぎ' || c == 'じ' || c == 'ぢ' || c == 'び' || c == 'ぴ'
val palatalizedHiraganaDiacriticsM: Map[Char, String] = Map('ぎ'->"ki",'じ'->"shi",'ぢ'->"chi",'び'->"ni",'ぴ'->"hi")
val palatalizedHiraganaDiacritics = palatalizedHiraganaDiacriticsM.keySet.toList


val yoonablePalatalizedKatakanaDiacriticsM: Map[Char, String] = Map('ギ'->"ki",'ジ'->"shi",'ヂ'->"chi",'ビ'->"ni",'ピ'->"hi")
def isYoonableKatakanaPalatalizedDiacritic(c: Char) = c == 'ギ' || c == 'ジ' || c == 'ヂ' || c == 'ビ' || c == 'ピ'
val yoonablePalatalizedKatakanaDiacritics = yoonablePalatalizedKatakanaDiacriticsM.keySet.toList

def isPalatalizedDiacritic(c: Char) = isYoonableHiraganaPalatalizedDiacritic(c) || isYoonableKatakanaPalatalizedDiacritic(c)
val palatalizedDiacritics = palatalizedHiraganaDiacritics ++ yoonablePalatalizedKatakanaDiacritics

val palatalizedHiraganaM: Map[Char, String] = Map('き'->"ki",'し'->"shi",'ち'->"chi",'に'->"ni",'ひ'->"hi", 'み'->"mi", 'り'->"ri")
val palatalizedDiacriticsHiraganaM: Map[Char, String] = Map('ぎ'->"gi",'び'->"bi",'ぴ'->"pi")
val palatalizedAllHiraganaM: Map[Char, String] =palatalizedHiraganaM ++ palatalizedDiacriticsHiraganaM
val palatalizedDiacriticsKatakanaM: Map[Char, String] = Map('ギ'->"gi",'ビ'->"bi",'ピ'->"pi")
val palatalizedKatakanaM: Map[Char,String] = Map('ギ'->"ki",'シ'->"shi",'チ'->"chi",'ニ'->"ni",'ヒ'->"hi", 'ミ'->"mi", 'リ'->"ri")
val palatalizedAllKatakanaM: Map[Char, String] =palatalizedKatakanaM ++ palatalizedDiacriticsKatakanaM


def makeHyatusHiragana(palatelizedC:Char, smallY:Char) =palatalizedAllHiraganaM.get(palatelizedC).getOrElse(palatelizedC.toString).toCharArray.head + smallYM.get(smallY).getOrElse(smallY.toString).toString
def makeHyatusKatakana(palatelizedC:Char, smallY:Char) =palatalizedAllKatakanaM.get(palatelizedC).getOrElse(palatelizedC.toString).toCharArray.head + smallYM.get(smallY).getOrElse(smallY.toString).toString
def makeHyatus(palatelizedC:Char, smallY:Char) = if (palatelizedC.isHiragana) makeHyatusHiragana(palatelizedC,smallY) else makeHyatusKatakana(palatelizedC,smallY)


val stdPalatalizedHiragana: List[Char] = palatalizedHiraganaM.keySet.toList diff List('し', 'ち')
val stdPalatalizedKatakana: List[Char] = palatalizedKatakanaM.keySet.toList diff List('シ', 'チ')
val stdPalatalizedHiraganaDiacritics: List[Char] = palatalizedDiacriticsHiraganaM.keySet.toList diff List('し', 'ち')
val stdPalatalizedKatakanaDiacritics: List[Char] = palatalizedDiacriticsKatakanaM.keySet.toList diff List('シ', 'チ')

val stdYoonHiragana: Map[String, String] =
  {for { x <- stdPalatalizedHiragana; y <- smallHiraganaYList }
    yield (x.toString+y.toString -> makeHyatus(x,y))}.toMap

val stdYoonHiraganaDiacritics: Map[String, String] =
  {for { x <- stdPalatalizedHiraganaDiacritics; y <- smallHiraganaYList }
  yield (x.toString+y.toString -> makeHyatus(x,y))}.toMap
val nonStdYoonHiragana: Map[String, String] = Map("じ"-> "ji", "ぢ" -> "ji"
                                                  ,"しゃ"-> "sha","しゅ"-> "shu","しょ"-> "sho"
                                                  ,"ちゃ"-> "sha","ちゅ"-> "chu","ちょ"-> "sho"
                                                  ,"じゃ"-> "ja","じゅ"-> "ju","じょ"-> "jo"
                                                  ,"ぢゃ"-> "ja","ぢゅ"-> "ju","ぢょ"-> "jo")
val yoonHiragana:Map[String,String] = stdYoonHiragana ++ nonStdYoonHiragana ++ stdYoonHiraganaDiacritics

val stdYoonKatakana: Map[String, String] =
  {for { x <- stdPalatalizedKatakana; y <- smallKatakanaYList }
    yield (x.toString+y.toString -> makeHyatus(x,y))}.toMap

val stdYoonKatakanaDiacritics: Map[String, String] =
  {for { x <- stdPalatalizedKatakanaDiacritics; y <- smallKatakanaYList }
    yield (x.toString+y.toString -> makeHyatus(x,y))}.toMap
val nonStdYoonKatakana: Map[String, String] = Map("ジ"-> "ji", "ヂ" -> "ji"
                                                  ,"シャ"-> "sha","シュ"-> "shu","ショ"-> "sho"
                                                  ,"チャ"-> "sha","チュ"-> "chu","チョ"-> "sho"
                                                  ,"ジャ"-> "ja","ジュ"-> "ju","ジョ"-> "jo"
                                                  ,"ヂャ"-> "ja","ヂュ"-> "ju","ヂョ"-> "jo")

val yoonKatakana:Map[String,String] = stdYoonKatakana ++ nonStdYoonKatakana ++ stdYoonKatakanaDiacritics

val yoonKana = yoonKatakana ++ yoonHiragana

/*
val stdPalatalizedHiraganaM: Map[String, String] = for { x <- stdPalatalizedHiragana; y <- smallHiraganaYList }
  yield (x.toString+y.toString -> (if (palatalizedHiraganaM.get(x).isDefined) palatalizedHiraganaM.get(x).getOrElse(x.toString).head+smallYM.get(y).getOrElse(y.toString) else x.toString)).groupBy(_._1).map { case (k,v) => (k,v.map(_._2))}
  */
val palatalizedHiragana: List[Char] = palatalizedHiraganaM.keySet.toList

val palatalizedKatakana: List[Char] = palatalizedKatakanaM.keySet.toList
val palatalizedKana: List[Char] = palatalizedHiragana ++ palatalizedKatakana

/*
val yoonHiraganaMap: Map[String, String] = for { x <- stdPalatalizedHiragana; y <- smallHiraganaYList } yield (Map(x.toString+y.toString)) ++ nonStdPalatalizedHiraganaM
val yoonHiraganaList: List[String] = (for { x <- palatalizedHiraganaM; y <- smallHiraganaYList } yield (x.toString+y.toString)).toList
val yoonKatakanaList: List[String] = for { x <- palatalizedKatakana; y <- smallKatakanaYList } yield (x.toString+y.toString)
val yoonHiraganaDiacriticsList: List[String] = for { x <- palatalizedHiraganaDiacritics; y <- smallKatakanaYList } yield (x.toString+y.toString)
val yoonKatakanaDiacriticsList: List[String] = for { x <- yoonablePalatalizedKatakanaDiacritics; y <- smallKatakanaYList } yield (x.toString+y.toString)
val yoonList: List[String] = yoonHiraganaList ++ yoonKatakanaList ++ yoonHiraganaDiacriticsList ++ yoonKatakanaDiacriticsList
println(yoonList)
*/
def isPalatalizedHiraganaConsonant(c: Char): Boolean = c == 'き' || c == 'し' || c == 'ち' || c == 'に' || c == 'ひ' || c == 'み' || c == 'り'
def isPalatalizedKatakanaConsonant(c: Char): Boolean = c == 'キ' || c == 'シ' || c == 'チ' || c == 'ニ' || c == 'ヒ' || c == 'ミ' || c == 'リ'
def isPalatalizedConsonant(c: Char): Boolean = isPalatalizedHiraganaConsonant(c) || isPalatalizedKatakanaConsonant(c) || isPalatalizedDiacritic(c)

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
def isHiraganaVowel(value: Char): Boolean = value == 'あ' || value == 'え' || value == 'い' || value == 'お' || value == 'う'
def isKatakanaVowel(value: Char): Boolean = value == 'ア' || value == 'エ' || value == 'イ' || value == 'オ' || value == 'ウ'
def isKatakanaMiniVowel(value: Char): Boolean = value == 'ェ' || value == 'ョ'

def extendChar(value:Char):Char = Map('a' -> 'ā', 'e' -> 'ē', 'i' -> 'ī', 'o'-> 'ō', 'u' -> 'ū').get(value).getOrElse(value)

val hiraganaSToRomajiM: Map[Char, String] = Map('あ' -> "a", 'か' -> "ka", 'さ' -> "sa", 'た' -> "ta", 'な' -> "na", 'は' -> "ha", 'ま' -> "ma", 'や' -> "ya", 'ら' -> "ra", 'わ' -> "wa"
  , 'い' -> "i", 'き' -> "ki", 'し' -> "shi", 'ち' -> "chi", 'に' -> "ni", 'ひ' -> "hi", 'み' -> "mi", 'り' -> "ri", 'ゐ' -> ""
  , 'う' -> "u", 'く' -> "ku", 'す' -> "su", 'つ' -> "tsu", 'ぬ' -> "nu", 'ふ' -> "fu", 'む' -> "mu", 'ゆ' -> "yu", 'る' -> "ru", '※' -> ""
  , 'え' -> "e", 'け' -> "ke", 'せ' -> "se", 'て' -> "te", 'ね' -> "ne", 'へ' -> "he", 'め' -> "me", '※' -> "ye", 'れ' -> "re", 'ゑ' -> ""
  , 'お' -> "o", 'こ' -> "ko", 'そ' -> "so", 'と' -> "to", 'の' -> "no", 'ほ' -> "ho", 'も' -> "mo", 'よ' -> "yo", 'ろ' -> "ro", 'を' -> "wo"
  , 'ん' -> "n")


val katakanaSToRomajiM: Map[Char, String] = Map('ア' -> "a", 'カ' -> "ka", 'サ' -> "sa", 'タ' -> "ta", 'ナ' -> "na", 'ハ' -> "ha", 'マ' -> "ma", 'ヤ' -> "ya", 'ラ' -> "ra", 'ワ' -> "wa"
  , 'イ' -> "i", 'キ' -> "ki", 'シ' -> "shi", 'チ' -> "chi", 'ニ' -> "ni", 'ヒ' -> "hi", 'ミ' -> "mi", 'リ' -> "ri", 'ヰ' -> ""
  , 'ウ' -> "u", 'ク' -> "ku", 'ス' -> "su", 'ツ' -> "tsu", 'ヌ' -> "nu", 'フ' -> "fu", 'ム' -> "mu", 'ユ' -> "yu", 'ル' -> "ru", '※' -> ""
  , 'エ' -> "e", 'ケ' -> "ke", 'セ' -> "se", 'テ' -> "te", 'ネ' -> "ne", 'ヘ' -> "he", 'メ' -> "me", '※' -> "ye", 'レ' -> "re", 'ゑ' -> ""
  , 'オ' -> "o", 'コ' -> "ko", 'ソ' -> "so", 'ト' -> "to", 'ノ' -> "no", 'ホ' -> "ho", 'モ' -> "mo", 'ヨ' -> "yo", 'ロ' -> "ro", 'ヲ' -> "wo"
  , 'ン' -> "n")

val exhiraganaDiacriticsToRomajiM: Map[Char, String] = Map('が' -> "ga", 'ざ' -> "sa", 'だ' -> "da", 'ば' -> "ba", 'ぱ' -> "pa"
  , 'ぎ' -> "gi", 'じ' -> "ji", 'ぢ' -> "ji", 'び' -> "bi", 'ぴ' -> "pi"
  , 'ぐ' -> "gu", 'ず' -> "zu", 'づ' -> "dzu", 'ぶ' -> "bu", 'ぷ' -> "pu"
  , 'げ' -> "ge", 'ぜ' -> "ze", 'で' -> "de", 'べ' -> "be", 'ぺ' -> "pe"
  , 'ご' -> "go", 'ぞ' -> "zo", 'ど' -> "do", 'ぼ' -> "bo", 'ぽ' -> "po")


val hiraganaDiacriticsToRomajiM: Map[Char, String] = Map('が' -> "ga", 'ざ' -> "sa", 'だ' -> "da", 'ば' -> "ba", 'ぱ' -> "pa"
  , 'ぎ' -> "gi", 'じ' -> "ji", 'ぢ' -> "ji", 'び' -> "bi", 'ぴ' -> "pi"
  , 'ぐ' -> "gu", 'ず' -> "zu", 'づ' -> "dzu", 'ぶ' -> "bu", 'ぷ' -> "pu"
  , 'げ' -> "ge", 'ぜ' -> "ze", 'で' -> "de", 'べ' -> "be", 'ぺ' -> "pe"
  , 'ご' -> "go", 'ぞ' -> "zo", 'ど' -> "do", 'ぼ' -> "bo", 'ぽ' -> "po")
exhiraganaDiacriticsToRomajiM.size
hiraganaDiacriticsToRomajiM.size

val katakanaDiacriticsToRomajiM: Map[Char, String] = Map('ガ' -> "ga", 'ザ' -> "sa", 'ダ' -> "da", 'バ' -> "ba", 'パ' -> "pa"
  , 'ギ' -> "gi", 'ジ' -> "ji", 'ヂ' -> "ji", 'ビ' -> "bi", 'ピ' -> "pi"
  , 'グ' -> "gu", 'ズ' -> "zu", 'ヅ' -> "dzu", 'ブ' -> "bu", 'プ' -> "pu"
  , 'ゲ' -> "ge", 'ゼ' -> "ze", 'デ' -> "de", 'ベ' -> "be", 'ペ' -> "pe"
  , 'ゴ' -> "go", 'ゾ' -> "zo", 'ド' -> "do", 'ボ' -> "bo", 'ポ' -> "po")

val diacriticsToRomajiM: Map[Char,String] = katakanaDiacriticsToRomajiM ++ hiraganaDiacriticsToRomajiM

val hiraganaToRomajiM: Map[Char, String] = hiraganaSToRomajiM ++ hiraganaDiacriticsToRomajiM
val katakanaToRomajiM: Map[Char, String] = katakanaSToRomajiM ++ katakanaDiacriticsToRomajiM
val kanaToRomajiM: Map[Char, String] = hiraganaToRomajiM ++ katakanaToRomajiM

val hiraganaExtendedConsonantYoonToRomajiM: Map[String,String] =  yoonHiragana.map{case (k, v) => (("っ" + k), v.toCharArray.headOption.getOrElse("") + v)}
val katakanaExtendedConsonantYoonToRomajiM: Map[String,String] =  yoonKatakana.map{case (k, v) => (("ッ" + k), v.toCharArray.headOption.getOrElse("") + v)}

val hiraganaExtendedConsonantToRomajiM: Map[String, String] = hiraganaToRomajiM.filter{case(k,v) => isConsonant(k)}.map{case (k, v) => (("っ" + k.toString), v.toCharArray.headOption.getOrElse("") + v)}
val hiraganaExtendedConsonantBlocksToRomajiM: Map[String, String] = hiraganaToRomajiM.map{case(k,v) => (k.toString, v.toString)} ++ hiraganaExtendedConsonantToRomajiM

val katakanaExtendedConsonantToRomajiM: Map[String, String] = katakanaToRomajiM.filter{case(k,v) => isConsonant(k)}.map{case (k, v) => (("ッ" + k.toString), v.toCharArray.headOption.getOrElse("") + v)}
val katakanaExtendedConsonantBlocksToRomajiM: Map[String, String] = katakanaToRomajiM.map{case(k,v) => (k.toString, v.toString)} ++ katakanaExtendedConsonantToRomajiM

val hiraganaExtendedConsonantYoonBlockToRomajiM: Map[String,String] =  hiraganaExtendedConsonantYoonToRomajiM ++ hiraganaExtendedConsonantToRomajiM
val katakanaExtendedConsonantYoonBlockToRomajiM: Map[String,String] =  katakanaExtendedConsonantYoonToRomajiM++ katakanaExtendedConsonantToRomajiM

val hiraganaExtendedConsonantNYoonBlocksToRomajiM: Map[String,String] =  hiraganaExtendedConsonantYoonBlockToRomajiM ++ hiraganaExtendedConsonantToRomajiM
val katakanaExtendedConsonantNYoonBlocksToRomajiM: Map[String,String] =  katakanaExtendedConsonantYoonBlockToRomajiM ++ katakanaExtendedConsonantToRomajiM

val kanaExtendedConsonantToRomajiM: Map[String, String] = hiraganaExtendedConsonantToRomajiM ++ katakanaExtendedConsonantToRomajiM
val kanaExtendedConsonantBlocksToRomajiM: Map[String, String] = kanaToRomajiM.map{case(k,v) => (k.toString, v.toString)} ++ kanaExtendedConsonantToRomajiM

val hiraganaExtendedConsonantNVowelToRomajiM: Map[String, String] = hiraganaExtendedConsonantBlocksToRomajiM ++ hiraganaExtendedConsonantBlocksToRomajiM.map{case (k, v) => ((k.toString + "う"), if (v.toCharArray.lastOption.isDefined) v.init + extendChar(v.toCharArray.last).toString else v)}
val katakanaExtendedConsonantNVowelToRomajiM: Map[String, String] = katakanaExtendedConsonantBlocksToRomajiM ++ katakanaExtendedConsonantBlocksToRomajiM.map{case (k, v) => ((k.toString + "ー"), if (v.toCharArray.lastOption.isDefined) v.init + extendChar(v.toCharArray.last).toString else v)}
val kanaExtendedConsonantNVowelToRomajiM: Map[String, String] = hiraganaExtendedConsonantNVowelToRomajiM ++ katakanaExtendedConsonantNVowelToRomajiM

val hiraganaExtendedYoonConsonantNVowelToRomajiM: Map[String, String] = hiraganaExtendedConsonantYoonBlockToRomajiM ++ hiraganaExtendedConsonantYoonBlockToRomajiM.map{case (k, v) => ((k.toString + "う"), if (v.toCharArray.lastOption.isDefined) v.init + extendChar(v.toCharArray.last).toString else v)}
val katakanaExtendedYoonConsonantNVowelToRomajiM: Map[String, String] = katakanaExtendedConsonantYoonBlockToRomajiM ++ katakanaExtendedConsonantYoonBlockToRomajiM.map{case (k, v) => ((k.toString + "ー"), if (v.toCharArray.lastOption.isDefined) v.init + extendChar(v.toCharArray.last).toString else v)}
val kanaExtendedYoonConsonantNVowelToRomajiM: Map[String, String] = hiraganaExtendedYoonConsonantNVowelToRomajiM ++ katakanaExtendedYoonConsonantNVowelToRomajiM

val romajiToKanaM: Map[String, Char] = kanaToRomajiM.map { case (k, v) => (v, k) }

def kanaSilableToRomaji(s: String): String = kanaExtendedYoonConsonantNVowelToRomajiM.get(s).getOrElse(s.toString)
def kanaToRomaji(c: Char): String = kanaToRomajiM.get(c).getOrElse(c.toString)
def romajiToHiragana(s: String): String = { // ??
  val ro = romajiToKanaM.get(s)
  if (ro.isDefined) ro.toString else s
}

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