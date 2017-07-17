package sjt
import JapaneseInstances._
import JapaneseSyntax._

import scala.annotation.tailrec

/* General Notes:
  K stands for Katakana, H stands for Hiragana
  i consonant means a palatalized consonant
  M stands for map to Romaji
*/
object Kana{

  // Standard kana
  //-- Vowels
  val katakanaVowelM:Map[Char, Char] = Map('ア'-> 'a','エ'-> 'e','イ'-> 'i','オ'-> 'o' ,'ウ'-> 'u') //,'n' -> 'ン', ',' -> '、'
  val katakanaVowels:List[Char] = katakanaVowelM.keySet.toList
  def isKatakanaVowel(c:Char):Boolean = katakanaVowels.contains(c)

  val hiraganaVowelM:Map[Char, Char] = Map('あ'-> 'a','え'-> 'e','い'-> 'i','お'-> 'o' ,'う'-> 'u')
  val hiraganaVowels:List[Char] = hiraganaVowelM.keySet.toList
  def isHiraganaVowel(c:Char):Boolean = hiraganaVowels.contains(c)

  val romajiVowelM:Map[Char, Char] = Map('a'-> 'あ','e'-> 'え','i'-> 'い','o'-> 'お' ,'u'-> 'う')
  val romajiVowels:List[Char] = romajiVowelM.keySet.toList
  def isRomajiVowel(c:Char):Boolean = romajiVowelM.contains(c)

  val extendedRomajiVowelsM:Map[Char,Char] = Map('a' -> 'ā', 'e' -> 'ē', 'i' -> 'ī', 'o'-> 'ō', 'u' -> 'ū')
  val extendedRomajiVowels:List[Char] = extendedRomajiVowelsM.values.toList
  def isRomajiExtendedVowel(c:Char):Boolean = extendedRomajiVowels.contains(c)

  val kanaVowelsM:Map[Char, Char] = katakanaVowelM ++ hiraganaVowelM
  val kanaVowels:List[Char] = kanaVowelsM.keySet.toList
  def isKanaVowel(c:Char):Boolean = isHiraganaVowel(c) || isKatakanaVowel(c)

  //-- Consonants
  val hiraganaConsonantExtension = 'っ'
  val katakanaConsonantExtension = 'ッ'
  def isHiraganaConsonantExtension(c:Char) = c == hiraganaConsonantExtension
  def isKatakanaConsonantExtension(c:Char) = c == katakanaConsonantExtension
  def isConsonantExtension(c:Char) = isHiraganaConsonantExtension(c) || isKatakanaConsonantExtension(c)

  val hiraganaVowelExtension = 'う'
  val katakanaVowelExtension = 'ー'
  def isHiraganaVowelExtension(c:Char) = c == hiraganaVowelExtension
  def isKatakanaVowelExtension(c:Char) = c == katakanaVowelExtension
  def isVowelExtension(c:Char) = isHiraganaVowelExtension(c) || isKatakanaVowelExtension(c)

  def extendConsonant(c:Char):String= if (isKanaVowel(c)) c.toString
  else if (c.isHiragana) "っ" + c
  else if (c.isKatakana) "ッ" + c
  else c.toString + c.toString
  def extendConsonant(s:String):String = if (s.headOption.isDefined) extendConsonant(s.head) + s.tail else s


  def extendVowel(c:Char):String= if (c.isHiragana) c + "う"
  else if (c.isKatakana) c + "ー"
  else extendedRomajiVowelsM.get(c).getOrElse(c).toString
  def extendVowel(s:String):String = if (!s.isEmpty) s.init + extendVowel(s.last) else s

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
  val romajiM: Map[String, Char] = kanaM.map { case (k, v) => (v, k) }
  val romaji: List[String] = romajiM.keySet.toList
  val kana: List[Char] = kanaM.keySet.toList

  val kanaConsonants:List[Char] = kana diff kanaVowels
  val romajiConsonants:List[String] = romaji diff romajiVowels.map{_.toString}
  def isKanaConsonant(c:Char) = kanaConsonants.contains(c)
  def isRomajiConsonant(c:Char) = romajiConsonants.exists(r => r == c.toString)
  def isRomajiConsonant(s:String) = romajiConsonants.contains(s)

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
  def isIConsonantH(c:Char): Boolean =iConsonantsH.contains(c)

  val iConsonantsKM: Map[Char, String] =iNormalConsonantsKM ++ iConsonantDiacriticsKM
  val iConsonantsK: List[Char] = iConsonantsKM.keySet.toList
  def isIConsonantK(c:Char): Boolean =iConsonantsK.contains(c)

  val iConsonantsRM: Map[String, Char] =iConsonantsHM.map{case (k,v) => (v,k)}
  val iConsonantsR: List[String] = iConsonantsRM.keySet.toList
  def isIConsonantR(c:Char): Boolean = false
  def isIConsonantR(s:String): Boolean = !s.isEmpty && iConsonantsR.contains(s)

  val iConsonantsM:Map[Char, String] = iConsonantsHM ++ iConsonantsKM
  val iConsonants:List[Char] = iConsonantsM.keySet.toList
  def isIConsonant(c:Char):Boolean = iConsonants.contains(c)

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

  val yoonRomajiM:Map[String,String] = yoonHiraganaM.map{case (k,v) => (v,k)}
  val yoonRomaji:List[String] = yoonRomajiM.keySet.toList
  def isYoonRomaji(s:String):Boolean = s.length >= 2 && yoonRomaji.contains(s)

  val yoonKanaM = yoonKatakanaM ++ yoonHiraganaM
  val yoonKana = yoonKanaM.keySet.toList

  // Wrap up
  val allKanaM:Map[String, String] = kanaM.map{case (k,v) => (k.toString, v)} ++ yoonKanaM
  val allKana:List[String] = allKanaM.keySet.toList

  //-- Extensions: Consonant & Vowel
  val extendedVowelYoonKanaM:Map[String,String] = yoonKanaM.map{case (k,v) => (extendVowel(k),extendVowel(v))}
  val extendedVowelYoonKana:List[String] = extendedVowelYoonKanaM.keySet.toList
  def isExtendedVowelYoonKana(s:String):Boolean = s != "" && extendedVowelYoonKana.contains(s)

  val extendedVowelYoonRomajiM:Map[String,String] = extendedVowelYoonKanaM.map{case (k,v) => (v,k)}
  val extendedVowelYoonRomaji:List[String] = extendedVowelYoonRomajiM.keySet.toList
  def isExtendedVowelYoonRomaji(s:String):Boolean = s != "" && extendedVowelYoonRomaji.contains(s)

  val allKanaAndExtendedConsonantM: Map[String, String] = allKanaM ++ allKanaM.map{case (k,v) => (extendConsonant(k),extendConsonant(v))}
  val allKanaAndExtendedVowelsM: Map[String, String] = allKanaM ++ allKanaM.map{case (k,v) => (extendVowel(k),extendVowel(v))}

  val allKanaAndAllKanaExtendedVowelsAndConsonantsM: Map[String, String] = (allKanaAndExtendedConsonantM ++ allKanaAndExtendedConsonantM.map{case (k,v) => (extendVowel(k),extendVowel(v))}) ++
    allKanaAndExtendedVowelsM ++ allKanaAndExtendedVowelsM.map{case (k,v) => (extendConsonant(k),extendConsonant(v))}

  val allRomajiAndAllRomajiExtendedVowelsAndConsonantsM: Map[String, String] =allKanaAndAllKanaExtendedVowelsAndConsonantsM.map { case (k, v) => (v, k) }

  def kanaSilableToRomaji(s: String): String = allKanaAndAllKanaExtendedVowelsAndConsonantsM.get(s).getOrElse(s.toString)
  def romajiSilableToHiragana(s: String): String = allRomajiAndAllRomajiExtendedVowelsAndConsonantsM.get(s).getOrElse(s.toString)


  @tailrec
  def splitIntoSyllables(input: String, l: List[Syllable]): List[Syllable] = {
    val nS = Syllable.nextSyllable(input)
    if (input.isEmpty) l
    else splitIntoSyllables(input.drop(nS.text.length), nS :: l)
  }
  def kanaToRomaji(c: Char): String = kanaM.get(c).getOrElse(c.toString)
  def kanaToRomaji(s: String): String = {

    splitIntoSyllables(s, List[Syllable]()).reverse.foldLeft("")((a, s) => a + Kana.kanaSilableToRomaji(s.text))
  }
  def romajiToHiragana(s:String):String={
    splitIntoSyllables(s, List[Syllable]()).reverse.foldLeft("")((a, s) => a + Kana.romajiSilableToHiragana(s.text))
  }
}

sealed abstract class Syllable(val text:String)
object Syllable{
  // Matching
  //C = Consonant; EC = Extended Consonant; IC = i-Consonant = Palatalized Consonant
  //V = Vowel; EV = Extended Vowel; sW = starts With
  def sWC(s: String): Boolean = s != "" && ((s.isHiragana || s.isKatakana && s.headOption.isDefined && Kana.isKanaConsonant(s.head)) || (s.headOption.isDefined && s.head == 'n') || (s.length >= 2 && Kana.isRomajiConsonant(s.take(2)))) //(1) / (2)
  def sWIC(s: String): Boolean = s.headOption.isDefined && Kana.isIConsonant(s.head) || (s.length >= 2 && Kana.isIConsonantR(s.take(2)))//(1) / (2)
  def sWSmallY(s: String): Boolean = s.headOption.isDefined && Kana.isSmallY(s.head) //(1)
  def swYoon(s: String): Boolean = (s.length >= 2 && sWIC(s) && sWSmallY(s.tail)) || (Kana.isYoonRomaji(s.take(3))) //(2) / (2-3)
  def swEVYoon(s: String): Boolean = (s.length >= 2 && Kana.isExtendedVowelYoonRomaji(s.take(3)) || Kana.isExtendedVowelYoonRomaji(s.take(2))) || (s.length >= 3 && swYoon(s) && Kana.isVowelExtension(s(2))) //(3) / (2-3)
  def swECYoon(s: String): Boolean = (s.length >= 3 && swEIC(s) && Kana.isSmallY(s(2))) || (s.length >=2 && s.head == s(1) && swYoon(s.tail)) //(3) / (2) // bug?? swEIC(s)
  def swECYoonEV(s: String): Boolean = s.length >= 3 && swECYoon(s) && swEVYoon(s.tail) //(4) / 3 //it was set to length>=4 before

  def swEC(s: String): Boolean = (s.length >= 2 && Kana.isConsonantExtension(s.head) && sWC(s.tail)) || (s.length >= 2 && s.head == s(1))  //(2) / (2)
  def swEIC(s: String): Boolean = swEC(s) && sWIC(s.tail) //(2) / (2)?
  def swEV(s: String): Boolean = (s.length >= 2 && Kana.isKanaConsonant(s.head) && Kana.isVowelExtension(s(1))) || (s.length >= 2 && Kana.isKanaConsonant(s.head) && Kana.isRomajiExtendedVowel(s(1))) //(2)
  def swECEV(s: String): Boolean = s.length >= 3 && swEC(s) && sWC(s.tail) && swEV(s.tail) // (3) / (3) ?

  def nextSyllable(s: String): Syllable = {
    if (swECYoonEV(s)) YoonECEV(s.take(4))
    else if (swECEV(s)) ExtendedConsonantNVowel(s.take(3))
    else if (swECYoon(s)) YoonEC(s.take(3))
    else if (swEVYoon(s)) YoonEV(s.take(3))
    else if (swEV(s)) ExtendedVowel(s.take(2))
    else if (swEIC(s)) ExtendedConsonant(s.take(2)) //make a new class for starts with iC?
    else if (swYoon(s)) Yoon(s.take(2))
    else if (sWSmallY(s)) SimpleSilable(s.take(1)) //should these 3 even be taken into consideration?
    else if (sWIC(s)) SimpleSilable(s.take(1))
    else if (sWC(s)) SimpleSilable(s.take(1))
    else NotSyllable(s.take(1))
  }

}
final case class NotSyllable(override val text:String) extends Syllable(text) //@$#!
final case class SimpleSilable(override val text:String) extends Syllable(text) //と
final case class ExtendedConsonant(override val text:String) extends Syllable(text) //った
final case class ExtendedVowel(override val text:String) extends Syllable(text) //とう
final case class ExtendedConsonantNVowel(override val text:String) extends Syllable(text) //っとう
final case class Yoon(override val text:String) extends Syllable(text) //palatelized: きゃ
final case class YoonEV(override val text:String) extends Syllable(text) //きゅう
final case class YoonEC(override val text:String) extends Syllable(text) //っきゅ
final case class YoonECEV(override val text:String) extends Syllable(text) //っきゅう