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

def isHiraganaPalatalizedDiacritic(c: Char) = c == 'ぎ' || c == 'じ' || c == 'ぢ' || c == 'び' || c == 'ぴ'
def isKatakanaPalatalizedDiacritic(c: Char) = c == 'ギ' || c == 'ジ' || c == 'ヂ' || c == 'ビ' || c == 'ピ'
def isPalatalizedDiacritic(c: Char) = isHiraganaPalatalizedDiacritic(c) || isKatakanaPalatalizedDiacritic(c)

def isPalatalizedHiraganaConsonant(c: Char): Boolean = c == 'き' || c == 'し' || c == 'ち' || c == 'に' || c == 'ひ' || c == 'み' || c == 'り'
def isPalatalizedKatakanaConsonant(c: Char): Boolean = c == 'キ' || c == 'シ' || c == 'チ' || c == 'ニ' || c == 'ヒ' || c == 'ミ' || c == 'リ'
def isPalatalizedConsonant(c: Char): Boolean = isPalatalizedHiraganaConsonant(c) || isPalatalizedKatakanaConsonant(c) || isPalatalizedDiacritic(c)

def startsWithPalatalizedConsonant(s: String): Boolean = s.headOption.isDefined && isPalatalizedConsonant(s.head)
def isDigraph(s: String) = (s.length == 2 || s.length == 3) && startsWithPalatalizedConsonant(s) && isSmallY(s.charAt(0 + 1))
def isDigraphPlusVowel(s: String) = (s.length == 3) && isDigraph(s.take(2)) && isVowel(s.last)

def isExtendedVowelSillabousDiagraphPlusVowel(s: String) = (s.length == 3) && isDigraphPlusVowel(s)
def startsWithExtendedVowelSillabousDiagraphPlusVowel(s: String) = (s.length >= 3) && isExtendedVowelSillabousDiagraphPlusVowel(s.take(3))
def isSimpleExtendedVowelSillabous(s: String) = (s.length == 2) && isConsonant(s.head) && ((s.last.isHiragana && isU(s.last)) || (s.last.isKatakana && isKatakanaExtension(s.last)))
def isExtendedVowelSillabous(s: String) = isSimpleExtendedVowelSillabous(s) || isExtendedVowelSillabousDiagraphPlusVowel(s)

def isKatakanaExtension(c: Char) = c == 'ー'
def isU(value: Char): Boolean = value == 'う' || value == 'ウ'
def isConsonant(value: Char): Boolean = value.isKana && !isVowel(value)
def startsWithSimpleExtendedVowelSillabous(s: String): Boolean = s.length >= 2 && isSimpleExtendedVowelSillabous(s.take(2))
def startsWithExtendedVowelSillabous(s: String): Boolean = startsWithSimpleExtendedVowelSillabous(s) || startsWithExtendedVowelSillabousDiagraphPlusVowel(s)


def isSmallTsuHiragana(value: Char): Boolean = value == 'っ'
def isSmallTsuKatakana(value: Char): Boolean = value == 'ッ'
def isSmallTsu(value: Char): Boolean = isSmallTsuHiragana(value) || isSmallTsuKatakana(value)
def isExtendedConsonantSillabous(s: String) = (s.length == 2) && isSmallTsu(s.head) && isConsonant(s.last)
def isExtendedConsonantSillabousVowel(s: String) = (s.length == 3) && isExtendedConsonantSillabous(s) && isVowel(s.last)

// regular sillabous?
//make sillabous AST?
//toSillabousArray??
//complete mapping of sillabous to romaji?

def isVowel(value: Char): Boolean = isLatinVowel(value) || isHiraganaVowel(value) || isKatakanaVowel(value) || isKatakanaMiniVowel(value)
def isLatinVowel(value: Char): Boolean = value == 'a' || value == 'e' || value == 'i' || value == 'o' || value == 'u'
def isHiraganaVowel(value: Char): Boolean = value == 'あ' || value == 'え' || value == 'い' || value == 'お' || value == 'う'
def isKatakanaVowel(value: Char): Boolean = value == 'ア' || value == 'エ' || value == 'イ' || value == 'オ' || value == 'ウ'
def isKatakanaMiniVowel(value: Char): Boolean = value == 'ェ' || value == 'ョ'



val example1 = "きょう"
val example2 = "とう"
val example3 = "ぎゅ"
val example4 = "ぎゅう"

tryAllFor(example1)
tryAllFor(example2)
tryAllFor(example3)
tryAllFor(example4)


isDigraphPlusVowel(example4)


def tryAllFor(s: String): Unit = {
  println("---------------------------")
  println(s"$s starts with palatalized consonant: " + startsWithPalatalizedConsonant(s))
  println(s"$s starts with extended sillabous: " + startsWithSimpleExtendedVowelSillabous(s))
  println(s"$s starts With extended diagraph: " + startsWithExtendedVowelSillabousDiagraphPlusVowel(s))
  println("---------------------------")

}




