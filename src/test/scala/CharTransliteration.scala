import org.scalatest._
import sjt.JapaneseInstances._
import sjt.JapaneseSyntax._

class CharTransliteration extends FlatSpec with Matchers{
  it should "correctly transliterate from hiragana to hiragana" in {
    'ろ'.toHiragana() should be("ろ")
    'さ'.toHiragana() should be("さ")
  }
  it should "correctly transliterate from katakana to hiragana" in {
    'ナ'.toHiragana() should be ("な")
    'ッ'.toHiragana() should be ("っ")
  }
  it should "correctly transliterate from romaji to hiragana" in {
    'n'.toHiragana() should be ("ん")
    'o'.toHiragana() should be ("お")
    'j'.toHiragana() should be ("j")
  }

  it should "correctly transliterate from hiragana to katakana" in {
    'す'.toKatakana() should be('ス')
    'ほ'.toKatakana() should be("ホ")
  }
  it should "correctly transliterate from katakana to katakana" in {
    'ホ'.toKatakana() should be("ホ")
    'ス'.toKatakana() should be("ス")
  }
  it should "correctly transliterate from romaji to katakana" in {
    'a'.toKatakana() should be("ア")
    'e'.toKatakana() should be("エ")
    'n'.toKatakana() should be("ン")
    't'.toKatakana() should be("t")
  }
  "Chars" should "correctly transliterate from hiragana to romaji" in {
    'す'.toRomaji() should be("su")
    'な'.toRomaji() should be("na")
    'ろ'.toHiragana() should be("ro")
    'さ'.toHiragana() should be("sa")
  }
  it should "correctly transliterate from katakana to romaji" in {
    'ス'.toRomaji() should be ("su")

    'ッ'.toRomaji() should be ("ッ")
    'ー'.toRomaji() should be ("ー")
  }
  it should "correctly transliterate from romaji to romaji" in {
    'n'.toRomaji() should be ("n")
    'o'.toRomaji() should be ("o")
    'k'.toRomaji() should be ("k")

    '#'.toRomaji() should be ("#")
  }


}
