import com.atilika.kuromoji.ipadic.Tokenizer
import org.scalatest._
import sjt.JapaneseInstances._
import sjt.JapaneseSyntax._
import sjt._

class KanaContents extends FlatSpec with Matchers {

  "Kana.hiragana" must "contain simple hiragana letters" in {
    Kana.hiragana should contain("は")
    Kana.hiragana should contain("り")
    Kana.hiragana should contain("く")
    Kana.hiragana should contain("な")
  }

  it must "only contain simple hiragana letters" in {
    Kana.hiragana.forall(_.isHiragana) should be(true)
  }

  "Kana.katakana" must "contain simple katakana letters" in {
    Kana.katakana should contain("ハ")
    Kana.katakana should contain("リ")
    Kana.katakana should contain("ク")
    Kana.katakana should contain("ナ")
  }

  it must "only contain simple katakana letters" in {
    Kana.katakana.forall(_.isKatakana) should be(true)
  }

  "Kana.yoon" must "contain only syllables formed by ya/yo/yu" in {
    Kana.yoon.forall(yoon => yoon.hiragana.contains("ゃ") || yoon.hiragana.contains("ょ") || yoon.hiragana.contains("ゅ") || yoon.hiragana.contains("ぇ")) should be(true)
    Kana.yoon.forall(yoon => yoon.katakana.contains("ャ") || yoon.katakana.contains("ョ") || yoon.katakana.contains("ュ")|| yoon.katakana.contains("ェ")) should be(true)
  }

  it must "be consistant in which ya/yo/yu is used in both hiragana and katakana" in {
    Kana.yoon.forall(yoon => (yoon.hiragana.contains("ゃ") && yoon.katakana.contains("ャ")) || (yoon.hiragana.contains("ょ") && yoon.katakana.contains("ョ")) || (yoon.hiragana.contains("ゅ") && yoon.katakana.contains("ュ")) || (yoon.hiragana.contains("ぇ") && yoon.katakana.contains("ェ"))) should be(true)
  }

  "Kana.allHiraganaEV" must "contain only hiragana with extended vowels" in {
    Kana.allHiraganaEV.forall(hiraganaEV => hiraganaEV.contains("う") && hiraganaEV.isHiragana) should be(true)
  }

  "Kana.allKatakanaEV" must "contain only katakana with extended vowels" in {
    Kana.allKatakanaEV.forall(katakanaEV => (katakanaEV.contains("ー") && katakanaEV.isKatakana)) should be(true)
  }

  "Kana.allHiraganaEC" must "contain only hiragana with extended consonants" in {
    Kana.allHiraganaEC.forall(hiraganaEC => (hiraganaEC.contains("っ") && hiraganaEC.isHiragana && hiraganaEC.length > 1)) should be(true)
  }

  "Kana.allKatakanaEC" must "contain only katakana with extended consonants" in {
    Kana.allKatakanaEC.forall(katakanaEC => (katakanaEC.contains("ッ") && katakanaEC.isKatakana && katakanaEC.length > 1)) should be(true)
  }

  "Kana.allHiraganaECEV" must "contain only hiragana with extended consonants and vowels" in {
    Kana.allHiraganaECEV.forall(hiraganaECEV => (hiraganaECEV.contains("っ") && hiraganaECEV.contains("う") && hiraganaECEV.isHiragana && hiraganaECEV.length > 2)) should be(true)
  }

  "Kana.allKatakanaECEV" must "contain only katakana with extended consonants and vowels" in {
    Kana.allKatakanaECEV.forall(katakanaECEV => (katakanaECEV.contains("ッ") && katakanaECEV.contains("ー") && katakanaECEV.isKatakana && katakanaECEV.length > 2)) should be(true)
  }

}

