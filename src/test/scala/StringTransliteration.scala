import com.atilika.kuromoji.ipadic.Tokenizer
import org.scalatest._
import sjt.JapaneseInstances._
import sjt.JapaneseSyntax._

class StringTransliteration extends FlatSpec with Matchers{
  val cachedTokenizer = new Tokenizer()
  "Strings" should "correctly transliterate from hiragana to hiragana" in {
    "ぎゅうにゅう".toHiragana(cachedTokenizer) should be("ぎゅうにゅう")
    "おすしがたべたいです".toHiragana(cachedTokenizer) should be("おすしがたべたいです")
    "わたしはかわいいです".toHiragana(cachedTokenizer) should be("わたしはかわいいです")
    "こと".toHiragana(cachedTokenizer) should be("こと")
    "ぎんこ".toHiragana(cachedTokenizer) should be("ぎんこ")
  }
  it should "correctly transliterate from katakana to hiragana" in {
    "ジョジョ".toHiragana(cachedTokenizer) should be("じょじょ")
    "ジョージョー".toHiragana(cachedTokenizer) should be("じょうじょう")
    "ジャク".toHiragana(cachedTokenizer) should be("じゃく")
    "シッテ".toHiragana(cachedTokenizer) should be("しって")
    "ナマエ ヲ シッテ イ マス カ".toHiragana(cachedTokenizer) should be("なまえ を しって い ます か")
    "ニッポン".toHiragana(cachedTokenizer) should be("にっぽん")
  }
  it should "correctly transliterate from romaji to hiragana" in {
    "kyō".toHiragana(cachedTokenizer) should be("きょう")
    "tō".toHiragana(cachedTokenizer) should be("とう")
    "gyu".toHiragana(cachedTokenizer) should be("ぎゅ")
    "gyū".toHiragana(cachedTokenizer) should be("ぎゅう")
    "tta".toHiragana(cachedTokenizer) should be("った")
    "ggyū".toHiragana(cachedTokenizer) should be("っぎゅう")
    "tto".toHiragana(cachedTokenizer) should be("っと")
    "ttō".toHiragana(cachedTokenizer) should be("っとう")
    "tōkyō".toHiragana(cachedTokenizer) should be("とうきょう")
    "tōkyōkyōto".toHiragana(cachedTokenizer) should be("とうきょうきょうと")
    "ji".toHiragana(cachedTokenizer) should be("じ")
    "shasshin".toHiragana(cachedTokenizer) should be("しゃっしん")
    "sshaberu".toHiragana(cachedTokenizer) should be("っしゃべる")
    "jjinkusu".toHiragana(cachedTokenizer) should be("っじんくす")
    "jojo".toHiragana(cachedTokenizer) should be("じょじょ")
    "jōjō".toHiragana(cachedTokenizer) should be("じょうじょう")
    "shitte".toHiragana(cachedTokenizer) should be("しって")
    "tte".toHiragana(cachedTokenizer) should be("って")
    "tō#$kyō".toHiragana(cachedTokenizer) should be("とう#$きょう")
  }

  it should "correctly transliterate from hiragana to katakana" in {
    "おさか".toKatakana(cachedTokenizer) should be("オサカ")
    "あ".toKatakana(cachedTokenizer) should be("ア")
    "お".toKatakana(cachedTokenizer) should be("オ")
    "っじんくす".toKatakana(cachedTokenizer) should be("ッジンクス")
    "じゃく".toKatakana(cachedTokenizer) should be("ジャク")
    "ぎんこ".toKatakana(cachedTokenizer) should be("ギンコ")
    "きき".toKatakana(cachedTokenizer) should be("キキ")
    "じょじょ".toKatakana(cachedTokenizer) should be("ジョジョ")
    "じょうじょう".toKatakana(cachedTokenizer) should be("ジョージョー")
    "じゃく".toKatakana(cachedTokenizer) should be("ジャク")
    "しって".toKatakana(cachedTokenizer) should be("シッテ")
  }

  it should "correctly transliterate from katakana to katakana" in {
    "アラスカ".toKatakana(cachedTokenizer) should be("アラスカ")
    "ッジンクス".toKatakana(cachedTokenizer) should be("ッジンクス")
  }
  it should "correctly transliterate from romaji to katakana" in {
    "arasuka".toKatakana(cachedTokenizer) should be("アラスカ")
    "osaka".toKatakana(cachedTokenizer) should be("オサカ")
    "a".toKatakana(cachedTokenizer) should be("ア")
    "o".toKatakana(cachedTokenizer) should be("オ")
    "jojo".toKatakana(cachedTokenizer) should be("ジョジョ")
    "jōjō".toKatakana(cachedTokenizer) should be("ジョージョー")
    "kiki".toKatakana(cachedTokenizer) should be("キキ")

  }

  it should "correctly transliterate from hiragana to romaji" in {
    "わたしはかわいいです".toRomaji(cachedTokenizer) should be("watashihakawaiidesu")
    "おすしがたべたいです".toRomaji(cachedTokenizer) should be("osushigatabetaidesu")
    "こと".toRomaji(cachedTokenizer) should be("koto")

    "と".toRomaji(cachedTokenizer) should be("to")
    "とう".toRomaji(cachedTokenizer) should be("tō")
    "っとう".toRomaji(cachedTokenizer) should be("ttō")
    "った".toRomaji(cachedTokenizer) should be("tta")
    "とうきょう".toRomaji(cachedTokenizer) should be("tōkyō")
    "とうきょうきょうと".toRomaji(cachedTokenizer) should be("tōkyōkyōto")

    "ぎゅ".toRomaji(cachedTokenizer) should be("gyu")
    "ぎゅう".toRomaji(cachedTokenizer) should be("gyū")
    "っぎゅう".toRomaji(cachedTokenizer) should be("ggyū")
    "ぎゅうにゅう".toRomaji(cachedTokenizer) should be("gyūnyū")
    "っぎゅうにゅう".toRomaji(cachedTokenizer) should be("ggyūnyū")
    "じ".toRomaji(cachedTokenizer) should be("ji")
    "しゃっしん".toRomaji(cachedTokenizer) should be("shasshin")
    "っしゃべる".toRomaji(cachedTokenizer) should be("sshaberu")
    "っじんくす".toRomaji(cachedTokenizer) should be("jjinkusu")
    "ぎんこ".toRomaji(cachedTokenizer) should be("ginko")
    "かわいいです".toRomaji(cachedTokenizer) should be("kawaiidesu")
    "きき".toRomaji(cachedTokenizer) should be("kiki")
    "って".toRomaji(cachedTokenizer) should be("tte")
    "みつけよう".toRomaji(cachedTokenizer) should be("mitsukeyō")
    "なまえ を しって い ます か".toRomaji() should be("namae wo shitte i masu ka")
    "とう#きょう".toRomaji(cachedTokenizer) should be("tō#kyō")
    "とう#$きょう".toRomaji(cachedTokenizer) should be("tō#$kyō")

  }
  it should "correctly transliterate from katakana to romaji" in {
    "コーヒー".toRomaji(cachedTokenizer) should be("kōhī")
    "ジャク".toRomaji(cachedTokenizer) should be("jaku")
    "ニャンコ".toRomaji(cachedTokenizer) should be("nyanko")
    "リグオブレジェンド".toRomaji(cachedTokenizer) should be("riguoburejendo")
    "オスシガタベタイデス".toRomaji(cachedTokenizer) should be("osushigatabetaidesu")
    "ワタシハカワイイデス".toRomaji(cachedTokenizer) should be("watashihakawaiidesu")
    "ッジンクス".toRomaji(cachedTokenizer) should be("jjinkusu")
    "ジャク".toRomaji(cachedTokenizer) should be("jaku")
    "ナマエ ヲ シッテ イ マス カ".toRomaji(cachedTokenizer) should be("namae wo shitte i masu ka")

  }
  it should "correctly transliterate from romaji to romaji" in {
    "rīguoburejendo".toRomaji(cachedTokenizer) should be("rīguoburejendo")
    "osushigatabetaidesu".toRomaji(cachedTokenizer) should be("osushigatabetaidesu")
    "watashihakawaiidesu".toRomaji(cachedTokenizer) should be("watashihakawaiidesu")
    "to".toRomaji(cachedTokenizer) should be("to")
    "k".toRomaji(cachedTokenizer) should be("k")
    "".toRomaji(cachedTokenizer) should be("")
    "@".toRomaji(cachedTokenizer) should be("@")
    "ki".toRomaji(cachedTokenizer) should be("ki")
  }
}
