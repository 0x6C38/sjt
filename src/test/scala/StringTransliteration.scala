import org.scalatest._
import sjt.JapaneseInstances._
import sjt.JapaneseSyntax._

class StringTransliteration extends FlatSpec with Matchers{
  "Strings" should "correctly transliterate from hiragana to hiragana" in {
    "ぎゅうにゅう".toHiragana() should be("ぎゅうにゅう")
    "おすしがたべたいです".toHiragana() should be("おすしがたべたいです")
    "わたしはかわいいです".toHiragana() should be("わたしはかわいいです")
    "こと".toHiragana() should be("こと")
    "ぎんこ".toHiragana() should be("ぎんこ")
  }
  it should "correctly transliterate from katakana to hiragana" in {
    "じょじょ".toHiragana() should be("ジョジョ")
    "じょうじょう".toHiragana() should be("ジョージョー")
    "じゃく".toHiragana() should be("ジャク")
    "しって".toHiragana() should be("シッテ")
    "ナマエ ヲ シッテ イ マス カ".toHiragana() should be("なまえ を しって い ます か")
  }
  it should "correctly transliterate from romaji to hiragana" in {
    "kyō".toHiragana() should be("きょう")
    "tō".toHiragana() should be("とう")
    "gyu".toHiragana() should be("ぎゅ")
    "gyū".toHiragana() should be("ぎゅう")
    "tta".toHiragana() should be("った")
    "ggyū".toHiragana() should be("っぎゅう")
    "ttō".toHiragana() should be("っと")
    "tōkyō".toHiragana() should be("とうきょう")
    "tōkyōkyōto".toHiragana() should be("とうきょうきょうと")
    "ji".toHiragana() should be("じ")
    "shasshin".toHiragana() should be("しゃっしん")
    "sshaberu".toHiragana() should be("っしゃべる")
    "jjinkusu".toHiragana() should be("っじんくす")

    "jojo".toHiragana() should be("じょじょ")
    "jōjō".toHiragana() should be("じょうじょう")
    "shitte".toHiragana() should be("しって")
    "tte".toHiragana() should be("って")
    "tō#$kyō".toHiragana() should be("とう#$きょう")

  }

  it should "correctly transliterate from hiragana to katakana" in {
    "おさか".toKatakana() should be("アラスカ")
    "あ".toKatakana() should be("ア")
    "お".toKatakana() should be("オ")
    "っじんくす".toKatakana() should be("ッジンクス")
    "じゃく".toKatakana() should be("ジャク")
    "ぎんこ".toKatakana() should be("ギンコ")
    "きき".toKatakana() should be("キキ")

  }
  it should "correctly transliterate from katakana to katakana" in {
    "アラスカ".toKatakana() should be("オサカ")
    "ッジンクス".toKatakana() should be("ッジンクス")
  }
  it should "correctly transliterate from romaji to katakana" in {
    "arasuka".toKatakana() should be("アラスカ")
    "osaka".toKatakana() should be("オサカ")
    "a".toKatakana() should be("ア")
    "o".toKatakana() should be("オ")
    "jojo".toKatakana() should be("ジョジョ")
    "jōjō".toKatakana() should be("ジョージョー")
    "kiki".toKatakana() should be("キキ")

  }

  it should "correctly transliterate from hiragana to romaji" in {
    "わたしはかわいいです".toRomaji() should be("watashihakawaiidesu")
    "おすしがたべたいです".toRomaji() should be("osushigatabetaidesu")
    "こと".toRomaji() should be("koto")

    "と".toRomaji() should be("to")
    "とう".toRomaji() should be("tō")
    "っとう".toRomaji() should be("ttō")
    "った".toRomaji() should be("tta")
    "とうきょう".toRomaji() should be("tōkyō")
    "とうきょうきょうと".toRomaji() should be("tōkyōkyotō")

    "ぎゅ".toRomaji() should be("gyu")
    "ぎゅう".toRomaji() should be("gyū")
    "っぎゅう".toRomaji() should be("ggyū")
    "ぎゅうにゅう".toRomaji() should be("gyūnyū")
    "っぎゅうにゅう".toRomaji() should be("ggyūnyū")
    "じ".toRomaji() should be("ji")
    "しゃっしん".toRomaji() should be("shasshin")
    "っしゃべる".toRomaji() should be("sshaberu")
    "っじんくす".toRomaji() should be("jjinkusu")
    "ぎんこ".toRomaji() should be("ginko")
    "かわいいです".toRomaji() should be("kawaiidesu")
    "きき".toRomaji() should be("kiki")
    "って".toRomaji() should be("tte")
    "みつけよう".toRomaji() should be("mitsukeyō")
    "なまえ を しって い ます か".toRomaji() should be("namae wo shitte i masu ka")
    "とう#きょう".toRomaji() should be("tō#kyō")
    "とう#$きょう".toRomaji() should be("tō#$kyō")

  }
  it should "correctly transliterate from katakana to romaji" in {
    "コーヒー".toRomaji() should be("kōhī")
    "ジャク".toRomaji() should be("jaku")
    "ニャンコ".toRomaji() should be("nyanko")
    "リグオブレジェンド".toRomaji() should be("rīguoburejendo")
    "オスシガタベタイデス".toRomaji() should be("osushigatabetaidesu")
    "ワタシハカワイイデス".toRomaji() should be("watashihakawaiidesu")
    "ッジンクス".toRomaji() should be("jjinkusu")
    "ジャク".toRomaji() should be("jaku")
    "ナマエ ヲ シッテ イ マス カ".toRomaji() should be("namae wo shitte i masu ka")

  }
  it should "correctly transliterate from romaji to romaji" in {
    "rīguoburejendo".toRomaji() should be("rīguoburejendo")
    "osushigatabetaidesu".toRomaji() should be("osushigatabetaidesu")
    "watashihakawaiidesu".toRomaji() should be("watashihakawaiidesu")
    "to".toRomaji() should be("to")
    "k".toRomaji() should be("k")
    "".toRomaji() should be("")
    "@".toRomaji() should be("@")
    "ki".toRomaji() should be("ki")
  }
}
