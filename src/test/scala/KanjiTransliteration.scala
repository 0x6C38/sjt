import org.scalatest._
import sjt.JapaneseInstances._
import sjt.JapaneseSyntax._

class KanjiTransliteration extends FlatSpec with Matchers{
  val sentenceA = "お寿司が食べたい"
  val sentenceB = "診察行ってきました。最終目標は心臓移植というのは変わらないそうで、自分の心臓じゃ生きられないみたいです。お金もかかるし、手術するのも入院するのももう嫌だな～…なんてことを考えながらvitaで朧村正をプレイしてました！　牛鬼と馬鬼ってボスより強くない！？　ぜんぜん勝てないんだけど…"
  val sentenceC = "「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる"
  val sentenceD = "皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。"
  val sentenceE = "聞く"
  val sentenceF = "大きな"
  val sentenceG = "東京"
  //val sentenceZ = "見つけよう" //Bug: Kuromoji error

  "Kanji" should "correctly transliterate to romaji" in {
    sentenceA.toRomaji() should be("o sushi ga tabe tai")
    //sentenceB.toRomaji() should be("shinsatsu itte ki mashi ta. saishū mokuhyō wa shinzō ishoku toyuu no wa kawara nai sō de, jibun no shinzō ja iki rare nai mitai desu. okane mo kakarushi, shujutsu suru no mo nyūin suru no mo mō iya da na ～… nante koto wo kangaenagara vita de oboro mura sei wo purei shite mashi ta! 　 ushi oni to uma oni tte bosu yori tsuyoku nai!? 　 zenzen kate nai n dakedo …")
    //sentenceC.toRomaji() should be("「 ima 」 okite iru koto wo mitsukeyo u. kokunai no nyūsu kara mijika na deki goto made, minna no wadai ga wakaru 「 ima 」 okite iru koto wo mitsukeyo u. kokunai no nyūsu kara mijika na deki goto made, minna no wadai ga wakaru")
    //sentenceD.toRomaji() should be("minasan wa nippon no yottsu no ōkina shima no namae wo shitte i masu ka. nippon ni wa tōkyō no yō na, sekai ni yoku shira rete iru toshi ga takusan ari masuga, minasan wa donna toshi namae wo kiki ta koto ga ari masu ka.")
    sentenceE.toRomaji() should be("kiku")
    sentenceF.toRomaji() should be("ōkina")
    sentenceG.toRomaji() should be("tōkyō")
  }

  it should "correctly transliterate to hiragana" in {
    sentenceA.toHiragana() should be("お すし が たべ たい")
    //sentenceB.toHiragana() should be("さ")
    //sentenceC.toHiragana() should be("さ")
    //sentenceD.toHiragana() should be("さ")
    sentenceE.toHiragana() should be("きく")
    sentenceF.toHiragana() should be("おうきな")
    sentenceG.toHiragana() should be("とうきょう")
  }

  it should "correctly transliterate to katakana" in {
    sentenceA.toKatakana() should be("オ スシ ガ タベ タイ")
    //sentenceB.toKatakana() should be("さ")
    //sentenceC.toKatakana() should be("さ")
    //sentenceD.toKatakana() should be("さ")
    sentenceE.toKatakana() should be("キク")
    sentenceF.toKatakana() should be("オーキナ")
    sentenceG.toKatakana() should be("トーキョー")
  }
}
