import com.atilika.kuromoji.ipadic.Tokenizer
import org.scalatest._
import sjt.JapaneseInstances._
import sjt.JapaneseSyntax._

class KanjiTransliteration extends FlatSpec with Matchers{
  val cachedTokenizer = new Tokenizer()

  val sentenceA = "お寿司が食べたい"
  val sentenceB = "診察行ってきました。最終目標は心臓移植というのは変わらないそうで、自分の心臓じゃ生きられないみたいです。お金もかかるし、手術するのも入院するのももう嫌だな～…なんてことを考えながらvitaで朧村正をプレイしてました！　牛鬼と馬鬼ってボスより強くない！？　ぜんぜん勝てないんだけど…"
  val sentenceC = "「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる"
  val sentenceD = "皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。"
  val sentenceE = "聞く"
  val sentenceF = "大きな"
  val sentenceG = "東京"
  val sentenceH = "可愛い"
  val sentenceI = "牛乳"
  val sentenceJ = "喋る"
  //val sentenceZ = "見つけよう" //Bug: Kuromoji error

  "Kanji" should "correctly transliterate to romaji" in {
    sentenceA.toRomaji(cachedTokenizer) should be("o sushi ga tabe tai")
    sentenceB.toRomaji(cachedTokenizer) should be("shinsatsu itte ki mashi ta. saishū mokuhyō wa shinzō ishoku toyuu no wa kawara nai sō de, jibun no shinzō ja iki rare nai mitai desu. okane mo kakarushi, shujutsu suru no mo nyūin suru no mo mō iya da na * nante koto wo kangaenagara * de oboro mura sei wo * shite mashi ta! 　 ushi oni to uma oni tte bosu yori tsuyoku nai!? 　 zenzen kate nai n dakedo …")
    sentenceC.toRomaji(cachedTokenizer) should be("「 ima 」 okite iru koto wo mitsukeyo u. kokunai no nyūsu kara mijika na deki goto made, minna no wadai ga wakaru 「 ima 」 okite iru koto wo mitsukeyo u. kokunai no nyūsu kara mijika na deki goto made, minna no wadai ga wakaru")
    sentenceD.toRomaji(cachedTokenizer) should be("minasan wa nippon no yottsu no ōkina shima no namae wo shitte i masu ka. nippon ni wa tōkyō no yō na, sekai ni yoku shira rete iru toshi ga takusan ari masuga, minasan wa donna toshi namae wo kiki ta koto ga ari masu ka.")
    sentenceE.toRomaji(cachedTokenizer) should be("kiku")
    sentenceF.toRomaji(cachedTokenizer) should be("ōkina")
    sentenceG.toRomaji(cachedTokenizer) should be("tōkyō")
    sentenceH.toRomaji(cachedTokenizer) should be("kawaii")
    sentenceI.toRomaji(cachedTokenizer) should be("gyūnyū")
    sentenceJ.toRomaji(cachedTokenizer) should be("shaberu")
  }

  it should "correctly transliterate to hiragana" in {
    sentenceA.toHiragana(cachedTokenizer) should be("おすしがたべたい")
    sentenceB.toHiragana(cachedTokenizer) should be("しんさついってきました。さいしゅうもくひょうはしんぞういしょくというのはかわらないそうで、じぶんのしんぞうじゃいきられないみたいです。おかねもかかるし、しゅじゅつするのもにゅういんするのももういやだな*なんてことをかんがえながら*でおぼろむらせいを*してました！　うしおにとうまおにってぼすよりつよくない！？　ぜんぜんかてないんだけど…")
    sentenceC.toHiragana(cachedTokenizer) should be("「いま」おきていることをみつけよう。こくないのにゅうすからみぢかなできごとまで、みんなのわだいがわかる「いま」おきていることをみつけよう。こくないのにゅうすからみぢかなできごとまで、みんなのわだいがわかる")
    sentenceD.toHiragana(cachedTokenizer) should be("みなさんはにっぽんのよっつのおおきなしまのなまえをしっていますか。にっぽんにはとうきょうのような、せかいによくしられているとしがたくさんありますが、みなさんはどんなとしなまえをききたことがありますか。")
    sentenceE.toHiragana(cachedTokenizer) should be("きく")
    sentenceF.toHiragana(cachedTokenizer) should be("おおきな")
    sentenceG.toHiragana(cachedTokenizer) should be("とうきょう")
    sentenceH.toHiragana(cachedTokenizer) should be("かわいい")
    sentenceI.toHiragana(cachedTokenizer) should be("ぎゅうにゅう")
    sentenceJ.toHiragana(cachedTokenizer) should be("しゃべる")
  }

  it should "correctly transliterate to katakana" in {
    sentenceA.toKatakana(cachedTokenizer) should be("オ・スシ・ガ・タベ・タイ")
    sentenceB.toKatakana(cachedTokenizer) should be("シンサツ・イッ・テ・キ・マシ・タ。・サイシュー・モクヒョー・ワ・シンゾー・イショク・トユウ・ノ・ワ・カワラ・ナイ・ソー・デ、・ジブン・ノ・シンゾー・ジャ・イキ・ラレ・ナイ・ミタイ・デス。・オカネ・モ・カカル・シ、・シュジュツ・スル・ノ・モ・ニューイン・スル・ノ・モ・モー・イヤ・ダ・ナ・*・ナンテ・コト・ヲ・カンガエ・ナガラ・*・デ・オボロ・ムラ・セイ・ヲ・*・シ・テ・マシ・タ！・　・ウシ・オニ・ト・ウマ・オニ・ッテ・ボス・ヨリ・ツヨク・ナイ！？・　・ゼンゼン・カテ・ナイ・ン・ダ・ケド・…")
    sentenceC.toKatakana(cachedTokenizer) should be("「・イマ・」・オキ・テ・イル・コト・ヲ・ミツケヨ・ウ。・コクナイ・ノ・ニュース・カラ・ミジカ・ナ・デキ・ゴト・マデ、・ミンナ・ノ・ワダイ・ガ・ワカル・「・イマ・」・オキ・テ・イル・コト・ヲ・ミツケヨ・ウ。・コクナイ・ノ・ニュース・カラ・ミジカ・ナ・デキ・ゴト・マデ、・ミンナ・ノ・ワダイ・ガ・ワカル")
    sentenceD.toKatakana(cachedTokenizer) should be("ミナサン・ワ・ニッポン・ノ・ヨッツ・ノ・オーキナ・シマ・ノ・ナマエ・ヲ・シッ・テ・イ・マス・カ。・ニッポン・ニ・ワ・トーキョー・ノ・ヨー・ナ、・セカイ・ニ・ヨク・シラ・レ・テ・イル・トシ・ガ・タクサン・アリ・マス・ガ、・ミナサン・ワ・ドンナ・トシ・ナマエ・ヲ・キキ・タ・コト・ガ・アリ・マス・カ。")
    sentenceE.toKatakana(cachedTokenizer) should be("キク")
    sentenceF.toKatakana(cachedTokenizer) should be("オーキナ")
    sentenceG.toKatakana(cachedTokenizer) should be("トーキョー")
    sentenceH.toKatakana(cachedTokenizer) should be("カワイイ")
    sentenceI.toKatakana(cachedTokenizer) should be("ギューニュー")
    sentenceJ.toKatakana(cachedTokenizer) should be("シャベル")
  }
}
