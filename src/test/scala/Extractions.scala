import org.scalatest._
import sjt.JapaneseInstances._
import sjt.JapaneseSyntax._

class Extractions extends FlatSpec with Matchers {
  val sentenceA = "お寿司が食べたい"
  val sentenceB = "日本語はクルだ日本語が学びたい"
  val sentenceC = "聞く"
  val sentenceD = '聞'
  val sentenceE = "かわいい"
  val sentenceF = "日本はチョョすごいい本当に"
  val sentenceG = ""
  /*
  val sentenceH = ""
  val sentenceI = ""
  val sentenceJ = ""
  */
  "Kanjis" should "correctly be extracted from instances" in {
    sentenceA.extractKanji should be("寿司食")
    sentenceB.extractKanji should be("日本語日本語学")
    sentenceC.extractKanji should be("聞")
    sentenceD.extractKanji should be("聞")
    sentenceE.extractKanji should be ("")
    sentenceF.extractKanji should be ("日本本当")
  }

  "Unique Kanji" should "correctly be extracted from instances" in {
    sentenceA.extractUniqueKanji should be(Set('寿', '司', '食'))
    sentenceB.extractUniqueKanji should be(Set('日', '本', '語', '学'))
    sentenceC.extractUniqueKanji should be(Set('聞'))
    sentenceD.extractUniqueKanji should be(Set('聞'))
    sentenceE.extractUniqueKanji should be (Set.empty)
    sentenceF.extractUniqueKanji should be(Set('日', '本', '当'))
  }

  "Hiragana" should "correctly be extracted from instances" in {
    sentenceA.extractHiragana should be("おがべたい")
    sentenceB.extractHiragana should be("はだがびたい")
    sentenceC.extractHiragana should be("く")
    sentenceD.extractHiragana should be("")
    sentenceE.extractHiragana should be ("かわいい")
    sentenceF.extractHiragana should be ("はすごいいに")
  }

  "Unique Hiragana" should "correctly be extracted from instances" in {
    sentenceA.extractUniqueHiragana should be(Set('お', 'が', 'べ', 'た','い'))
    sentenceB.extractUniqueHiragana should be(Set('は', 'だ', 'が', 'び', 'た', 'い'))
    sentenceC.extractUniqueHiragana should be(Set('く'))
    sentenceD.extractUniqueHiragana should be(Set.empty)
    sentenceE.extractUniqueHiragana should be (Set('か', 'わ','い'))
    sentenceF.extractUniqueHiragana should be (Set('は', 'す','ご', 'い', 'に'))
  }

  "Katakana" should "correctly be extracted from instances" in {
    sentenceA.extractKatakana should be("")
    sentenceB.extractKatakana should be("クル")
    sentenceC.extractKatakana should be("")
    sentenceD.extractKatakana should be("")
    sentenceE.extractKatakana should be ("")
    sentenceF.extractKatakana should be ("チョョ")
  }

  "Unique Katakana" should "correctly be extracted from instances" in {
    sentenceA.extractUniqueKatakana should be(Set.empty)
    sentenceB.extractUniqueKatakana should be(Set('ク', 'ル'))
    sentenceC.extractUniqueKatakana should be(Set.empty)
    sentenceD.extractUniqueKatakana should be(Set.empty)
    sentenceE.extractUniqueKatakana should be(Set.empty)
    sentenceF.extractUniqueKatakana should be(Set('チ', 'ョ'))
  }

}