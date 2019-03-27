import com.atilika.kuromoji.ipadic.{Token, Tokenizer}
import sjt._
import sjt.JapaneseSyntax._
import sjt.JapaneseInstances._
import collection.JavaConverters._
import scala.annotation.tailrec

sjt.Kana.allKana
sjt.Kana.everyKanaSyllable
val cachedTokenizer = new Tokenizer()

import JapaneseInstances._
import JapaneseSyntax._
"tōkyō".toHiragana()
"ぎゅうにゅう".toKatakana()
"shasshin"
"猫はすごいです".transliterate(cachedTokenizer)
"図書館".transliterate(cachedTokenizer)
"としょかん".transliterate()
"目標".transliterate()

Kana.isTranslateableSymbol("。")
Kana.translateableSymbolsStr
def printDebug(s: String): Unit = {
  //println("---------------------------")
  println(s"$s Original: $s, romaji: " + s.toRomaji() + " hiragana: " + s.toHiragana() + ", katakana: " +s.toKatakana()+ ", syllables: " + s.syllabify)
  println(s"$s to romaji[New]: " + s.toRomaji())
  println(s"$s to Hiragana: " + s.toHiragana())
  println(s"$s syllables: " + s.syllabify)
  println(s"$s syllables k: " + s.toKatakana().syllabify)
  println

}
def transliterateAll(s: String, t: Tokenizer): Unit = {
  println("------------------------")
  println(s)
  println(s.toRomaji(t))
  println(s.toHiragana(t))
  println(s.toKatakana(t))
}

def printAllFeatures(s:String):Unit = new Tokenizer().tokenize(s).asScala.toArray.foreach(t => println(t.getSurface() + ": " + t.getAllFeatures + "| " + t.getAllFeaturesArray()(1) + " | Pronunciation: " + t.getPronunciation))

def strToRomaji(s: String):Unit = {
  new Tokenizer().tokenize(s).asScala.toArray.foreach(_.getAllFeatures)
}
printDebug("じょじょ")
printDebug("目標")
printDebug("ヒョー")

val t1 = System.currentTimeMillis()
println("毎日私は午前十時に起きます。十時から十二まで勉強します。午後一時に昼ごはんを食べます。あとでまた勉強をします。六時に地下鉄で大学へ行きます。十一時に私の家へ帰ります。そして晩ご飯を食べます。次に少し仕事をします。プログラミンをします。難しいです。それでも、私は大好きです。なぜならとても楽しいですから。朝の五時にねます。".toRomaji())
println("診察行ってきました。最終目標は心臓移植というのは変わらないそうで、自分の心臓じゃ生きられないみたいです。お金もかかるし、手術するのも入院するのももう嫌だな～…なんてことを考えながらvitaで朧村正をプレイしてました！　牛鬼と馬鬼ってボスより強くない！？　ぜんぜん勝てないんだけど…".toRomaji())
println("「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる".toRomaji())
println("皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。".toRomaji())
val t2 = System.currentTimeMillis()
val deltaT = t2-t1
println(s"Δt = $deltaT")

val t9 = System.currentTimeMillis()
println("毎日私は午前十時に起きます。十時から十二まで勉強します。午後一時に昼ごはんを食べます。あとでまた勉強をします。六時に地下鉄で大学へ行きます。十一時に私の家へ帰ります。そして晩ご飯を食べます。次に少し仕事をします。プログラミンをします。難しいです。それでも、私は大好きです。なぜならとても楽しいですから。朝の五時にねます。".toRomaji())
println("診察行ってきました。最終目標は心臓移植というのは変わらないそうで、自分の心臓じゃ生きられないみたいです。お金もかかるし、手術するのも入院するのももう嫌だな～…なんてことを考えながらvitaで朧村正をプレイしてました！　牛鬼と馬鬼ってボスより強くない！？　ぜんぜん勝てないんだけど…".toRomaji())
println("「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる".toRomaji())
println("皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。".toRomaji())
val t10 = System.currentTimeMillis()
val deltaT4 = t10-t9
println(s"Δt = $deltaT4")

val t3 = System.currentTimeMillis()
println("毎日私は午前十時に起きます。十時から十二まで勉強します。午後一時に昼ごはんを食べます。あとでまた勉強をします。六時に地下鉄で大学へ行きます。十一時に私の家へ帰ります。そして晩ご飯を食べます。次に少し仕事をします。プログラミンをします。難しいです。それでも、私は大好きです。なぜならとても楽しいですから。朝の五時にねます。".toRomaji(cachedTokenizer))
println("診察行ってきました。最終目標は心臓移植というのは変わらないそうで、自分の心臓じゃ生きられないみたいです。お金もかかるし、手術するのも入院するのももう嫌だな～…なんてことを考えながらvitaで朧村正をプレイしてました！　牛鬼と馬鬼ってボスより強くない！？　ぜんぜん勝てないんだけど…".toRomaji(cachedTokenizer))
println("「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる".toRomaji(cachedTokenizer))
println("皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。".toRomaji(cachedTokenizer))

val t4 = System.currentTimeMillis()
val deltaT2 = t4-t3
println(s"Δt = $deltaT2 ~ 479 chars. 100k chars ~ 3.5 seconds")

val t5 = System.currentTimeMillis()
transliterateAll("毎日私は午前十時に起きます。十時から十二まで勉強します。午後一時に昼ごはんを食べます。あとでまた勉強をします。六時に地下鉄で大学へ行きます。十一時に私の家へ帰ります。そして晩ご飯を食べます。次に少し仕事をします。プログラミンをします。難しいです。それでも、私は大好きです。なぜならとても楽しいですから。朝の五時にねます。", cachedTokenizer)
transliterateAll("診察行ってきました。最終目標は心臓移植というのは変わらないそうで、自分の心臓じゃ生きられないみたいです。お金もかかるし、手術するのも入院するのももう嫌だな～…なんてことを考えながらvitaで朧村正をプレイしてました！　牛鬼と馬鬼ってボスより強くない！？　ぜんぜん勝てないんだけど…", cachedTokenizer)
transliterateAll("「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる「いま」起きていることを見つけよう。国内のニュースから身近なできごとまで、みんなの話題がわかる", cachedTokenizer)
transliterateAll("皆さんは日本の四つの大きな島の名前を知っていますか。日本には東京のような、世界によく知られている都市がたくさんありますが、皆さんはどんな都市名前を聞きたことがありますか。", cachedTokenizer)
transliterateAll("お寿司が食べたい", cachedTokenizer)
val t6 = System.currentTimeMillis()
val deltaT3 = t6-t5
println(s"Δt = $deltaT3 ~ 479 chars. 100k chars ~ 3.5 seconds")


val sample = "日本はチョョすごいい本当"
sample.extractHiragana
sample.extractKatakana
sample.extractKana
sample.extractKanji

sample.extractUniqueHiragana
sample.extractUniqueKatakana
sample.extractUniqueKana
sample.extractUniqueKanji
