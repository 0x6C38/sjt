import com.atilika.kuromoji.ipadic.{Token, Tokenizer}
import sjt._
import sjt.JapaneseSyntax._
import sjt.JapaneseInstances._
import collection.JavaConverters._
import scala.annotation.tailrec
import JapaneseInstances._
import JapaneseSyntax._

val cachedTokenizer = new Tokenizer()
val sample1 = "猫が好きです"
val sample2 = "図書館"
val sample3 = "最近"
val sample4 = "行きます"
val sample5 = "行"
val sample6 = "四月は四つの嘘です四人"
val sample7 = "月曜日" //最近
val sample8 = "時々" //最近
val sample9 = "皆さんは日本の四つの大きな島の名前を知っていますか" //最近

val samples = Array(sample1, sample2, sample3, sample4, sample5, sample6, sample7, sample8, sample9)

val sampleReadings = Map[Char, Array[String]]('月' -> Array("げつ", "がつ", "つき"), '曜' -> Array("よう"), '日' -> Array("び", "ひ", "にち", "よう"), '四' -> Array("し", "よん")) //removed , "び"from hi よう from you

samples.zipWithIndex.foreach(is => println("n:" + is._2 + ":" + is._1 + ": furigana: " + is._1.furigana(sampleReadings).mkString(",")))
samples.zipWithIndex.foreach(is => println("n:" + is._2 + ":" + is._1 + ": furigana: " + is._1.furigana().mkString(",")))
