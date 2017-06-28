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
val token:Token = tokens.head
val romanizedTokens = tokens.map(_.toRomaji)
//println("し".toRomaji)
println(token.toRomaji)
println(romanizedTokens)
//val ts3:String = tokens.flatMap(_.getPronunciation.map(_.toRomaji)).mkString
val ts3:String = tokens.map(_.getPronunciation).mkString
tokens.head.getPronunciation
//def isHiragana(value:Char):Boolean = '\u3041' <= value
//println(isHiragana('C'))
import sjt._
import sjt.JapaneseSyntax._
import sjt.JapaneseInstances._

"shiっte".indexOf("っ")
'n'.isFullWidthKatakana