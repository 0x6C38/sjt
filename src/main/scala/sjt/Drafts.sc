import sjt._

import sjt.JapaneseSyntax._
import sjt.JapaneseInstances._

val t = 'c'

t.isHiragana

import JapaneseInstances._
import JapaneseSyntax._
Japanese.isHiragana('c')

'j'.isHiragana
'j'.isKatakana
'カ'.isKatakana

//def isHiragana(value:Char):Boolean = '\u3041' <= value
//println(isHiragana('C'))
import sjt._

import sjt.JapaneseSyntax._
import sjt.JapaneseInstances._