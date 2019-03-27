# Simple Japanese Transliterator
A transliteration library that uses kuromoji to convert text between the different japanese alphabets.

## Features
- Transliterate hiragana / katakana / romaji / kanji
- Split into syllables
- Tokenization Wrapper around kuromoji
- Extract furigana on a per-kanji level (as opposed to a token level offered by kuromoji)
- Utility methods

## Architecture
The main abstraction in this library is the `Japanese[A]` typeclass which defines a number of functions to work with japanese text. There are typeclass instances defined for `String`, `Char` and (kuromoji) `Token`, available out of the box.

 Aside from that, the `Kana` class defines a number of methods specific to kana and has defined several sets of characters such as diacritics, etc., that should come in handy.

## Usage

First, import a few things:

``` scala
import sjt.JapaneseInstances._
import sjt.JapaneseSyntax._
```

With the String instance in scope, we convert transliterate string literals like this:

``` scala
"tōkyō".toHiragana()
// String = とうきょう
"しゃっしん".toRomaji()
// String = shasshin
"ぎゅうにゅう".toKatakana()
// String = ギューニュー
```

Alternatively you can transliterate to hiragana, romaji and katakana at the same:

``` scala
"としょかん".transliterate()
// Transliteration(としょかん,Kana(としょかん,トショカン,toshokan))
```

Kanji can also be transliterated but it needs to be tokenized so it's a more expensive operation:

``` scala
"目標".transliterate()
// Transliteration(目標,Kana(もくひょう,モクヒョー,mokuhyō))
```

If you plan on transliterating things in bulk you should create a tokenizer instance before hand for better performance:

``` scala
val cachedTokenizer = new Tokenizer()
...
"図書館".transliterate(cachedTokenizer)
// Transliteration(図書館,Kana(としょかん,トショカン,toshokan))
"猫はすごいです".transliterate(cachedTokenizer)
// Transliteration(猫はすごいです,Kana(ねこはすごいです,ネコ・ワ・スゴイ・デス,neko wa sugoi desu))
```

If you don't pass a tokenizer instance explicitly one will be created for you under the covers. Instanciating a tokenizer can take almost 1000ms cold and that is a huge amount of time so always be sure to pass one.

### Split into syllables
The `Japanese` typeclass also defines `syllabify` which returns a `List[(Kana, String)]` with the syllables reversed:

``` scala
"ゆうびんきょく".syllabify
// List[(sjt.Kana, String)] = List((Kana(く,ク,ku),く), (Kana(きょ,キョ,kyo),きょ), (Kana(ん,ン,n),ん), (Kana(び,ビ,bi),び), (Kana(ゆ,ユ,yu),ゆう))
```

### Furigana
You can extract furigana for each kanji by using the `furigana` function:

``` scala
"郵便局".furigana()
// Array[sjt.Transliteration] = Array(Transliteration(郵,Kana(ゆう,ユー,yū)), Transliteration(便,Kana(びん,ビン,bin)), Transliteration(局,Kana(きょく,キョク,kyoku)))
```

### Utility functions
There are a number of functions defined by the `Japanese` typeclass, such as:

```scala
"言葉".containsKanji
// true
```

The `Kana` class also defines some utility methods as well as a number of sets of different kana:

```scala
import sjt.Kana._
val yoon = Kana.yoon
// Set[sjt.Kana] = Set(Kana(ちょ,チョ,cho), Kana(じゃ,ジャ,ja), Kana(きゃ,キャ,kya), Kana(ぢゅ,ヂュ,ju), Kana(きゅ,キュ,kyu), Kana(にょ,ニョ,nyo), Kana(しゃ,シャ,sha), Kana(じぇ,ジェ,je), Kana(ぎゅ,ギュ,gyu), Kana(りゃ,リャ,rya), Kana(ひゃ,ヒャ,hya), Kana(びゅ,ビュ,byu), Kana(びょ,ビョ,byo), Kana(ぢょ,ヂョ,jo), Kana(ちゅ,チュ,chu), Kana(ひょ,ヒョ,hyo), Kana(ぢゃ,ヂャ,ja), Kana(りゅ,リュ,ryu), Kana(しょ,ショ,sho), Kana(ひゅ,ヒュ,hyu), Kana(にゅ,ニュ,nyu), Kana(りょ,リョ,ryo), Kana(ちゃ,チャ,cha), Kana(ぎゃ,ギャ,gya), Kana(きょ,キョ,kyo), Kana(みゃ,ミャ,mya), Kana(にゃ,ニャ,nya), Kana(じょ,ジョ,jo), Kana(びゃ,ビャ,bya), Kana(じゅ,ジュ,ju), Kana(みょ,ミョ,myo), Kana(ぎょ,ギョ,gyo), Kana(しゅ,シュ,shu), Kana(みゅ,ミュ,myu))

ys.head.extendVowel()
// Kana(ちょう,チョー,chō)
```
