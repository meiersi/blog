The `blaze-builder` library: faster construction of bytestrings
===============================================================

NOTE to build in:
  - Builder is the basis for a serialization library.

Hello dear reader, I am [Simon Meier](http://people.inf.ethz.ch/meiersi), a
swiss Haskell enthusiast currently pursuing his PhD in computer science at ETH
Zurich. In this blog post, I'll introduce you to the
[`blaze-builder`](http://hackage.haskell.org/package/blaze-builder) library.

The `blaze-builder` library provides you with a `Builder` type that you can use
to *efficiently* construct sequences of bytes represented in a packed form as a
strict or lazy bytestring. Hence, typical use cases for a `Builder` are saving
your application data in a space efficient binary form to a file or sending a
response to some request over the network. 

Probably, you know about the
[`binary`](http://hackage.haskell.org/package/binary-0.5.0.2) package, which
also provides a `Builder` type in the
[`Data.Binary.Builder`](http://hackage.haskell.org/packages/archive/binary/0.5.0.2/doc/html/Data-Binary-Builder.html)
module targeting exactly the same use-case as our `Builder`. This is no coincidence.  During
this year's Google Summer of Code, [Jasper Van der
Jeugt](http://jaspervdj.be) and I developed the `blaze-buider` library to
overcome performance shortcomings of `Data.Binary.Builder` with respect to the
specific needs of the [`blaze-html`](http://jaspervdj.be/blaze) HTML templating
library. Since then, I have restructured the `blaze-builder` library to serve
as a drop-in replacement for `Data.Binary.Builder`, *which it improves upon
with respect to both speed as well as expressivity*.


Usage example
-------------

> import qualified Data.ByteString      as S
> import qualified Data.ByteString.Lazy as L

> import Text.Blaze.Builder
> import Text.Blaze.Builder.Char.Utf8 (fromString)

> import Data.Monoid
> import Criterion.Main

> infixl 4 <>

> (<>) :: Monoid a => a -> a -> a
> (<>) = mappend

> data Person = Person { name :: String, age :: Int }

> people = zipWith Person ["Haskell 98", "Switzerland", "λ-bot"] [12, 719, 7]

> fromStringLength32le :: String -> Builder
> fromStringLength32le cs = 
>   fromInt32le (fromIntegral $ length cs) <> fromString cs

> fromPerson :: Person -> Builder
> fromPerson p = 
>   fromStringLength32le (name p) <> fromInt32le (fromIntegral $ age p)

> fromPeople :: [Person] -> Builder
> fromPeople = mconcat . map fromPerson

> cloneVillage :: [Person]
> cloneVillage = take 10000 $ cycle $ people

> lazyBinaryPeople :: [Person] -> L.ByteString
> lazyBinaryPeople = toLazyByteString . fromPeople

> strictBinaryPeople :: [Person] -> S.ByteString
> strictBinaryPeople = toByteString . fromPeople

Well now we would like to benchmark these two functions. That's no problem. But what do we compare these numbers to?
There's no library available that allows a *direct* implementation of the same funtionality. 
'binary' has no support for Unicode character encoding. `utf8-string` and `text` only produce bytestrings. Obviously,
we could combine these libraries with 'binary' by copying the produced bytestrings. However, chances are that's not
going to be super fast. Actually, it really isn't fast. We tried this route in various forms during the development 
of `blaze-html` and in the end using `blaze-builder` turned out to be the fastest route.

However, before  ... let us focus on simpler tasks where we have good competitors. 

map S.length $ L.toChunks $ lazyBinaryPeople cloneVillage 
[4088,32760,32760,32760,32760,32760,2113]

L.length $ lazyBinaryPeople cloneVillage
170001

> main :: IO ()
> main = defaultMain 
>     [ bench "lazyBinaryPeople cloneVillage" $ 
>         whnf (L.length . lazyBinaryPeople) cloneVillage
>
>     , bench "strictBinaryPeople cloneVillage" $ 
>         whnf strictBinaryPeople cloneVillage
>     ]

Packing `[Word8]`
-----------------





, whose
concatenation is not exactly

Obviously, we could
use 


On my Intel Core 2 .... blah computing `lazyBinaryPeople cloneVillage` takes .. and `strictBinaryPeople cloneVillage` take ... 

Well that's a number it could be good, but what do we compare it to? We cannot really compare it to `Data.Binary.Builder`, as
it does not support the encoding of 'String's directly. Obviously, there are ways to sidestep this issue. Using for 
example `Data.Text`.

 import Data.Binary.Builder as 

 fromStringLength32le' :: String -> Binary.Builder
 fromStringLength32le' s = 
    Binary.putInt32le (length s) <> fromByteString (T.encodeUtf8 (T.pack s))

 fromPerson' :: Person -> Binary.Builder
 fromPerson' p = fromStringLength32le' (name p) <> Binary.putInt32le (age p)


Now the corresponding benchmarks `lazyBinaryPeople' cloneVillage` take .. and  ...

This, is more than ... slower!

The point is that `Data.Binary.Builder`'s interface is too abstract to build
efficient serializers. Moreover, `Data.Binary.Builder`s `mconcat` is too
expensive.

But that's not the point. Let's separate the issue expressivity from
benchmarking as good as possible. Let's focus on very simple benchmarks.

Comparison to 


%%%%%%%%%%%%% OLD STUFFFFFFF %%%%%%%%%%%%%%%%%%%%%%


This first blog post about the `blaze-builder` library features a simple usage
example to get you acquinted with the library as well as a comparison of the
performance between `blaze-builder` and `binary`.

In the next blog post, I will then explain how the `blaze-builder` library is
implemented.  Intuitively, its implementation can be seen as a neat abstraction of
buffered output. However, more about that in the next post.


Usage example
-------------

Let us first install the newest version (0.2.0.0) of the
[`blaze-builder`](http://hackage.haskell.org/package/blaze-builder) library

~~~
  cabal install blaze-builder
~~~

and import its main module as well as the UTF-8 encoding module.

~~~ { .haskell }
  import Text.Blaze.Builder
  import Text.Blaze.Builder.Char.Utf8
~~~

The `Text.Blaze.Builder` modules provides you with a type `Builder` that allows
to efficiently construct lazy bytestrings with a large average chunk size.

Intuitively, a `Builder` denotes the construction of a part of a lazy
bytestring. Builders can either be created using one of the primitive
combinators in `Text.Blaze.Builder.Write` or by using one of the predefined
combinators for standard Haskell values (see the exposed modules of this
package).  Concatenation of builders is done using `mappend` from the
`Monoid` typeclass.

Here is a small example that serializes a list of strings using the UTF-8
encoding.

~~~ { .haskell }
  serializeString :: String -> Builder
  serializeString ss = fromInt (length ss) `mappend` fromString ss
~~~

Let's assume, we are given a large list of rather short strings and we would
like to concatenate all of them and UTF-8 encode the resulting string. Tasks
like this occur for example when rendering a HTML template. For the sake of
simplicity let us assume that this list of `strings` is defined as follows.

~~~ { .haskell }
  strings :: [String]
  strings = replicate 10000 "Hello there!"
~~~

The function `fromString :: String -> Builder` creates a `Builder` denoting the
UTF-8 encoded argument. Hence, UTF-8 encoding and concatenating all @strings@
can be done follows.

~~~ { .haskell }
  concatenation :: Builder
  concatenation = mconcat $ map fromString strings
~~~

The function 'toLazyByteString'  can be used to execute a `Builder` and
obtain the resulting lazy bytestring.

~~~ { .haskell }
  result :: L.ByteString
  result = toLazyByteString concatenation
~~~

The @result@ is a lazy bytestring containing 10000 repetitions of the string
@\"Hello there!\"@ encoded using UTF-8. The corresponding 120000 bytes are
distributed among three chunks of 32kb and a last chunk of 6kb.


Comparison to the `binary` library
----------------------------------


