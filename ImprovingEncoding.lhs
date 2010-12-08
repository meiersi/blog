Dear Haskellers,

this is an RFC on an idea of to improve the support for encoding in Haskell. It
is written in literate Haskell and uses the following extensions and imports.

> {-# LANGUAGE OverloadedString #-}

> import Data.ByteString
> import Data.Text
> import Data.Text.Encoding


The Efficiency Problem 
----------------------

In the following, encoding denotes the process of mapping Haskell values to
their corresponding sequence of bytes that is then sent over the network,
written to a file, etc. Obviously, we want encoding to be as fast as possible.
"as fast as possible" is difficult to define, but I think the following two
design criteria should at least be met by every code that implements encoding.

  1. It should use as few system calls as possible.
  2. It should copy data as few times as possible.

The following example illustrates why our current library infrastructure leads
to unnecessary system calls and/or copies of data. Assume that we implement a
webserver. Now let us determine the cost of sending a HTTP response containing
a short (e.g., < 2kb) little-endian UTF-16 encoded website. For the sake of
argument, assume we have a representation of this website as a text value t.

> t :: Text
> t = "<html> ...HTML of our example website.... </html>"

Hence, encoding it to little-endian UTF-16 is as simple as calling

> tEncoded :: ByteString
> tEncoded = encodeUtf16le t

But now how do we prepend the necessary HTTP headers? These headers are encoded
using the US-ASCII charset. Hence, we cannot prepend them directly as Text and
encode only afterwards. We can represent the headers as a bytestring. Then we
can either (1) send the headers and the encoded website using two system calls
or we can (2) first concatenate the two bytestrings and use a single system
call. Both solutions are suboptimal with respect to encoding code that
allocates a single large enough (e.g., 32kb) buffer, then first writes the
headers into it, afterwards encodes the text directly into this buffer, and
finally sends this buffer using a single system call. Solution (1) uses a
system call too much and solution (2) copies the the data twice.

It is easy to see that this problem of doing too many copies or too many system
calls gets more severe the smaller the pieces of data are that need to be
concatenated.

It is common knowledge that one can use a builder monoid as it is provided by
the 'binary' package or my 'blaze-builder' package to get cheap concatenation
for the generation of lazy bytestrings. However, note that using a builder is
not sufficient to solve the above problem, as encodeUtf16le returns a
bytestring, which a builder still has to copy to its output buffer to provide a
large average chunk size (which is required to avoid unnecessary system calls).
This design choice for encoding functions to return bytestrings is actually
almost pervasive in the libraries provided on Hackage. Hence, currently, we are
forced to make too many copies to ensure few system calls. [1]


A Solution to the Efficiency Problem
------------------------------------

I cannot see another solution to the above problem, but to change our encoding 
functions such that they have type 

  encodeX :: a -> Builder

With this change, the caller of an encoding function gets control over the
buffer allocation strategy, which allows best use of the available ressources.
For example, a builder can be instructed to write directly to the free space of the
buffer associated to a filehandle and, after flushing, this buffer can be
reused. Another example is that, when implementing a low-latency network
protocol, you can produce data in small chunks rather than the big default
chunks normally produced by encoding functions.

The builder design [1] I've implement in the 'blaze-builder' package allows this
separatation of the allocation strategy from the implementation of the encoding
function. It provides both an implementation of a Builder monoid as well as a
Put monad similar to the ones from the 'binary' package. The Put monad is
useful for implementing encoding functions that may fail (e.g., decompress from
the 'zlib' package). They can use the embedded value to return their failure
status and later encoding functions can react accordingly. In most cases, I
imagine this to be that no encoding is done and the failure is returned to the
driver that wants the builders/puts to fill the buffer.

I would suggest that such a builder monoid and such a put monad are provided by
the 'bytestring' package. This allows authors of encoding functions to base on
a standard package and provide implementations with the above mentioned good
compositionality properties. Moreover, we could then also implement
chunk-generating-functions functions like filter or map in Data.ByteString.Lazy
such that they automatically defragment [3, 4] the input lazy bytestring.

Apart from 


Sharing Encoding Implementations
--------------------------------


Conclusion
----------


[1] There would be the option of using vectored IO as it is provided by the
network-2.3 package. However, it is easy to see that its performance also
detoriates when faced with very small bytestrings, which are actually the
rule rather than the exception when serializing non-homogenous data (e.g., a
dynamically generated website).

[2] https://github.com/meiersi/blaze-builder/blob/master/Blaze/ByteString/Builder/Internal/Types.hs

[3] http://lambda-view.blogspot.com/2010/11/defragmenting-lazy-bytestrings.html



On the
other hand the encoding 


in the
encoding process to write at the right position into the 

The solution to the above problem is simple but for encoding functions to result in
builders and not in (lazy) bytestrings.



this does not solve the above
problem! 




the solution to this problem 


this website is
represented in memory as a text value t


For encoding homogenuous data like a sequence of unicode characters represented
as a Text value t the situation is not too bad. 


  
  the cost of system calls
  
  guarantee a large average chunk size (i.e., avoid unnecessary
     buffer flushes) to amortize the cost of system calls.


However, our current library infrastructure 


In the following, encoding denotes the process of mapping Haskell values to
their corresponding sequence of bytes that is then sent over the network,
written to a file, etc. Obviously, we want
encoding to be as fast as possible. "as fast as possible" is difficult to
define, but the following two design criteria should at least be met by every
code that implements encoding.
