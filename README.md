This is a port of all the Miranda code from the 1992 publication of
‘Implementing Functional Languages’ by Simon Peyton Jones and David
Lester. The code is ported to Haskell.

While Simon Peyton Jones very kindly provided me the book sources so
that I could include the Haskell right inside the book, the age of the
sources has defeated modern tools and I was unable to do so. For this
reason, this repository is simply a supplement to the original book
rather than a stand-alone package that you can compile into the full
book. There does seem to be some newer sources even with some Haskell
in them
[here](http://code.haskell.org/SLPJ-collaborative-papers/pj-lester-book/)
and if you can get it all to compile properly, I'd love to hear about
it.

The code is tagged based on each section of the book: if you have
finished reading the section 1.5.4 of the book and would like to see
what the code looks like and to play around with it, you would check
out the ‘1.5.4’ tag. I will try my best to solve all the exercises
that result in code needed to progress in the book. Please note that I
can not assure that the exercises or the implementation itself are
correct and that they replicate the implementation in the book
exactly. It is still very much work in progress, implemented as I go
through the book: bug reports and pull requests are encouraged.

Please note that only sections with code changes will be tagged: if
section 1.1.4 didn't have any changes in it, it will not be tagged.
Lastly, there may be some commits between tags that don't necessarily
reflect the progress through the book such as README changes and
module clean-up which means that if you're interested in code from
section 3.1, you should check out the tag for 3.1 and only pay
attention to that: what happened in commit history between 3.0 and 3.1
is not important and what actual changes were made should be clear
from reading the book.

All code in this repository is under BSD 3-Clause License. See LICENSE
file for details.
