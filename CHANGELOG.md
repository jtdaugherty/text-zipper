
0.11
----

API changes:
 * Added `gotoBOF`, `gotoEOF`, `killToBOF`, and `killToEOF` functions
   (thanks Itai Y. Efrat)

0.10.1
------

 * WordSpec: fix a test verification bug (fixed #11)
 * WordSpec: generation of random text should never include newlines

0.10
----

- Integrated word editing and navigation functions
  courtesy of Hans-Peter Deifel's hledger-iadd project (see
  Data.Text.Zipper.Generic.Words)
- Added currentChar, nextChar, and previousChar (thanks @kRITZCREEK)

0.9
---

- insertChar and insertMany now only insert printable characters and
  newlines (subject to text zipper line limits)
- The GenericTextZipper class now requires a new method,
  toList :: a -> [Char]

0.8.3
-----

- Fixed insertMany accidental addition of trailing newline

0.8.2
-----

- Fixed insertMany for zippers with no line limit

0.8.1
-----

- Added Github links and CHANGELOG to package

0.8
---

- Added 'transposeChars' function

0.7.1
-----

- Generic: import everything from Monoid for older GHCs

0.7
---

- API changes: Add Generic module to abstract over text container types

0.6.1
-----

- Make insertMany respect the zipper's line limit

0.6
---

- Add insertMany for faster bulk insertion

0.5
---

- Added killToBOL function (thanks Hans-Peter Deifel)
- Enabled -Wall
- Added dependency on deepseq
- Added NFData instance for TextZipper

0.4
---

- Added clearZipper
- Added isFirstLine (thanks Kwang Yul Seo)
- Renamed lastLine to isLastLine (thanks Kwang Yul Seo)

0.3.1
-----

- Fixed export of vectorZipper

0.3
---

- Added vectorZipper for zipping over vectors of characters

0.2.1
-----

- Exported getLineLimit to permit obtaining a zipper's line limit

0.2
---

- Added support for limiting the number of lines in the zipper
- insertChar "\n" is now equivalent to breakLine
- Improved Show instance for TextZipper

0.1.1
-----

- Updated package metadata

0.1
---

Initial release (originally split off from vty-ui)
