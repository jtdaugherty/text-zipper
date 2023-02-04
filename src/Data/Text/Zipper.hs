-- | This module provides a two-dimensional text zipper data structure.
-- This structure represents a body of text and an editing cursor
-- which can be moved throughout the text, along with a set of editing
-- transformations.
--
-- Text zippers are generalized over the set of data types that might be
-- used to store lists of characters (e.g., 'String', 'T.Text', etc.).
-- As a result, the most general way to create a text zipper is to use
-- 'mkZipper' and provide all of the functions required to manipulate
-- the underlying text data.
--
-- Implementations using 'T.Text' and 'String' are provided.
module Data.Text.Zipper
    ( TextZipper

    -- * Construction and extraction
    , mkZipper
    , textZipper
    , stringZipper
    , clearZipper
    , vectorZipper
    , getText
    , currentLine
    , cursorPosition
    , lineLengths
    , getLineLimit

    -- * Navigation functions
    , moveCursor
    , moveCursorClosest
    , moveRight
    , moveLeft
    , moveUp
    , moveDown
    , gotoEOL
    , gotoBOL
    , gotoEOF
    , gotoBOF

    -- * Inspection functions
    , currentChar
    , nextChar
    , previousChar

    -- * Editing functions
    , insertChar
    , insertMany
    , deletePrevChar
    , deleteChar
    , breakLine
    , killToEOL
    , killToBOL
    , killToEOF
    , killToBOF
    , transposeChars
    )
where

import Control.Applicative ((<$>))
import Control.DeepSeq
import Data.Char (isPrint)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.Zipper.Vector as V

data TextZipper a =
    TZ { toLeft :: a
       , toRight :: a
       , above :: [a]
       , below :: [a]
       , fromChar :: Char -> a
       , drop_ :: Int -> a -> a
       , take_ :: Int -> a -> a
       , length_ :: a -> Int
       , last_ :: a -> Char
       , init_ :: a -> a
       , null_ :: a -> Bool
       , lines_ :: a -> [a]
       , toList_ :: a -> [Char]
       , lineLimit :: Maybe Int
       }

instance (NFData a) => NFData (TextZipper a) where
    rnf z = (toLeft z) `deepseq`
            (toRight z) `deepseq`
            (above z) `deepseq`
            (below z) `deepseq`
            ()

-- | Get the line limit, if any, for a zipper.
getLineLimit :: TextZipper a -> Maybe Int
getLineLimit = lineLimit

instance (Eq a) => Eq (TextZipper a) where
    a == b = and [ toLeft a == toLeft b
                 , toRight a == toRight b
                 , above a == above b
                 , below a == below b
                 ]

instance (Show a) => Show (TextZipper a) where
    show tz = concat [ "TextZipper { "
                     , "above = "
                     , show $ above tz
                     , ", below = "
                     , show $ below tz
                     , ", toLeft = "
                     , show $ toLeft tz
                     , ", toRight = "
                     , show $ toRight tz
                     , " }"
                     ]

-- | Create a zipper using a custom text storage type. Takes the initial
-- text as well as all of the functions necessary to manipulate the
-- underlying text values.
mkZipper :: (Monoid a) =>
            (Char -> a)
         -- ^A singleton constructor.
         -> (Int -> a -> a)
         -- ^'drop'.
         -> (Int -> a -> a)
         -- ^'take'.
         -> (a -> Int)
         -- ^'length'.
         -> (a -> Char)
         -- ^'last'.
         -> (a -> a)
         -- ^'init'.
         -> (a -> Bool)
         -- ^'null'.
         -> (a -> [a])
         -- ^'lines'.
         -> (a -> [Char])
         -- ^'toList'.
         -> [a]
         -- ^The initial lines of text.
         -> Maybe Int
         -- ^Limit to this many lines of text ('Nothing' means no limit).
         -> TextZipper a
mkZipper fromCh drp tk lngth lst int nl linesFunc toListF ls lmt =
    let limitedLs = case lmt of
          Nothing -> ls
          Just n -> take n ls
        (first, rest) = if null limitedLs
                        then (mempty, mempty)
                        else (head limitedLs, tail limitedLs)
        numLines = length ls
        insertLine z (i, l) = (if i < numLines - 1 then breakLine else id) $ insertMany l z
        loadInitial z = foldl insertLine z $ zip [0..] (first:rest)
    in loadInitial $ TZ mempty mempty mempty mempty fromCh drp tk lngth lst int nl linesFunc toListF lmt

-- | Get the text contents of the zipper.
getText :: (Monoid a) => TextZipper a -> [a]
getText tz = concat [ above tz
                    , [currentLine tz]
                    , below tz
                    ]

-- | Return the lengths of the lines in the zipper.
lineLengths :: (Monoid a) => TextZipper a -> [Int]
lineLengths tz = (length_ tz) <$> concat [ above tz
                                         , [currentLine tz]
                                         , below tz
                                         ]

-- | Get the cursor position of the zipper; returns @(row, col)@.
-- @row@ ranges from @[0..num_rows-1]@ inclusive; @col@ ranges from
-- @[0..length of current line]@ inclusive. Column values equal to line
-- width indicate a cursor that is just past the end of a line of text.
cursorPosition :: TextZipper a -> (Int, Int)
cursorPosition tz = (length $ above tz, length_ tz $ toLeft tz)

-- | Move the cursor to the specified row and column. Invalid cursor
-- positions will be ignored. Valid cursor positions range as described
-- for 'cursorPosition'.
moveCursor :: (Monoid a) => (Int, Int) -> TextZipper a -> TextZipper a
moveCursor (row, col) tz =
    let t = getText tz
    in if row < 0
           || row >= length t
           || col < 0
           || col > length_ tz (t !! row)
       then tz
       else tz { above = take row t
               , below = drop (row + 1) t
               , toLeft = take_ tz col (t !! row)
               , toRight = drop_ tz col (t !! row)
               }

-- | Move the cursor to the specified row and column. Invalid cursor
-- positions will be reinterpreted as the closest valid position. Valid
-- cursor positions range as described for 'cursorPosition'.
moveCursorClosest :: (Monoid a) => (Int, Int) -> TextZipper a -> TextZipper a
moveCursorClosest (row, col) tz =
    let t = getText tz
        bestRow = min (max 0 $ length t - 1) $ max 0 row
        bestCol = if bestRow < length t
                  then min (length_ tz (t !! bestRow)) $ max 0 col
                  else 0
    in tz { above = take bestRow t
          , below = drop (bestRow + 1) t
          , toLeft = take_ tz bestCol (t !! bestRow)
          , toRight = drop_ tz bestCol (t !! bestRow)
          }

isFirstLine :: TextZipper a -> Bool
isFirstLine = null . above

isLastLine :: TextZipper a -> Bool
isLastLine = (== 0) . length . below

nextLine :: TextZipper a -> a
nextLine = head . below

-- | The line of text on which the zipper's cursor currently resides.
currentLine :: (Monoid a) => TextZipper a -> a
currentLine tz = (toLeft tz) `mappend` (toRight tz)

-- | Insert a character at the current cursor position.
--
-- If the character is a newline, break the current line.
--
-- If the character is non-printable, ignore it.
--
-- Otherwise insert the character and move the cursor one position to
-- the right.
insertChar :: (Monoid a) => Char -> TextZipper a -> TextZipper a
insertChar ch tz
    | ch == '\n' = breakLine tz
    | isPrint ch = tz { toLeft = toLeft tz `mappend` (fromChar tz ch) }
    | otherwise  = tz

-- | Insert many characters at the current cursor position. Move the
-- cursor to the end of the inserted text.
insertMany :: (Monoid a) => a -> TextZipper a -> TextZipper a
insertMany str tz = foldl (\z c -> insertChar c z) tz $ toList_ tz str

-- | Insert a line break at the current cursor position.
breakLine :: (Monoid a) => TextZipper a -> TextZipper a
breakLine tz =
    -- Plus two because we count the current line and the line we are
    -- about to create; if that number of lines exceeds the limit,
    -- ignore this operation.
    let modified = tz { above = above tz ++ [toLeft tz]
                      , toLeft = mempty
                      }
    in case lineLimit tz of
          Just lim -> if length (above tz) + length (below tz) + 2 > lim
                      then tz
                      else modified
          Nothing -> modified

-- | Move the cursor to the end of the current line.
gotoEOL :: (Monoid a) => TextZipper a -> TextZipper a
gotoEOL tz = tz { toLeft = currentLine tz
                , toRight = mempty
                }

-- | Move the cursor to the end of a text zipper.
gotoEOF :: (Monoid a) => TextZipper a -> TextZipper a
gotoEOF tz =
    tz { toLeft = end
       , toRight = mempty
       , above = top
       , below = mempty
       }
   where
       tx = getText tz
       (top, end) = if null tx
                    then (mempty, mempty)
                    else (init tx, last tx)

-- | Remove all text from the cursor position to the end of the current
-- line. If the cursor is at the beginning of a line and the line is
-- empty, the entire line will be removed.
killToEOL :: (Monoid a) => TextZipper a -> TextZipper a
killToEOL tz
    | (null_ tz $ toLeft tz) && (null_ tz $ toRight tz) &&
      (not $ null $ below tz) =
          tz { toRight = head $ below tz
             , below = tail $ below tz
             }
    | otherwise = tz { toRight = mempty
                     }

-- | Remove all text from the cursor position to the beginning of the
-- current line.
killToBOL :: Monoid a => TextZipper a -> TextZipper a
killToBOL tz = tz { toLeft = mempty
                  }

-- | Remove all text from the cursor position to the end of the text
-- zipper. If the cursor is at the beginning of a line and the line is
-- empty, the entire line will be removed.
killToEOF :: (Monoid a) => TextZipper a -> TextZipper a
killToEOF tz =
    tz { toRight = mempty
       , below = mempty
       }

-- | Remove all text from the cursor position to the beginning of the
-- text zipper.
killToBOF :: Monoid a => TextZipper a -> TextZipper a
killToBOF tz =
    tz { toLeft = mempty
       , above = mempty
       }

-- | Delete the character preceding the cursor position, and move the
-- cursor backwards by one character.
deletePrevChar :: (Eq a, Monoid a) => TextZipper a -> TextZipper a
deletePrevChar tz
    | moveLeft tz == tz = tz
    | otherwise = deleteChar $ moveLeft tz

-- | Delete the character at the cursor position. Leaves the cursor
-- position unchanged. If the cursor is at the end of a line of text,
-- this combines the line with the line below.
deleteChar :: (Monoid a) => TextZipper a -> TextZipper a
deleteChar tz
    -- Can we just remove a char from the current line?
    | (not $ null_ tz (toRight tz)) =
        tz { toRight = drop_ tz 1 $ toRight tz
           }
    -- Do we need to collapse the previous line onto the current one?
    | null_ tz (toRight tz) && (not $ null $ below tz) =
        tz { toRight = head $ below tz
           , below = tail $ below tz
           }
    | otherwise = tz

-- | Get the Char on which the cursor currently resides. If the cursor
-- is at the end of the text or the text is empty return @Nothing@.
currentChar :: TextZipper a -> Maybe Char
currentChar tz
  | not (null_ tz (toRight tz)) =
    Just (last_ tz (take_ tz 1 (toRight tz)))
  | otherwise = Nothing

-- | Get the Char after the cursor position. If the cursor is at the end
-- of a line return the first character of the next line, or if that one
-- is empty as well, return @Nothing@.
nextChar :: (Monoid a) => TextZipper a -> Maybe Char
nextChar tz = currentChar (moveRight tz)

-- | Get the Char before the cursor position. If the cursor is at the
-- beginning of the text, return @Nothing@
previousChar :: (Monoid a) => TextZipper a -> Maybe Char
previousChar tz
  -- Only return Nothing if we are at the beginning of a line and only empty
  -- lines are above
  | snd (cursorPosition tz) == 0 && all (null_ tz) (above tz) =
    Nothing
  | otherwise =
    currentChar (moveLeft tz)

-- | Move the cursor to the beginning of the current line.
gotoBOL :: (Monoid a) => TextZipper a -> TextZipper a
gotoBOL tz = tz { toLeft = mempty
                , toRight = currentLine tz
                }

-- | Move the cursor to the beginning of a text zipper.
gotoBOF :: (Monoid a) => TextZipper a -> TextZipper a
gotoBOF tz =
    tz { toLeft = mempty
       , toRight = first
       , above = mempty
       , below = rest
       }
    where
        tx = getText tz
        (first, rest) = if null tx
                        then (mempty, mempty)
                        else (head tx, tail tx)

-- | Move the cursor right by one position. If the cursor is at the end
-- of a line, the cursor is moved to the first position of the following
-- line (if any).
moveRight :: (Monoid a) => TextZipper a -> TextZipper a
moveRight tz
    -- Are we able to keep moving right on the current line?
    | not (null_ tz (toRight tz)) =
        tz { toLeft = toLeft tz
                      `mappend` (take_ tz 1 $ toRight tz)
           , toRight = drop_ tz 1 (toRight tz)
           }
    -- If we are going to go beyond the end of the current line, can
    -- we move to the next one?
    | not $ null (below tz) =
        tz { above = above tz ++ [toLeft tz]
           , below = tail $ below tz
           , toLeft = mempty
           , toRight = nextLine tz
           }
    | otherwise = tz

-- | Move the cursor left by one position. If the cursor is at the
-- beginning of a line, the cursor is moved to the last position of the
-- preceding line (if any).
moveLeft :: (Monoid a) => TextZipper a -> TextZipper a
moveLeft tz
    -- Are we able to keep moving left on the current line?
    | not $ null_ tz (toLeft tz) =
        tz { toLeft = init_ tz $ toLeft tz
           , toRight = fromChar tz (last_ tz (toLeft tz))
                       `mappend` toRight tz
           }
    -- If we are going to go beyond the beginning of the current line,
    -- can we move to the end of the previous one?
    | not $ null (above tz) =
        tz { above = init $ above tz
           , below = currentLine tz : below tz
           , toLeft = last $ above tz
           , toRight = mempty
           }
    | otherwise = tz

-- | Move the cursor up by one row. If there no are rows above the
-- current one, move to the first position of the current row. If the
-- row above is shorter, move to the end of that row.
moveUp :: (Monoid a) => TextZipper a -> TextZipper a
moveUp tz
    -- Is there a line above at least as long as the current one?
    | (not $ isFirstLine tz) &&
      (length_ tz $ last $ above tz) >= length_ tz (toLeft tz) =
        tz { below = currentLine tz : below tz
           , above = init $ above tz
           , toLeft = take_ tz (length_ tz $ toLeft tz) (last $ above tz)
           , toRight = drop_ tz (length_ tz $ toLeft tz) (last $ above tz)
           }
    -- Or if there is a line above, just go to the end of it
    | (not $ isFirstLine tz) =
        tz { above = init $ above tz
           , below = currentLine tz : below tz
           , toLeft = last $ above tz
           , toRight = mempty
           }
    -- If nothing else, go to the beginning of the current line
    | otherwise = gotoBOL tz

-- | Move the cursor down by one row. If there are no rows below the
-- current one, move to the last position of the current row. If the row
-- below is shorter, move to the end of that row.
moveDown :: (Monoid a) => TextZipper a -> TextZipper a
moveDown tz
    -- Is there a line below at least as long as the current one?
    | (not $ isLastLine tz) &&
      (length_ tz $ nextLine tz) >= length_ tz (toLeft tz) =
        tz { below = tail $ below tz
           , above = above tz ++ [currentLine tz]
           , toLeft = take_ tz (length_ tz $ toLeft tz) (nextLine tz)
           , toRight = drop_ tz (length_ tz $ toLeft tz) (nextLine tz)
           }
    -- Or if there is a line below, just go to the end of it
    | (not $ isLastLine tz) =
        tz { above = above tz ++ [currentLine tz]
           , below = tail $ below tz
           , toLeft = nextLine tz
           , toRight = mempty
           }
    -- If nothing else, go to the end of the current line
    | otherwise = gotoEOL tz

-- | Transpose the character before the cursor with the one at the
-- cursor position and move the cursor one position to the right. If
-- the cursor is at the end of the current line, transpose the current
-- line's last two characters.
transposeChars :: (Monoid a) => TextZipper a -> TextZipper a
transposeChars tz
    | null_ tz (toLeft tz) = tz
    | null_ tz (toRight tz) =
        if length_ tz (toLeft tz) < 2
        then tz
        else let prefixLen = length_ tz (toLeft tz) - 2
                 prefix = take_ tz prefixLen (toLeft tz)
                 lastTwo = drop_ tz prefixLen (toLeft tz)
                 a = take_ tz 1 lastTwo
                 b = drop_ tz 1 lastTwo
             in tz { toLeft = prefix <> b <> a
                   }
    | otherwise = tz { toLeft = (init_ tz $ toLeft tz) <>
                                (take_ tz 1 $ toRight tz) <>
                                (fromChar tz $ last_ tz $ toLeft tz)
                     , toRight = (drop_ tz 1 $ toRight tz)
                     }

-- | Construct a zipper from list values.
stringZipper :: [String] -> Maybe Int -> TextZipper String
stringZipper =
    mkZipper (:[]) drop take length last init null lines id

-- | Construct a zipper from vectors of characters.
vectorZipper :: [V.Vector Char] -> Maybe Int -> TextZipper (V.Vector Char)
vectorZipper =
    mkZipper V.singleton V.drop V.take V.length V.last V.init V.null V.vecLines V.toList

-- | Empty a zipper.
clearZipper :: (Monoid a) => TextZipper a -> TextZipper a
clearZipper tz =
    tz { toLeft = mempty
       , toRight = mempty
       , above = []
       , below = []
       }

-- | Construct a zipper from 'T.Text' values.
textZipper :: [T.Text] -> Maybe Int -> TextZipper T.Text
textZipper =
    mkZipper T.singleton T.drop T.take T.length T.last T.init T.null T.lines T.unpack
