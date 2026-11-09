-- | Turning metadata names into Haskell identifiers.
--
-- Haskell requires functions and record fields to start with a lowercase
-- letter, and types, constructors and pattern synonyms to start with an
-- uppercase one. Metadata names are kept as they are where Haskell allows
-- it; otherwise only the leading word changes case.
module Windows.Bindgen.Names
  ( valueName
  , conName
  , dedupe
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toUpper)
import Data.List (mapAccumL)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | A name for a function or record field. The leading run of capitals is
-- lowered, except a final capital that starts the next word:
--
-- > GetTickCount -> getTickCount
-- > WSAStartup   -> wsaStartup
-- > DPA_Create   -> dpa_Create
-- > hWnd         -> hWnd
valueName :: Text -> Text
valueName n = escapeKeyword (lowered <> rest)
  where
    (upper, rest0) = T.span isAsciiUpper n
    (lowered, rest)
      | T.length upper > 1 && startsLower rest0 =
          (T.toLower (T.init upper), T.singleton (T.last upper) <> rest0)
      | T.null upper = case T.uncons n of
          Just (c, cs) | not (isAsciiLower c || c == '_') -> ("v" <> T.singleton c, cs)
          _ -> ("", n)
      | otherwise = (T.toLower upper, rest0)
    startsLower t = maybe False (isAsciiLower . fst) (T.uncons t)

-- | A name for a type, constructor or pattern synonym.
conName :: Text -> Text
conName n = case T.uncons n of
  Just (c, cs)
    | isAsciiUpper c -> n
    | isAsciiLower c -> T.cons (toUpper c) cs
    | c == '_' || isDigit c -> "T" <> n
  _ -> "T" <> n

-- | Make names unique by appending underscores, keeping the first
-- occurrence of each name unchanged. Names in @taken@ are already in use.
dedupe :: Set.Set Text -> [Text] -> [Text]
dedupe taken = snd . mapAccumL step taken
  where
    step seen n =
      let free c
            | c `Set.member` seen = free (c <> "_")
            | otherwise = c
          n' = free n
       in (Set.insert n' seen, n')

escapeKeyword :: Text -> Text
escapeKeyword n
  | n `elem` keywords = n <> "_"
  | otherwise = n

keywords :: [Text]
keywords =
  [ "case"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "else"
  , "foreign"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "then"
  , "type"
  , "where"
  , "forall"
  , "pattern"
  , "proc"
  , "rec"
  , "mdo"
  ]
