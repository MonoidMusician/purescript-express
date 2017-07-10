module Halogen.HTML.Lens.Autocomplete where

import Prelude

import Control.Monad.Except (runExcept)
import DOM.Event.KeyboardEvent (key)
import DOM.Event.Types (focusEventToEvent, keyboardEventToEvent, mouseEventToEvent)
import DOM.Util.TextCursor (TextCursor(..))
import DOM.Util.TextCursor as TC
import DOM.Util.TextCursor.Element as TC.El
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens (Lens', non, re, view, (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String as Str
import Data.String.Regex (Regex)
import Data.String.Regex (match, test, source) as Re
import Data.String.Regex.Flags (noFlags) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import Data.String.Utils (startsWith, endsWith)
import Data.String.VerEx
  ( anyOf, anythingBut, capture, endOfLine, find, match, some
  , startOfLine, test, toRegex, upper, whitespace, word
  )
import Data.String.VerEx as VerEx
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Lens as HL
import Halogen.HTML.Properties as HP

nospellcheck :: forall a r. Array
  (HH.IProp
    ( autocomplete :: HP.OnOff
    , spellcheck :: Boolean
    | r
    ) a)
nospellcheck = [HP.autocomplete false, HP.spellcheck false]

onKey :: Array String -> String -> TextCursor -> TextCursor
onKey completions k tc@(TextCursor { before, selected, after }) = case k of
  -- add a period after forall
  " " | "forall " == before || "∀ " == before
      , "" <- selected
      , not (Re.test (Re.unsafeRegex "^[\\w\\s]*\\." Re.noFlags) after)
          -> TextCursor { before, selected, after: "." <> after }
  -- expand "f." into a forall symbol followed by a period after the cursor
  "." | "f." <- before
      , "" <- selected
          -> TextCursor { before: "∀", selected, after: ". " <> after }
  -- place constraints written in a forall after the quantifier
  "." | "" <- selected
      , Just [_, Just b, Just c, Just v] <-
          before # Re.match forallregex
            ->
              let
                after' = Str.dropWhile (eq '.' || eq ' ') after
                m = case Re.match constraintsregex after' of
                  Just [Just m'] -> m'
                  _ -> ""
                after'' =
                  ". " <> m <> c <> v <> " => " <>
                  Str.drop (Str.length m) after'
              in TextCursor { before: b <> v, selected, after: after'' }
  -- deduplicate periods, passing them over
  "." | endsWith "." before
      , "" <- selected
      , startsWith "." after
          -> TextCursor { before, selected, after: Str.drop 1 after }
  -- autocomplete a selected autocompletion
  "Enter"
      | Just w <- lastword before
      , Just r <- Array.head (getrest w completions)
      , selected == r -- check that selection is a completion
          -> TextCursor { before: before <> r, selected: "", after }
  -- generate autocompletion when a regular character is typed
  _   | Str.length k == 1 && k /= " " -- ordinary characters
      , "" <- selected -- no selection
      , noword after -- not right before a word
      , Just w <- lastword before -- but right after a word which
      , Just r <- Array.head (getrest w completions) -- starts completion
          -> TextCursor { before, selected: r, after }
  _ -> tc

noword :: String -> Boolean
noword = not <<< test do
  startOfLine
  word

lastword :: String -> Maybe String
lastword = id <=< Array.head <=< match do
  w <- capture word
  endOfLine
  pure ([w])

-- | Return possible non-empty completions for a prefix.
getrest :: String -> Array String -> Array String
getrest w = Array.mapMaybe (Str.stripPrefix (Str.Pattern w) >=> wo)
  where wo = view (re (non "")) -- Just "" -> Nothing

forallregex :: Regex
forallregex = toRegex do
  -- "^(forall.+)([A-Z]\\w* )([\\w\\s]+)\\.$"
  let letters = "abcdefghijklmnopqrstuvwxyz"
  startOfLine
  b <- capture do
    VerEx.alt (find "∀") (find "forall")
    anythingBut (Str.toUpper letters)
  c <- capture do
    upper
    word
    whitespace
  v <- capture do
    some do
      anyOf (letters <> Str.toUpper letters <> "1234567890 ")
  find "."
  endOfLine

ops :: String
ops = ":!#$%&*+./<=>?@\\\\^|-~"

constraintsregex :: Regex
constraintsregex =
  Re.unsafeRegex
    ("(?:[\\w\\s]+\\s*=>(?=[^" <> ops <> "]|$)\\s*)*")
    Re.noFlags

render ::
  forall s r.
  Lens' s TextCursor ->
  Array String -> s ->
  HH.HTML r (HL.Query s Unit)
render lens completions state = HH.input $
    [ HE.onInput (HE.input $ tcQuery)
    , HE.onKeyUp (HE.input $ doKey)
    , HE.onBlur (HE.input $ focusEventToEvent >>> tcQuery)
    , HE.onClick (HE.input $ mouseEventToEvent >>> tcQuery)
    , HP.value (TC.content (state ^. lens))
    , HP.class_ (wrap "type")
    --, HP.attr (HH.AttrName "data") (Re.source forallregex)
    ] <> nospellcheck
    where
      queryF e f = HL.UpdateState
        case runExcept $ TC.El.readEventTarget e of
          Left _ -> pure id
          Right node -> do
            value <- TC.El.textCursor node
            let updated = f value
            when (updated /= value)
              (TC.El.setTextCursor updated node)
            pure (lens .~ updated)
      tcQuery e = queryF e id
      doKey e =
        queryF (keyboardEventToEvent e) $ onKey completions (key e)
