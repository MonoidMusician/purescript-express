module Halogen.HTML.Lens.Autocomplete where

import Prelude

import Control.Monad.Except (runExcept)
import DOM.Event.Event (preventDefault)
import DOM.Event.KeyboardEvent (key)
import DOM.Event.Types (focusEventToEvent, keyboardEventToEvent, mouseEventToEvent)
import DOM.Util.TextCursor (TextCursor(..))
import DOM.Util.TextCursor as TC
import DOM.Util.TextCursor.Element as TC.El
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens (Lens', non, re, view, (.~), (^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (wrap)
import Data.String as Str
import Data.String.Regex (Regex)
import Data.String.Regex (match, test) as Re
import Data.String.Regex.Flags (noFlags) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import Data.String.Utils (startsWith, endsWith)
import Data.String.VerEx (anyOf, anythingBut, capture, endOfLine, find, match, some, startOfLine, test, toRegex, upper, whitespace, word)
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

onKey :: Array String -> String -> TextCursor -> Maybe TextCursor
onKey completions k tc@(TextCursor { before, selected, after, direction }) = case k of
  -- add a period after forall
  " " | "forall " == before || "∀ " == before
      , "" <- selected
      , not (Re.test (Re.unsafeRegex "^[\\w\\s]*\\." Re.noFlags) after)
          -> Just $ TextCursor { before, selected, after: "." <> after, direction }
  -- expand "f." into a forall followed by a period after the cursor
  "." | "f." <- before
      , "" <- selected
          -> Just $ TextCursor { before: "forall", selected, after: ". " <> after, direction }
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
              in Just $ TextCursor { before: b <> v, selected, after: after'', direction }
  -- deduplicate periods, passing them over
  "." | endsWith "." before
      , "" <- selected
      , startsWith "." after
          -> Just $ TextCursor { before, selected, after: Str.drop 1 after, direction }
  -- autocomplete a selected autocompletion
  "Enter"
      | Just w <- lastword before
        -- check that selection is a completion
      , selected `Array.elem` (getrest w completions)
          -> Just $ TextCursor { before: before <> selected, selected: "", after, direction }
  -- generate autocompletion when a regular character is typed
  _   | Str.length k == 1 && k /= " " -- ordinary characters
      , "" <- selected -- no selection
      , noword after -- not right before a word
      , Just w <- lastword before -- but right after a word which
      , Just r <- Array.head (getrest w completions) -- starts completion
          -> Just $ TextCursor { before, selected: r, after, direction }
  _ -> Nothing

beforeKey :: Array String -> String -> TextCursor -> Maybe TextCursor
beforeKey completions k tc@(TextCursor { before, selected, after, direction }) = case k of
  "ArrowUp"
    | noword after
    , Just w <- lastword before
    , Just c <- nextWrapped selected $ getrest w completions
        -> Just $ TextCursor { before, selected: c, after: after, direction }
  "ArrowDown"
    | noword after
    , Just w <- lastword before
    , Just c <- nextWrapped selected $ Array.reverse $ getrest w completions
        -> Just $ TextCursor { before, selected: c, after: after, direction }
  _ -> Nothing

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

nextWrapped :: String -> Array String -> Maybe String
nextWrapped w completions = do
  i <- completions # Array.elemIndex w
  let
    j =
      if i > 0
        then i-1
        else Array.length completions - 1
  completions Array.!! j

forallregex :: Regex
forallregex = toRegex do
  -- "^(forall.+)([A-Z]\\w* )([\\w\\s]+)\\.$"
  let letters = "abcdefghijklmnopqrstuvwxyz"
  startOfLine
  b <- capture do
    find "forall" -- VerEx.alt (find "∀") (find "forall")
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
    [ HE.onInput (HE.input tcQuery)
    , HE.onKeyUp (HE.input doKey)
    , HE.onBlur (HE.input $ focusEventToEvent >>> tcQuery)
    , HE.onClick (HE.input $ mouseEventToEvent >>> tcQuery)
    , HE.onKeyDown (HE.input doKeyDown)
    , HP.value (TC.content (state ^. lens))
    , HP.class_ (wrap "type")
    --, HP.attr (HH.AttrName "data") (Re.source forallregex)
    ] <> nospellcheck
    where
      queryF prevent e f = HL.UpdateState
        case runExcept $ TC.El.readEventTarget e of
          Left _ -> pure id
          Right node -> do
            value <- TC.El.textCursor node
            let
              updated = f value
              updated' = fromMaybe value updated
            when (prevent && isJust updated) $ preventDefault e
            when (updated' /= value) do
              TC.El.setTextCursor updated' node
            pure (lens .~ updated')
      tcQuery e = queryF false e (const Nothing)
      doKey e =
        queryF false (keyboardEventToEvent e) $ onKey completions (key e)
      doKeyDown e =
        queryF true (keyboardEventToEvent e) $ beforeKey completions (key e)
