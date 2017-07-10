module Component where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.KeyboardEvent (key)
import DOM.Event.Types (focusEventToEvent, keyboardEventToEvent)
import DOM.Util.TextCursor (TextCursor(..))
import DOM.Util.TextCursor as TC
import DOM.Util.TextCursor.Element as TC.El
import Data.Array (filter, foldr, intercalate)
import Data.Array as Array
import Data.Char (toUpper)
import Data.Either (Either(..))
import Data.Lens ((.~), (^.), view, re, non)
import Data.Lens.Suggestion (Lens', lens, suggest)
import Data.Maybe (Maybe(..))
import Data.String (joinWith, singleton, uncons)
import Data.String as Str
import Data.String.Regex (match) as Re
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import Data.String.Utils (endsWith, startsWith, words)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Lens as HL
import Halogen.HTML.Lens.Checkbox as HL.Checkbox
import Halogen.HTML.Lens.Input as HL.Input
import Halogen.HTML.Lens.TextArea as HL.TextArea
import Halogen.HTML.Properties as HP
import Main.Grammar (parseType)
import Data.String.VerEx as Vex

type Query = HL.Query State

type Element p = H.HTML p Query

type State =
  { description :: String
  , name :: String
  , typeAnnot :: TextCursor
  , executeBody :: String
  , isInstant :: Boolean
  }

_description :: Lens' State String
_description = lens (_.description) (\s d -> s { description = d })

_name :: Lens' State String
_name = lens (_.name) (\s n -> s { name = n })

_typeAnnot :: Lens' State TextCursor
_typeAnnot = lens (_.typeAnnot) (\s t -> s { typeAnnot = t })

_execute :: Lens' State String
_execute = lens (_.executeBody) (\s e -> s { executeBody = e })

_isInstant :: Lens' State Boolean
_isInstant = lens (_.isInstant) (\s i -> s { isInstant = i })

toName :: String -> String
toName = select >>> words >>> exclude >>> mapHeadTail Str.toLower camel >>> joinWith ""
    where
    select s = case Re.match (Re.unsafeRegex "^[\\w\\s]+" Re.noFlags) s of
        Just [Just r] -> r
        _ -> ""
    blacklist = ["this", "the","a","an","it"]
    exclude ws =
        foldr (\w -> filter (w /= _)) ws blacklist
    mapHeadTail f g a = case Array.uncons a of
        Nothing -> a
        Just { head, tail } ->
            Array.cons (f head) (map g tail)
    camel s =
        case uncons s of
            Nothing -> s
            Just { head, tail } ->
                (head # toUpper # singleton) <> tail

_suggestDescription :: Lens' State String
_suggestDescription = suggest _description toName _name

descriptionComponent :: forall p. State -> Element p
descriptionComponent =
  HL.Input.render'
    _suggestDescription
    [class_ "documentation"]

class_ :: forall a r. String -> HH.IProp ( "class" :: String | r ) a
class_ = HP.class_ <<< HH.ClassName

nospellcheck :: forall a r. Array
  (HH.IProp
    ( autocomplete :: HP.OnOff
    , spellcheck :: Boolean
    | r
    ) a)
nospellcheck = [HP.autocomplete false, HP.spellcheck false]

nameComponent :: forall p. State -> Element p
nameComponent = HL.Input.render' _name $ [class_ "name"] <> nospellcheck

executeComponent :: forall p. State -> Element p
executeComponent = HL.TextArea.render _execute

instantComponent :: forall p. State -> Element p
instantComponent = HL.Checkbox.renderAsField "Instant command" _isInstant

autocomplete :: Array String
autocomplete = ["Foldable", "Monoid"]

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { description: "Fold a data structure, accumulating values in some `Monoid`."
    , name: "fold"
    , typeAnnot: TC.single TC._before "forall f m. Foldable f => Monoid m => f m -> m"
    , executeBody: "foldMap id"
    , isInstant: false
    }

  render :: State -> H.ComponentHTML Query
  render state@{ description, name, typeAnnot } =
    HH.div_
      [ HH.h1_
          [ HH.text "Create PureScript Program" ]
      , HH.text "-- | ", descriptionComponent state
      , HH.br_
      , nameComponent state, HH.text " :: ", tcElement
      , HH.br_
      , nameComponent state, HH.text " = "
      , HH.br_, HH.text "\xa0\xa0\xa0\xa0"
      , executeComponent state
      , HH.br_, HH.text $ show (parseType (TC.content typeAnnot))
      , HH.br_, HH.text $ show (typeAnnot)
      , HH.br_, HH.text $ show (lastword $ typeAnnot ^. TC._before)
      ]
    where
      doKey e =
        queryF (keyboardEventToEvent e)
          (onKey (key e))
      onKey :: String -> TextCursor -> TextCursor
      onKey k tc@(TextCursor { before, selected, after }) = case k of
        -- add a period after forall
        " " | "forall " <- before
            , "" <- selected
            , not (test (Re.unsafeRegex "^[\\w\\s]+\\." Re.noFlags) after)
                -> TextCursor { before, selected, after: "." <> after }
        -- place constraints written in a forall after the quantifier
        "." | "" <- selected
            , Just [_, Just b, Just c, Just v] <-
                before # Re.match forallregex
                  ->
                    let
                      after' = Str.dropWhile (eq '.' || eq ' ') after
                      m = case Re.match contraintsregex after' of
                        Just [Just m'] -> m'
                        _ -> ""
                      after'' = ". " <> m <> c <> v <> " => " <> Str.drop (Str.length m) after'
                    in TextCursor { before: b <> v, selected, after: after'' }
        -- deduplicate periods, passing them over
        "." | endsWith "." before
            , "" <- selected
            , startsWith "." after
                -> TextCursor { before, selected, after: Str.drop 1 after }
        -- autocomplete a selected autocompletion
        "Enter"
            | Just w <- lastword before
            , Just r <- Array.head (getrest w autocomplete)
            , selected == r -- check that selection is a completion
                -> TextCursor { before: before <> r, selected: "", after }
        -- generate autocompletion when a regular character is typed
        _   | Str.length k == 1 && k /= " " -- ordinary characters
            , "" <- selected -- no selection
            , noword after -- not right before a word
            , Just w <- lastword before -- but right after a word which
            , Just r <- Array.head (getrest w autocomplete) -- starts completion
                -> TextCursor { before, selected: r, after }
        _ -> tc
      noword = not <<< Vex.test do
        Vex.startOfLine
        Vex.word
      lastword = id <=< Array.head <=< Vex.match do
        w <- Vex.capture Vex.word
        Vex.endOfLine
        pure ([w])
      getrest w = Array.mapMaybe (Str.stripPrefix (Str.Pattern w) >=> (view $ re $ non ""))
      forallregex = Vex.toRegex do
        -- "^(.*forall.+)([A-Z]\\w* )([\\w\\s]+)\\.$"
        let letters = "abcdefghijklmnopqrstuvwxyz"
        Vex.startOfLine
        b <- Vex.capture do
          Vex.anything
          Vex.find "forall"
          Vex.anythingBut (Str.toUpper letters)
        c <- Vex.capture do
          Vex.upper
          Vex.word
          Vex.whitespace
        v <- Vex.capture do
          Vex.some do
            Vex.anyOf (letters <> Str.toUpper letters <> "1234567890 ")
        Vex.find "."
        Vex.endOfLine
      ops = ":!#$%&*+./<=>?@\\\\^|-~"
      contraintsregex =
        Re.unsafeRegex
          ("(?:[\\w\\s]+\\s*=>(?=[^" <> ops <> "]|$)\\s*)*")
          Re.noFlags
      queryF e f = HL.UpdateState
        case runExcept $ TC.El.readEventTarget e of
          Left _ -> pure id
          Right node -> do
            value <- TC.El.textCursor node
            let updated = f value
            when (updated /= value)
              (TC.El.setTextCursor updated node)
            pure (_typeAnnot .~ updated)
      tcQuery e = queryF e id
      tcElement = HH.input $
          [ HE.onInput (HE.input tcQuery)
          , HE.onKeyUp (HE.input doKey)
          , HE.onBlur (HE.input (focusEventToEvent >>> tcQuery))
          , HP.value (TC.content (state ^. _typeAnnot))
          , class_ "type"
          ] <> nospellcheck

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM | eff))
  eval = HL.eval

texts :: forall p. Array String -> Array (Element p)
texts = map HH.text

from :: forall p. Array String -> Element p
from = HH.span_ <<< texts

commentline :: forall p. String -> Element p
commentline text = from [ "// ", text, "\n" ]

separate :: forall p i. HH.HTML p i -> Array (HH.HTML p i) -> HH.HTML p i
separate sep = HH.span_ <<< intercalate [sep] <<< map Array.singleton
