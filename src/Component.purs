module Component where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.KeyboardEvent (key)
import DOM.Event.Types (focusEventToEvent, keyboardEventToEvent)
import DOM.Util.TextCursor (TextCursor(..), appendr)
import DOM.Util.TextCursor as TC
import DOM.Util.TextCursor.Element as TC.El
import Data.Array (filter, foldr, intercalate)
import Data.Array as Array
import Data.Char (toUpper)
import Data.Either (Either(..))
import Data.Lens (over, (.~), (^.))
import Data.Lens.Suggestion (Lens', lens, suggest)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
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

descriptionL :: Lens' State String
descriptionL = lens (_.description) (\s d -> s { description = d })

nameL :: Lens' State String
nameL = lens (_.name) (\s n -> s { name = n })

typeAnnotL :: Lens' State TextCursor
typeAnnotL = lens (_.typeAnnot) (\s t -> s { typeAnnot = t })

executeL :: Lens' State String
executeL = lens (_.executeBody) (\s e -> s { executeBody = e })

isInstantL :: Lens' State Boolean
isInstantL = lens (_.isInstant) (\s i -> s { isInstant = i })

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

suggestDescriptionL :: Lens' State String
suggestDescriptionL = suggest descriptionL toName nameL

descriptionComponent :: forall p. State -> Element p
descriptionComponent = HL.Input.render suggestDescriptionL

class_ = HP.class_ <<< HH.ClassName
nospellcheck = [HP.autocomplete false, HP.spellcheck false]

nameComponent :: forall p. State -> Element p
nameComponent = HL.Input.render' nameL $ [class_ "name"] <> nospellcheck

--typeComponent :: forall p. State -> Element p
--typeComponent = HL.Input.render' typeAnnotL $ [class_ "type"] <> nospellcheck

executeComponent :: forall p. State -> Element p
executeComponent = HL.TextArea.render executeL

instantComponent :: forall p. State -> Element p
instantComponent = HL.Checkbox.renderAsField "Instant command" isInstantL

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
    { description: "Fold"
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
      ]
    where
      doKey e =
        queryF (keyboardEventToEvent e)
          (onKey (key e))
      onKey :: String -> TextCursor -> TextCursor
      onKey = case _ of
        " " -> case _ of
          tc@(TextCursor { before, selected, after })
            | "forall " <- before
            , "" <- selected
            , not (test (Re.unsafeRegex "^[\\w\\s]+\\." Re.noFlags) after)
                -> TextCursor { before, selected, after: "." <> after }
          tc -> tc
        "." -> case _ of
          tc@(TextCursor { before, selected, after })
            | "" <- selected
            , Just [_, Just b, Just c, Just v] <-
                before # Re.match forallregex
                  ->
                    let
                      after' = Str.dropWhile (eq '.' || eq ' ') after
                      m = case Re.match contraintsregex after' of
                        Just [Just m] -> m
                        _ -> ""
                      after'' = ". " <> m <> c <> v <> " => " <> Str.drop (Str.length m) after'
                    in TextCursor { before: b <> v, selected, after: after'' }
          tc@(TextCursor { before, selected, after })
            | endsWith "." before
            , "" <- selected
            , startsWith "." after
                -> TextCursor { before, selected, after: Str.drop 1 after }
          tc -> tc
        _ -> id
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
          Vex.many Vex.word
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
            pure (typeAnnotL .~ updated)
      tcQuery e = queryF e id
      tcElement = HH.input
          [ HE.onInput (HE.input tcQuery)
          , HE.onKeyUp (HE.input doKey)
          , HE.onBlur (HE.input (focusEventToEvent >>> tcQuery))
          , HP.value (TC.content (state ^. typeAnnotL))
          , HP.classes [wrap "type"]
          ]

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
