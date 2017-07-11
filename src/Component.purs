module Component where

import Prelude

import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.Util.TextCursor as TC
import Data.Array (filter, foldr, intercalate)
import Data.Array as Array
import Data.Char (toUpper)
import Data.Lens ((^.))
import Data.Lens.Suggestion (Lens', lens, suggest)
import Data.Maybe (Maybe(..))
import Data.String (joinWith, singleton, uncons)
import Data.String as Str
import Data.String.Regex (match) as Re
import Data.String.Regex.Flags (noFlags) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import Data.String.Utils (words)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Lens as HL
import Halogen.HTML.Lens.Checkbox as HL.Checkbox
import Halogen.HTML.Lens.Input as HL.Input
import Halogen.HTML.Lens.TextArea as HL.TextArea
import Halogen.HTML.Lens.Autocomplete as HL.Autocomplete
import Halogen.HTML.Properties as HP
import Main.Grammar (parseType)
import DOM.Util.TextCursor (TextCursor)

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

completions :: Array String
completions = ["Foldable", "Functor", "Monad", "Monoid"]

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
      , HH.br_, HH.text $ show (HL.Autocomplete.lastword $ typeAnnot ^. TC._before)
      ] where tcElement = HL.Autocomplete.render _typeAnnot completions state

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
