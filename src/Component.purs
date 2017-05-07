module Component where

import Prelude
import Data.Array as Array
import Data.Array as Array
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Lens as HL
import Halogen.HTML.Lens.Checkbox as HL.Checkbox
import Halogen.HTML.Lens.Input as HL.Input
import Halogen.HTML.Lens.TextArea as HL.TextArea
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import DOM (DOM)
import Data.Array (filter, foldr, intercalate)
import Data.Char (toUpper)
import Data.Lens.Suggestion (Lens', lens, suggest)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, singleton, split, uncons)
import Data.String.Regex (split, match) as Re
import Data.String.Regex.Flags (noFlags, global) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import Data.String.Utils (words)

type Query = HL.Query State

type Element p = H.HTML p Query

type State =
  { description :: String
  , name :: String
  , typeAnnot :: String
  , executeBody :: String
  , isInstant :: Boolean
  }

descriptionL :: Lens' State String
descriptionL = lens (_.description) (\s d -> s { description = d })

nameL :: Lens' State String
nameL = lens (_.name) (\s n -> s { name = n })

typeAnnotL :: Lens' State String
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

typeComponent :: forall p. State -> Element p
typeComponent = HL.Input.render' typeAnnotL $ [class_ "type"] <> nospellcheck

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
    , typeAnnot: "forall f m. Foldable f => Monoid m => f m -> m"
    , executeBody: "foldl (<>) mempty"
    , isInstant: false
    }

  render :: State -> H.ComponentHTML Query
  render state@{ description, name } =
    HH.div_
      [ HH.h1_
          [ HH.text "Create FRC Command" ]
      , HH.text "-- | ", descriptionComponent state
      , HH.br_
      , nameComponent state, HH.text " :: ", typeComponent state
      , HH.br_
      , nameComponent state, HH.text " = "
      , HH.br_, HH.text "\xa0\xa0\xa0\xa0"
      , executeComponent state
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
