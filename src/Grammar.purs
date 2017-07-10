module Main.Grammar where

import Prelude

import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)

newtype Lit = Lit String
derive instance newtypeLit :: Newtype Lit _
instance showLit :: Show Lit where show (Lit s) = s

space :: forall a b. Show a => Show b => a -> b -> Lit
space a b = Lit (show a <> " " <> show b)
infixl 5 space as ◫

spaceL :: forall b. Show b => String -> b -> Lit
spaceL s b = Lit s ◫ b
infixr 6 spaceL as ◧

spaceR :: forall a. Show a => a -> String -> Lit
spaceR a s = a ◫ Lit s
infixl 5 spaceR as ◨

spaceB :: String -> String -> Lit
spaceB l r = Lit l ◫ Lit r
infix 5 spaceB as ■

parens :: forall a. Show a => a -> Lit
parens s = Lit ("(" <> show s <> ")")

varR :: Regex
varR = unsafeRegex "^\\w+$" (unicode)

opR :: Regex
opR = unsafeRegex "^$" unicode

isvar :: String -> Boolean
isvar = test varR

data TypeAny
  = Qualified (Array String) (Array Constraint) TypeSimple
  | Other TypeSimple

data TypeSimple
  = TypeVar String
  | TypeOp String
  | TypeApp TypeSimple TypeSimple
  | TypeParen TypeSimple

data Constraint = Constraint String (Array String)

instance showTypeAny :: Show TypeAny where
  show (Other t) = show t
  show (Qualified vars constraints t) =
    "forall "
      <> joinWith " " vars <> ". "
      <> joinWith " => " cons <> " => "
      <> show t
    where cons = constraints # map \(Constraint name vars) ->
      name <> " " <> joinWith " " vars

instance showTypeSimple :: Show TypeSimple where
  show (TypeParen t) = "(" <> show t <> ")"
  show (TypeVar v)
    | isvar v = v
    | otherwise = show (parens (Lit v))
  show (TypeOp v)
    | isvar v = "`" <> v <> "`"
    | otherwise = v
  show (TypeApp (TypeApp (TypeOp o) l) r) =
    show (l ◨ o ◫ r)
  show (TypeApp f a) =
    show (f ◫ a)

parseType :: String -> TypeAny
parseType s = Qualified
  ["a","b"]
  [Constraint "Show" ["a"], Constraint "Show" ["b"]]
  (TypeVar "a" `fn` (TypeVar "b" `fn` TypeVar "String"))
  where
    opp op l r = TypeApp (TypeOp op) l `TypeApp` r
    fn = opp "->"
