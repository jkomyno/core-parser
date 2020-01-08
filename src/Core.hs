{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Core where

-- Type that represents a variable name
type Name = String

-- A Core-lang program is just a list of supercombinator
-- definitions.
type Program a = [ScDef a]
type CoreProgram = Program Name

-- A supercombinator definition contains the name of the
-- supercombinator, its arguments and its body.
type ScDef a = (Name, [a], Expr a)
type CoreScDefn = ScDef Name

-- A variable definition contains the name of the
-- variable and its body.
type Def a = (a, Expr a)
type CoreDef = Def Name

data IsRec
  = NonRecursive
  | Recursive
  deriving (Show, Eq)

data Expr a
  = EVar Name                   -- Variables
  | ENum Int                    -- Numbers
  | EConstr Int Int             -- Constructor tag arity
  | EAp (Expr a) (Expr a)       -- Applications
  | ELet                        -- Let(rec) expressions
      IsRec                       -- boolean with True = recursive
      [Def a]                     -- Definitions
      (Expr a)                    -- Body of let(rec)
  | ECase                       -- Case expression
      (Expr a)                    -- Expression to scrutinise
      [Alter a]                   -- Alternatives
  | ELam [a] (Expr a)           -- Lambda abstractions
  deriving (Show, Eq)

-- Type tha parameterise the Expr data type wrt. its binders.
-- A binder is on the left-hand side (lhs) of a let(rec) definition,
-- or in a lambda abstraction.
type CoreExpr = Expr Name

-- type that models case of alternative

-- case expressions have an expression to analyse, and a list
-- of alternatives. Each alternative contains a tag,
-- a list of the bound variables and the expression to the right
-- of the arrow.
type Alter a = (Int, [a], Expr a)
type CoreAlter = Alter Name

{- RELOP lexems specification -}
-- http://www.personal.kent.edu/~rmuhamma/Compilers/MyCompiler/chapter3.htm

-- list of comparison operators
relopList :: [String]
relopList = [
  "<",
  "<=",
  "==",
  "˜=",
  ">=",
  ">"]
