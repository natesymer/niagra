{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.Selector.Combinators
(
  not,
  focus,
  hover,
  active
)
where
  
import Data.Niagra.Selector
import Prelude hiding (not)

not :: Selector -> Selector
not = pseudoClass "not" . Just

focus :: Selector
focus = pseudoClass' "focus"

hover :: Selector
hover = pseudoClass' "hover"

active :: Selector
active = pseudoClass' "active"

