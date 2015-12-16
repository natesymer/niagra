{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.Selector.Combinators
(
  -- * Pseudoclass Combinators
  active,
  checked,
  disabled,
  empty,
  enabled,
  firstChild,
  firstOfType,
  focus,
  hover,
  inRange,
  invalid,
  lang,
  lastChild,
  lastOfType,
  link,
  not,
  nthChild,
  nthLastOfType,
  nthOfType,
  onlyOfType,
  onlyChild,
  optional,
  outOfRange,
  readOnly,
  readWrite,
  required,
  root,
  target,
  valid,
  visited,
  
  -- * Pseudotype combinators
  before,
  after,
  firstLetter,
  firstLine,
  selection
)
where
  
import Data.Niagra.Selector
import Data.String
import Data.Text.Lazy (Text)
import Prelude hiding (not)


{- pseudoclasses -}

-- |Selects active elements.
active :: Selector
active = pseudoClass' "active"

-- |Selects checked elements.
checked :: Selector
checked = pseudoClass' "checked"

-- |Selects disabled elements.
disabled :: Selector
disabled = pseudoClass' "disabled"

-- |Selects elements that have no children.
empty :: Selector
empty = pseudoClass' "empty"

-- |Selects enabled elements.
enabled :: Selector
enabled = pseudoClass' "enabled"

-- |Selects elements that are the first child of its parent.
firstChild :: Selector
firstChild = pseudoClass' "first-child"

-- |Selects elements that are the first element
-- of its type that is a child of its parent.
firstOfType :: Selector
firstOfType = pseudoClass' "first-of-type"

-- |Selects elements that have focus.
focus :: Selector
focus = pseudoClass' "focus"

-- |Selects elements over which the mouse hovers.
hover :: Selector
hover = pseudoClass' "hover"

-- |Selects elements with a value within a specified range.
inRange :: Selector
inRange = pseudoClass' "in-range"

-- |Selects elements with an invalid value.
invalid :: Selector
invalid = pseudoClass' "invalid"

-- |Selects elements with a lang attribute starting with
-- the given language identifier.
lang :: Text -- ^ language identifier, eg en
     -> Selector
lang = pseudoClass "lang" . Just . Raw

-- |Selects elements that are the last child of their parent.
lastChild :: Selector
lastChild = pseudoClass' "last-child"

-- |Selects elements that are the last of their type in their
-- parent's children.
lastOfType :: Selector
lastOfType = pseudoClass' "last-of-type"

-- |Selects unvisited links
link :: Selector
link = pseudoClass' "link"

-- |Selects every element that doesn't match the given selector.
not :: Selector -- ^ selector to not match
    -> Selector
not = pseudoClass "not" . Just

-- |Selects elements that are the nth child of their parent.
nthChild :: Integer -- ^ n
         -> Selector
nthChild = pseudoClass "nth-child" . Just . Raw . fromString . show

-- |Selects elements that are the nth child of their parent,
-- counting from the last child.
nthLastChild :: Integer -- ^ n
             -> Selector
nthLastChild = pseudoClass "nth-last-child" . Just . Raw . fromString . show

-- |Selects elements that are the nth element of their parent, counting from the last child.
nthLastOfType :: Integer -- ^ n
              -> Selector
nthLastOfType = pseudoClass "nth-last-of-type" . Just . Raw . fromString . show     

-- |Selects elements that are the second of their parent.
nthOfType :: Integer -- ^ n
          -> Selector
nthOfType = pseudoClass "nth-of-type" . Just . Raw . fromString . show

-- |Selects elements that are the only element of their type
-- in the children of their parents.
onlyOfType :: Selector
onlyOfType = pseudoClass' "only-of-type"

-- |Selects elements that are the only child of their parent.
onlyChild :: Selector
onlyChild = pseudoClass' "only-child"

-- |Selects elements with no @required@ attribute specified.
optional :: Selector
optional = pseudoClass' "optional"

-- |Selects elements with a value outside a specified range.
outOfRange :: Selector
outOfRange = pseudoClass' "out-of-range"

-- |Selects elements with a @readonly@ attribute specified.
readOnly :: Selector
readOnly = pseudoClass' "read-only"

-- |Selects elements with no @readonly@ attribute specified.
readWrite :: Selector
readWrite = pseudoClass' "read-write"

-- |Selects elements with a @required@ attribute specified.
required :: Selector
required = pseudoClass' "required"

-- |Selects the document's root element.
root :: Selector
root = pseudoClass' "root"

-- |Selects the current active element.
target :: Selector
target = pseudoClass' "target"

-- |Selects all elements with a valid value.
valid :: Selector
valid = pseudoClass' "valid"

-- |Selects all visited links.
visited :: Selector
visited = pseudoClass' "visited"

{- Pseudotype combinators -}

-- |Insert content after elements.
after :: Selector
after = pseudoType' "after"

-- |Insert content before elements.
before :: Selector
before = pseudoType' "before"

-- |Selects the first letter of an element.
firstLetter :: Selector
firstLetter = pseudoType' "first-letter"

-- |Selects the first line of an element.
firstLine :: Selector
firstLine = pseudoType' "first-line"

-- |Selects the portion of an element that's selected by a user.
selection :: Selector
selection = pseudoType' "selection"