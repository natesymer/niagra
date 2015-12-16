{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.Selector.Tags
(
  html,
  body,
  a,
  h1,
  h2,
  h3,
  h4,
  h5,
  h6,
  p,
  br,
  hr,
  div,
  span,
  header,
  footer,
  table,
  caption,
  th,
  tr,
  td,
  thead,
  tbody,
  tfoot,
  col,
  colgroup,
  ul,
  ol,
  li,
  dl,
  dt,
  dd,
  img,
  canvas,
  iframe,
  form,
  input,
  textarea,
  button,
  label,
  b,
  bdi,
  bdo,
  blockquote,
  cite,
  code,
  em,
  i,
  pre,
  q,
  s,
  samp,
  small,
  strong,
  sub,
  sup,
  time,
  u,
  wbr
)
where
  
import Data.Niagra.Selector
import Prelude hiding (div,span)

html :: Selector
html =  "html"

body :: Selector
body =  "body"

a :: Selector
a =  "a"

h1 :: Selector
h1 =  "h1"

h2 :: Selector
h2 =  "h2"

h3 :: Selector
h3 =  "h3"

h4 :: Selector
h4 =  "h4"

h5 :: Selector
h5 =  "h5"

h6 :: Selector
h6 =  "h6"

p :: Selector
p =  "p"

br :: Selector
br =  "br"

hr :: Selector
hr =  "hr"

div :: Selector
div =  "div"

span :: Selector
span =  "span"

header :: Selector
header =  "header"

footer :: Selector
footer =  "footer"

table :: Selector
table =  "table"

caption :: Selector
caption =  "caption"

th :: Selector
th =  "th"

tr :: Selector
tr =  "tr"

td :: Selector
td =  "td"

thead :: Selector
thead =  "thead"

tbody :: Selector
tbody =  "tbody"

tfoot :: Selector
tfoot =  "tfoot"

col :: Selector
col =  "col"

colgroup :: Selector
colgroup =  "colgroup"

ul :: Selector
ul =  "ul"

ol :: Selector
ol =  "ol"

li :: Selector
li =  "li"

dl :: Selector
dl =  "dl"

dt :: Selector
dt =  "dt"

dd :: Selector
dd =  "dd"

img :: Selector
img =  "img"

canvas :: Selector
canvas =  "canvas"

iframe :: Selector
iframe =  "iframe"
  
form :: Selector
form =  "form"

input :: Selector
input =  "input"

textarea :: Selector
textarea = "textarea"

button :: Selector
button = "button"

label :: Selector
label = "label"

b :: Selector
b = "b"

bdi :: Selector
bdi = "bdi"

bdo :: Selector
bdo = "bdo"

blockquote :: Selector
blockquote = "blockquote"

cite :: Selector
cite = "cite"

code :: Selector
code = "code"

em :: Selector
em = "em"

i :: Selector
i = "i"

pre :: Selector
pre = "pre"

q :: Selector
q = "q"

s :: Selector
s = "s"

samp :: Selector
samp = "samp"

small :: Selector
small = "small"

strong :: Selector
strong = "strong"

sub :: Selector
sub = "sub"

sup :: Selector
sup = "sup"

time :: Selector
time = "time"

u :: Selector
u = "u"

wbr :: Selector
wbr = "wbr"