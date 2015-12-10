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
html = Raw "html"

body :: Selector
body = Raw "body"

a :: Selector
a = Raw "a"

h1 :: Selector
h1 = Raw "h1"

h2 :: Selector
h2 = Raw "h2"

h3 :: Selector
h3 = Raw "h3"

h4 :: Selector
h4 = Raw "h4"

h5 :: Selector
h5 = Raw "h5"

h6 :: Selector
h6 = Raw "h6"

p :: Selector
p = Raw "p"

br :: Selector
br = Raw "br"

hr :: Selector
hr = Raw "hr"

div :: Selector
div = Raw "div"

span :: Selector
span = Raw "span"

header :: Selector
header = Raw "header"

footer :: Selector
footer = Raw "footer"

table :: Selector
table = Raw "table"

caption :: Selector
caption = Raw "caption"

th :: Selector
th = Raw "th"

tr :: Selector
tr = Raw "tr"

td :: Selector
td = Raw "td"

thead :: Selector
thead = Raw "thead"

tbody :: Selector
tbody = Raw "tbody"

tfoot :: Selector
tfoot = Raw "tfoot"

col :: Selector
col = Raw "col"

colgroup :: Selector
colgroup = Raw "colgroup"

ul :: Selector
ul = Raw "ul"

ol :: Selector
ol = Raw "ol"

li :: Selector
li = Raw "li"

dl :: Selector
dl = Raw "dl"

dt :: Selector
dt = Raw "dt"

dd :: Selector
dd = Raw "dd"

img :: Selector
img = Raw "img"

canvas :: Selector
canvas = Raw "canvas"

iframe :: Selector
iframe = Raw "iframe"
  
form :: Selector
form = Raw "form"

input :: Selector
input = Raw "input"

textarea :: Selector
textarea = Raw "textarea"

button :: Selector
button = Raw "button"

label :: Selector
label = Raw "label"

b :: Selector
b = Raw "b"

bdi :: Selector
bdi = Raw "bdi"

bdo :: Selector
bdo = Raw "bdo"

blockquote :: Selector
blockquote = Raw "blockquote"

cite :: Selector
cite = Raw "cite"

code :: Selector
code = Raw "code"

em :: Selector
em = Raw "em"

i :: Selector
i = Raw "i"

pre :: Selector
pre = Raw "pre"

q :: Selector
q = Raw "q"

s :: Selector
s = Raw "s"

samp :: Selector
samp = Raw "samp"

small :: Selector
small = Raw "small"

strong :: Selector
strong = Raw "strong"

sub :: Selector
sub = Raw "sub"

sup :: Selector
sup = Raw "sup"

time :: Selector
time = Raw "time"

u :: Selector
u = Raw "u"

wbr :: Selector
wbr = Raw "wbr"