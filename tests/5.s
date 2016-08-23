  globals .t .f false symbols
  char 't'
  nil
  prim 2 cons
  checkdef .t
  char 'f'
  nil
  prim 2 cons
  checkdef .f
  false
  checkdef false
  global .f
  nil
  prim 2 cons
  nil
  prim 2 cons
  checkdef symbols
  call 0 __main
  halt

proc __main
  locals
  nil
  call 1 print
  pop
  char 'a'
  char 'b'
  char 'c'
  nil
  prim 2 cons
  prim 2 cons
  prim 2 cons
  call 1 print
  pop
  char 'a'
  char 'b'
  nil
  prim 2 cons
  nil
  prim 2 cons
  prim 2 cons
  call 1 print
  pop
  char 'h'
  char 'e'
  char 'l'
  char 'l'
  char 'o'
  nil
  prim 2 cons
  prim 2 cons
  prim 2 cons
  prim 2 cons
  prim 2 cons
  call 1 intern
  global .f
  char 'w'
  char 'o'
  char 'r'
  char 'l'
  char 'd'
  nil
  prim 2 cons
  prim 2 cons
  prim 2 cons
  prim 2 cons
  prim 2 cons
  nil
  prim 2 cons
  prim 2 cons
  prim 2 cons
  tailcall 1 print

proc write-string
  locals chars
  local chars
  prim 1 null?
  if
  global false
  return
  else
  global .t
  if
  local chars
  prim 1 car
  prim 1 write-char
  pop
  local chars
  prim 1 cdr
  tailcall 1 write-string
  else
  false
  return
  then
  then

proc write-each
  locals xs
  local xs
  prim 1 null?
  if
  global false
  return
  else
  global .t
  if
  char ' '
  prim 1 write-char
  pop
  local xs
  prim 1 car
  call 1 write
  pop
  local xs
  prim 1 cdr
  tailcall 1 write-each
  else
  false
  return
  then
  then

proc write
  locals x
  local x
  prim 1 null?
  if
  char '('
  char ')'
  nil
  prim 2 cons
  prim 2 cons
  tailcall 1 write-string
  else
  local x
  prim 1 char?
  if
  char '\\'
  prim 1 write-char
  pop
  local x
  prim 1 write-char
  return
  else
  local x
  call 1 string?
  if
  local x
  call 1 symbol?
  if
  local x
  tailcall 1 write-string
  else
  global .t
  if
  char '"'
  prim 1 write-char
  pop
  local x
  call 1 write-string
  pop
  char '"'
  prim 1 write-char
  return
  else
  false
  return
  then
  then
  else
  global .t
  if
  char '('
  prim 1 write-char
  pop
  local x
  prim 1 car
  call 1 write
  pop
  local x
  prim 1 cdr
  call 1 write-each
  pop
  char ')'
  prim 1 write-char
  return
  else
  false
  return
  then
  then
  then
  then

proc print
  locals x
  local x
  call 1 write
  pop
  char '\n'
  prim 1 write-char
  return

proc intern-lookup
  locals s syms
  local syms
  prim 1 null?
  if
  local s
  global symbols
  call 2 cons!
  pop
  local s
  return
  else
  local s
  local syms
  prim 1 car
  call 2 string=?
  if
  local syms
  prim 1 car
  return
  else
  global .t
  if
  local s
  local syms
  prim 1 cdr
  tailcall 2 intern-lookup
  else
  false
  return
  then
  then
  then

proc intern
  locals s
  local s
  global symbols
  prim 1 car
  tailcall 2 intern-lookup

proc symbol?
  locals x
  local x
  global symbols
  prim 1 car
  tailcall 2 memq?

proc cons!
  locals x xs-cell
  local xs-cell
  local x
  local xs-cell
  prim 1 car
  prim 2 cons
  prim 2 set-car!
  return

proc memq?
  locals x xs
  local xs
  prim 1 null?
  if
  global false
  return
  else
  local x
  local xs
  prim 1 car
  prim 2 eq?
  if
  global .t
  return
  else
  global .t
  if
  local x
  local xs
  prim 1 cdr
  tailcall 2 memq?
  else
  false
  return
  then
  then
  then

proc string=?
  locals s t
  local s
  prim 1 null?
  if
  local t
  prim 1 null?
  return
  else
  local t
  prim 1 null?
  if
  global false
  return
  else
  local s
  prim 1 car
  local t
  prim 1 car
  prim 2 eq?
  if
  local s
  prim 1 cdr
  local t
  prim 1 cdr
  tailcall 2 string=?
  else
  global .t
  if
  global false
  return
  else
  false
  return
  then
  then
  then
  then

proc string?
  locals x
  local x
  prim 1 null?
  if
  global .t
  return
  else
  local x
  prim 1 char?
  if
  global false
  return
  else
  local x
  prim 1 car
  prim 1 char?
  if
  local x
  prim 1 cdr
  tailcall 1 string?
  else
  global .t
  if
  global false
  return
  else
  false
  return
  then
  then
  then
  then
