  globals .t
  char 't'
  nil
  prim 2 cons
  checkdef .t
  call 0 __main
  halt

proc __main
  locals
  char 'x'
  char 'y'
  char 'z'
  nil
  prim 2 cons
  prim 2 cons
  prim 2 cons
  prim 1 string->symbol
  pop
  char 'a'
  char 'b'
  char 'c'
  nil
  prim 2 cons
  prim 2 cons
  prim 2 cons
  prim 1 string->symbol
  pop
  char 'y'
  char 'o'
  nil
  prim 2 cons
  prim 2 cons
  prim 1 string->symbol
  pop
  char 'a'
  char 'b'
  char 'c'
  nil
  prim 2 cons
  prim 2 cons
  prim 2 cons
  prim 1 string->symbol
  char 'a'
  char 'b'
  char 'c'
  nil
  prim 2 cons
  prim 2 cons
  prim 2 cons
  prim 1 string->symbol
  prim 2 eq?
  if
  char 'y'
  prim 1 write-char
  else
  global .t
  if
  char 'n'
  prim 1 write-char
  else
  false
  then
  then
  pop
  char '\n'
  prim 1 write-char
  return
