import sys

opcodes = {}

primitives = ['read-char peek-char abort self ?'.split(),
              'null? boolean? char? symbol? pair? ' \
                 'string->symbol symbol->string ' \
                 'car cdr write-char fixnum? pid?'.split(),
              'eq? cons set-car! + - * quotient remainder < !'.split()]

directives = 'halt checkdef char fixnum nil false true ' \
             'prim enframe call tailcall return ' \
             'global local if pop spawn defglobal'.split()

def set_up():
    ops = 'halt enframe call tailcall nparams return global local ' \
          'branch jump pop ' \
          'char nil false true eq? null? boolean? char? pair? ' \
	  'cons car cdr set-car! read-char peek-char write-char abort ' \
          'symbol? string->symbol symbol->string ' \
          '+ - * quotient remainder fixnum? fixnum < pid? self spawn ! ? ' \
          'defglobal'
    a = ops.split()
    for code, name in zip(range(len(a)), a):
        opcodes[name] = code

for_real    = None
the_file    = None
line        = None
tokens      = None
here        = None
the_globals = None
the_locals  = None
the_procs   = None
targets     = None

procs_list  = []
syms_file   = file('syms', 'wb')

def main(filename):
    global the_procs, the_globals, the_locals, targets
    set_up()
    the_procs   = {}
    the_globals = {}
    the_locals  = {}
    targets     = {}
    assemble(filename, False)
    write_symbols()
    assemble(filename, True)

def write_symbols():
    write16(sys.stdout, len(procs_list))
    for name, address in procs_list:
        write16(sys.stdout, address)
        write8(sys.stdout, len(name))
        sys.stdout.write(name)

def write16(out, value):
    hi, lo = divmod(value, 256)
    write8(out, hi)
    write8(out, lo)

def write8(out, value):
    assert 0 <= value and value < 256
    out.write(chr(value))

def assemble(filename, writing):
    global the_file, for_real, here
    for_real = writing
    the_file = file(filename, 'r')
    here = 0
    eat()
    program()
    expect('EOF')

def eat():
    global line, tokens
    while True:
        line = the_file.readline()
        if line == '':
	    line = 'EOF'
            break
        if line.strip() != '':
            break
    tokens = line.split()

def expect(keyword):
    if tokens[0] != keyword:
        panic('Expected %s instead of %s' % (keyword, tokens[0]))

def program():
    globals_()
    code()
    procs()

def globals_():
    expect('globals')
    namelist(the_globals)
    eat()

def code():
    while tokens[0] in directives:
        do_instruc()

def do_instruc():
    if tokens[0] == 'prim':
        arity = int(tokens[1])
        if tokens[2] not in primitives[arity]:
            panic('Unknown primitive')
        output(opcodes[tokens[2]])
        eat()
    elif tokens[0] == 'char':
	do_char()
        eat()
    elif tokens[0] == 'fixnum':
        output(opcodes['fixnum'])
        encode_int32(int(tokens[1]))
        eat()
    elif tokens[0] == 'global':
        output(opcodes['global'])
        encode16(the_globals[tokens[1]])
        eat()
    elif tokens[0] == 'defglobal':
        output(opcodes['defglobal'])
	eat()
    elif tokens[0] == 'if':
        branch_from = here
        output(opcodes['branch'])
	encode_jump_offset(branch_from)
        eat()
	code()
	expect('else')
	eat()
	jump_from = here
	output(opcodes['jump'])
	encode_jump_offset(jump_from)
	resolve(branch_from)
	code()
	expect('then')
	eat()
	resolve(jump_from)
    elif tokens[0] == 'local':
        output(opcodes['local'])
        output(the_locals[tokens[1]])
        eat()
    elif tokens[0] == 'call':
        output(opcodes['call'])
        output(int(tokens[1]))
        encode_address(tokens[2])
        eat()
    elif tokens[0] == 'tailcall':
        output(opcodes['tailcall'])
        output(int(tokens[1]))
        encode_address(tokens[2])
        eat()
    elif tokens[0] == 'spawn':
        output(opcodes['spawn'])
        output(int(tokens[1]))
        encode_address(tokens[2])
        eat()
    elif tokens[0] == 'checkdef':
	eat()
    elif tokens[0] in opcodes:
        output(opcodes[tokens[0]])
        eat()
    else:
        panic('wtf? ' + tokens[0])

def do_char():
    q = line.index("'")
    assert 0 <= q
    r = line.rindex("'")
    assert q < r
    if q + 2 == r:
        c = line[q+1]
        assert c != '\\'
    elif q + 3 == r:
        assert line[q+1] == '\\'
        c = line[q+2]
        assert c in ['n', '\\', "'"]
        if c == 'n': c = '\n'
    else:
        panic('Bad char literal')
    output(opcodes['char'])
    output(ord(c))

def output(byte):
    if for_real:
        write8(sys.stdout, byte)
    global here
    here += 1

def encode_address(name):
    if for_real:
        encode16(the_procs[name])
    else:
        encode16(0)

def encode_jump_offset(from_address):
    if for_real:
	encode16(targets[from_address] - from_address)
    else:
	encode16(0)

def resolve(from_address):
    targets[from_address] = here

def encode_int32(value):
    output((value >> 24) & 0xFF)
    output((value >> 16) & 0xFF)
    output((value >>  8) & 0xFF)
    output((value >>  0) & 0xFF)

def encode16(value):
    hi, lo = divmod(value, 256)
    output(hi)
    output(lo)

def procs():
    while tokens[0] == 'proc':
        do_proc()

def do_proc():
    expect('proc')
    the_procs[tokens[1]] = here
    if not for_real:
        procs_list.append((tokens[1], here))
    else:
        syms_file.write('%s\t%d\n' % (tokens[1], here))
    eat()
    expect('locals')
    namelist(the_locals)
    output(opcodes['nparams'])
    output(len(tokens[1:]))
    eat()
    code()

def namelist(table):
    table.clear()
    a = tokens[1:]
    for code, name in zip(range(len(a)), a):
        table[name] = code

def panic(plaint):
    #sys.stderr.write(plaint + '\n')
    raise plaint

main(sys.argv[1])
