#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum { symbol_table_size  =  64 * 1024 };
enum { entry_table_size   =   1 * 1024 };
enum { process_table_size =   1 * 1024 };
enum { globals_size       =   4 * 1024 };
enum { stack_size         =   4 * 1024 }; // XXX that's still pretty big
enum { heap_size          = 512 * 1024 };
enum { code_size          =  64 * 1024 };

enum { tracing = 0 };
#define TRACEOUT stderr

#define TRACE1(f,x)     do { if (tracing) fprintf (TRACEOUT,f,x);   } while (0);
#define TRACE2(f,x,y)   do { if (tracing) fprintf (TRACEOUT,f,x,y); } while (0);

static void panic (const char *plaint) {
  fprintf (stderr, "%s\n", plaint);
  exit (1);
}

static void *allot (size_t size) {
  void *result = malloc (size);
  if (NULL == result && 0 != size)
    panic ("Out of memory");
  return result;
}

// syms_space holds a chain of strings from index 0 up below syms_ptr.
// The first byte of each entry is its length. There are no duplicates.
static unsigned char syms_space[symbol_table_size];
static unsigned syms_ptr = 0;

// For each of the first n_entry_points symbols, there's a value in
// entry_points giving the starting address of the function with that
// name. These are in ascending order by address.
static unsigned entry_points[entry_table_size];
static unsigned n_entry_points = 0;

static unsigned nth_symbol (unsigned n) {
  unsigned s = 0;
  unsigned i;
  for (i = 0; i < n; ++i) {
    assert (s < syms_ptr);
    s += 1 + syms_space[s];
  }
  assert (s < syms_ptr);
  return s;
}

static void print_symbol (unsigned s) {
  unsigned n = syms_space[s];
  printf ("%*.*s\n", n, n, syms_space + s + 1);
}

// Print the name of the code block containing :address.
static void print_proc_containing (unsigned address) {
  unsigned proc;
  if (0 < n_entry_points && address < entry_points[0]) {
    printf ("initialization\n");
    return;
  }
  for (proc = 0; proc + 1 < n_entry_points; ++proc)
    if (entry_points[proc] <= address && address < entry_points[proc + 1]) {
      print_symbol (nth_symbol (proc));
      return;
    }
  // TODO: simplify by searching from the end, above
  if (proc < n_entry_points && entry_points[proc] <= address) {
    print_symbol (nth_symbol (proc));
    return;
  }
  printf ("wtf?\n");
}

static unsigned read8 (FILE *f) {
  int result = getc (f);
  if (EOF == result)
    panic (ferror (f) ? strerror (errno) : "Premature EOF in read8");
  return 0xFF & result;
}

static unsigned read16 (FILE *f) {
  unsigned hi = read8 (f);
  unsigned lo = read8 (f);
  return (hi << 8) | lo;
}

static void read_block (unsigned char *dest, unsigned size, FILE *f) {
  while (0 < size) {
    size_t n = fread (dest, 1, size, f);
    if (0 == n)
      panic (ferror (f) ? strerror (errno) : "Premature EOF in read_block");
    dest += n, size -= n;
  }
}

static void load_syms (FILE *f) {
  unsigned n_entries = read16 (f);
  unsigned i;
  for (i = 0; i < n_entries; ++i) {
    unsigned address = read16 (f);
    assert (n_entry_points < entry_table_size);
    entry_points[n_entry_points++] = address;
    unsigned name_length = read8 (f);
    assert (syms_ptr + 1 + name_length <= symbol_table_size);
    syms_space[syms_ptr] = name_length;
    read_block (syms_space + syms_ptr + 1, name_length, f);
    syms_ptr += 1 + name_length;
  }
}

enum { tag_bits = 3 };
typedef enum  { a_pair, nil, a_fixnum, a_bool, a_symbol, a_pid } Tag;
static const char *tag_names[] = {
  "pair", "nil", "fixnum", "boolean", "symbol", "pid",
};
typedef unsigned Obj;
static Tag      get_tag (Obj x)      { return ~(~0 << tag_bits) & x; }
#define         ENTAG(tag, value)    ( (tag) | ((value) << tag_bits) )
static Obj      entag (Tag tag, unsigned value)
                                     { return ENTAG (tag, value); }
static unsigned untag_fixnum (Obj x) { assert (a_fixnum == get_tag (x));
                                       return x >> tag_bits; }


typedef struct VM VM;
struct VM {  // A process
  const unsigned char *pc;    // Program counter
  unsigned sp;                // Stack pointer
  unsigned bp;                // Base pointer
  Obj      stack[stack_size];
  Obj      mailbox_head;          // A queue of messages, represented by ...
  Obj      mailbox_tail_reversed; // ... a pair of lists.
  unsigned pid;               // Process ID
  VM      *next;              // The next process in the run queue
};

static Obj globals[globals_size];
static unsigned n_globals = 0;

static unsigned char code[code_size];

static VM *the_processes[process_table_size] = { };

static unsigned next_pid = 1;

static unsigned vm_index (const VM *vm) {
  unsigned i;
  for (i = 0; i < process_table_size; ++i)
    if (the_processes[i] == vm)
      return i;
  if (NULL == vm)
    panic ("Process table full");
  panic ("Can't happen: missing process"); assert (0);
}

static VM *pid_to_vm (unsigned pid) {
  unsigned i;
  for (i = 0; i < process_table_size; ++i)
    if (NULL != the_processes[i] && the_processes[i]->pid == pid)
      return the_processes[i];
  return NULL;
}

// Pre: vm is in the process table but not on the run queue
static void unmake_vm (VM *vm) {
  the_processes[vm_index (vm)] = NULL;
  free (vm);
}

static VM *make_vm (void) {
  VM *vm = allot (sizeof *vm);   // XXX raise an exception on error
  vm->pc = code;
  vm->sp = -1;
  vm->bp = 0;
  vm->mailbox_head = nil;
  vm->mailbox_tail_reversed = nil;
  if (next_pid != ((next_pid << tag_bits) >> tag_bits))
    panic ("Out of new PIDs");
  vm->pid = next_pid++;
  vm->next = NULL;
  the_processes[vm_index (NULL)] = vm;
  return vm;
}

static VM *run_queue_predecessor;

static void run_queue_advance (void) {
  run_queue_predecessor = run_queue_predecessor->next;
}

static VM *run_queue_front (void) {
  return run_queue_predecessor->next;
}

static void run_queue_pop_front (void) {
  if (run_queue_predecessor->next == run_queue_predecessor)
    exit (0);  // No more processes to run.
  run_queue_predecessor->next = run_queue_predecessor->next->next;
}

static void run_queue_push_back (VM *vm) {
  VM *head = run_queue_predecessor->next;
  run_queue_predecessor->next = vm;
  vm->next = head;
  run_queue_predecessor = vm;
}

static void make_startup_vm (void) {
  VM *vm = make_vm ();
  run_queue_predecessor = vm->next = vm;
}

#define         TOP(vm)              ( vm->stack[vm->sp] )
static Obj      pop  (VM *vm)        { return vm->stack[vm->sp--]; }
static void     push (VM *vm, Obj x) { assert (vm->sp + 1 < stack_size);
                                       vm->stack[++vm->sp] = x; }

static void print_traceback (VM *vm) {
  unsigned p = vm->bp;
  for (; 0 < p; p = untag_fixnum (vm->stack[p-2]))
    print_proc_containing (untag_fixnum (vm->stack[p-1]));
}

static void panic_traceback (VM *vm, const char *plaint) {
  fprintf (stderr, "%s\n", plaint);
  print_traceback (vm);
  exit (1);
}

static unsigned untag (VM *vm, Tag tag, Obj x) {
  if (tag != get_tag (x)) {
    fprintf (stderr, "Bad type: %s (expected %s)\n",
             tag_names[get_tag(x)], tag_names[tag]);
    print_traceback (vm);
    exit (1);
  }
  return x >> tag_bits;
}

static int is_fixnum (Obj x) {
  return a_fixnum == get_tag (x);
}
static int fixnum_value (VM *vm, Obj x) {
  unsigned v = untag (vm, a_fixnum, x);
  return (((int)v) << tag_bits) >> tag_bits;   // XXX unportable
}
static Obj make_fixnum (VM *vm, int value) {
  Obj result = entag (a_fixnum, (unsigned) value);
  if (value != fixnum_value (vm, result))
    panic_traceback (vm, "Overflow");
  return result;
}
static Obj make_long (VM *vm, long long value) {
  Obj result = entag (a_fixnum, (unsigned) value);
  if (value != fixnum_value (vm, result))
    panic_traceback (vm, "Overflow");
  return result;
}

static int is_char (Obj x) {
  return (a_fixnum == get_tag (x)
          && (x >> tag_bits) < 256);
}
static unsigned char char_value (VM *vm, Obj c) {
  return untag (vm, a_fixnum, c);  // XXX range check
}


static Obj      heap[heap_size][2];
static char     marks[heap_size];
static unsigned hp = 0;

static unsigned heap_index (VM *vm, Obj x) {
  unsigned p = untag (vm, a_pair, x);
  assert (p < heap_size);
  return p;
}
static Obj car (VM *vm, Obj x)     { return heap[heap_index (vm, x)][0]; }
static Obj cdr (VM *vm, Obj x)     { return heap[heap_index (vm, x)][1]; }
static void set_car (VM *vm, Obj x, Obj y) { heap[heap_index (vm, x)][0] = y; }

static void mark (VM *vm, Obj x) {
  while (get_tag (x) == a_pair && !marks[heap_index (vm, x)]) {
    marks[heap_index (vm, x)] = 1;
    mark (vm, car (vm, x));
    x = cdr (vm, x);
  }
}
static void sweep (void) {
  while (hp < heap_size && marks[hp])
    marks[hp++] = 0;
}
static void gc (VM *vm, Obj car, Obj cdr) {
  unsigned i, j;
  mark (vm, car); mark (vm, cdr);
  for (i = 0; i <= n_globals; ++i)
    mark (vm, globals[i]);
  for (i = 0; i < process_table_size; ++i) {
    VM *vi = the_processes[i];
    if (NULL != vi) {
      mark (vi, vi->mailbox_head);
      mark (vi, vi->mailbox_tail_reversed);
      for (j = 0; j <= vi->sp; ++j)
        mark (vi, vi->stack[j]);
    }
  }
  hp = 0;
}

static Obj cons (VM *vm, Obj car, Obj cdr) {
  sweep ();
  if (heap_size <= hp) {
    gc (vm, car, cdr);
    sweep ();
    if (heap_size <= hp)
      panic ("Heap full"); }
    heap[hp][0] = car;
    heap[hp][1] = cdr;
    return entag (a_pair, hp++);
}

static Obj reverse (VM *vm, Obj xs) {
  Obj acc = nil;
  for (; nil != xs; xs = cdr (vm, xs))
    acc = cons (vm, car (vm, xs), acc);
  return acc;
}

static void send (VM *vm, Obj message) {
  vm->mailbox_tail_reversed = cons (vm, message, vm->mailbox_tail_reversed);
}

// Remove the first message from vm's mailbox and push it on its stack,
// if there is one; return true iff so.
static int receive (VM *vm) {
  if (nil == vm->mailbox_head) {
    vm->mailbox_head = reverse (vm, vm->mailbox_tail_reversed);
    vm->mailbox_tail_reversed = nil;
  }
  if (nil == vm->mailbox_head)
    return 0;
  push (vm, car (vm, vm->mailbox_head));
  vm->mailbox_head = cdr (vm, vm->mailbox_head);
  return 1;
}

static void add_symbol (VM *vm, Obj chars) {
  unsigned i;
  for (i = 1; nil != chars; ++i, chars = cdr (vm, chars)) {
    if (symbol_table_size <= syms_ptr + i)
      panic ("Symbol table full");
    syms_space[syms_ptr + i] = char_value (vm, car (vm, chars));
  }
  assert (i - 1 < 256);
  syms_space[syms_ptr] = i - 1;
  syms_ptr += i;
}

static int is_interned_at (VM *vm, Obj chars, unsigned s) {
  unsigned i, length = syms_space[s];
  for (i = 1; i <= length; ++i, chars = cdr (vm, chars))
    if (a_pair != get_tag (chars)
        || !is_char (car (vm, chars))
        || syms_space[s + i] != char_value (vm, car (vm, chars)))
      return 0;
  return nil == chars;
}

static unsigned intern (VM *vm, Obj chars) {
  unsigned i;
  for (i = 0; i < syms_ptr; i += 1 + syms_space[i])
    if (is_interned_at (vm, chars, i))
      return i;
  add_symbol (vm, chars);
  return i;
}

#define     sym_f                 ( ENTAG (a_bool, 0) )
#define     sym_t                 ( ENTAG (a_bool, 1) )
static Obj  make_flag (int flag)  { return flag ? sym_t : sym_f; }

static int read_char (VM *vm)     { int c = getchar ();
                                    push (vm,
                                          EOF == c 
                                            ? sym_f
                                            : make_fixnum (vm, c));
                                    return c; }

#define DEF(prim) static void prim (VM *vm)
DEF(prim2_eqP)        { Obj z = pop (vm);
                        TOP (vm) = make_flag (TOP (vm) == z); }
DEF(prim1_nullP)      { TOP (vm) = make_flag (nil == TOP (vm)); }
DEF(prim1_booleanP)   { TOP (vm) = make_flag (a_bool == get_tag (TOP (vm))); }
DEF(prim1_charP)      { TOP (vm) = make_flag (is_char (TOP (vm))); }
DEF(prim1_symbolP)    { TOP (vm) = make_flag (a_symbol == get_tag (TOP (vm))); }
DEF(prim1_pairP)      { TOP (vm) = make_flag (a_pair == get_tag (TOP (vm))); }
DEF(prim2_cons)       { Obj z = pop (vm); TOP (vm) = cons (vm, TOP (vm), z); }
DEF(prim1_car)        { TOP (vm) = car (vm, TOP (vm)); }
DEF(prim1_cdr)        { TOP (vm) = cdr (vm, TOP (vm)); }
DEF(prim2_set_carB)   { Obj z = pop (vm); set_car (vm, TOP (vm), z); 
                        TOP (vm) = sym_f; }
DEF(prim0_read_char)  { (void) read_char (vm); }
DEF(prim0_peek_char)  { ungetc (read_char (vm), stdin); }
DEF(prim1_write_char) { putchar (char_value (vm, TOP (vm))); TOP (vm) = sym_f; }
DEF(prim0_abort)      { (void) vm; exit (1); }
DEF(prim1_string_Gsymbol)
                      { TOP (vm) = entag (a_symbol, intern (vm, TOP (vm))); }
DEF(prim1_symbol_Gstring) {
  unsigned s = untag (vm, a_symbol, TOP (vm));
  unsigned i, length = syms_space[s];
  TOP (vm) = nil;
  for (i = length; 0 < i; --i)
    TOP (vm) = cons (vm, make_fixnum (vm, syms_space[s + i]), TOP (vm));
}

static Obj add (VM *vm, Obj a, Obj b) {
  return make_fixnum (vm, fixnum_value (vm, a) + fixnum_value (vm, b));
}
static Obj sub (VM *vm, Obj a, Obj b) {
  return make_fixnum (vm, fixnum_value (vm, a) - fixnum_value (vm, b));
}
static Obj mul (VM *vm, Obj a, Obj b) {
  return make_long (vm, (long long)fixnum_value (vm, a) * fixnum_value (vm, b));
}
static Obj divide (VM *vm, Obj a, Obj b) {
  int bv = fixnum_value (vm, b);
  if (bv == 0)
    panic_traceback (vm, "Division by 0");
  return make_fixnum (vm, fixnum_value (vm, a) / bv);
}
static Obj rem (VM *vm, Obj a, Obj b) {
  int bv = fixnum_value (vm, b);
  if (bv == 0)
    panic_traceback (vm, "Division by 0");
  return make_fixnum (vm, fixnum_value (vm, a) % bv);
}

DEF(prim2_add)        { Obj z = pop (vm); TOP (vm) = add (vm, TOP (vm), z); }
DEF(prim2_sub)        { Obj z = pop (vm); TOP (vm) = sub (vm, TOP (vm), z); }
DEF(prim2_mul)        { Obj z = pop (vm); TOP (vm) = mul (vm, TOP (vm), z); }
DEF(prim2_div)        { Obj z = pop (vm); TOP (vm) = divide (vm, TOP (vm), z); }
DEF(prim2_rem)        { Obj z = pop (vm); TOP (vm) = rem (vm, TOP (vm), z); }

DEF(prim1_fixnumP)    { TOP (vm) = make_flag (is_fixnum (TOP (vm))); }

DEF(prim2_lt)         { Obj z = pop (vm);
                        TOP (vm) = make_flag (fixnum_value (vm, TOP (vm))
                                            < fixnum_value (vm, z)); }

DEF(prim1_pidP)       { TOP (vm) = make_flag (a_pid == get_tag (TOP (vm))); }

DEF(prim2_send)       { Obj z = pop (vm);
                        VM *receiver = pid_to_vm (untag (vm, a_pid, TOP (vm)));
                        if (NULL != receiver)
                          send (receiver, z);
                        TOP (vm) = sym_f; }

enum {
  op_halt,
  op_enframe,
  op_call,
  op_tailcall,
  op_nparams,
  op_return,
  op_global,
  op_local,
  op_branch,
  op_jump,
  op_pop,
  op_char,
  op_nil,
  op_false,
  op_true,
  op_eqP,
  op_nullP,
  op_booleanP,
  op_charP,
  op_pairP,
  op_cons,
  op_car,
  op_cdr,
  op_set_carB,
  op_read_char,
  op_peek_char,
  op_write_char,
  op_abort,
  op_symbolP,
  op_string_Gsymbol,
  op_symbol_Gstring,
  op_add,
  op_sub,
  op_mul,
  op_div,
  op_rem,
  op_fixnumP,
  op_fixnum,
  op_lt,
  op_pidP,
  op_self,
  op_spawn,
  op_send,
  op_receive,
  op_defglobal,
};

static int find_halt (void) {
  unsigned i;
  for (i = 0; i < code_size; ++i)
    if (op_halt == code[i])
      return i;
  panic ("HALT instruction missing"); assert (0);
}

static unsigned decode16 (const unsigned char *address) {
  return (address[0]<<8) + address[1];
}

static int decode_int32 (const unsigned char *address) {
  unsigned u = (((((address[0]<<8)+address[1])<<8)+address[2])<<8)+address[3];
  return (int) u;		// XXX sign-extend portably
}

#define CASE(value) \
    break; case value: \
      TRACE2 ("\n%zd %s\t", vm->pc - code, #value);

#define NEXT(incr)  ( vm->pc += (incr) )

// Run from the front of the run queue for up to the given number of steps.
static void run (unsigned steps) {
  VM *vm = run_queue_front ();

// A stack frame looks like this, growing upwards:
//   sp[0]: topmost temporary
//   ...temporaries...
//   bp[n-1]: rightmost argument (where n is the number of arguments)
//   ...
//   bp[0]:   leftmost argument
//   bp[-1]:  return address (encoded as a_fixnum)
//   bp[-2]:  old bp         (encoded as a_fixnum)
//            (this is also where the return value will go)
// (Except that when starting up there's nothing below the temporaries.)
// When we first enter the function, there are no temporaries; when we're
// ready to execute op_return, there's only one temporary left, the return
// value.

  for (; 0 < steps; --steps) {
    // An instruction may take up to 5 bytes:
    assert ((unsigned) (vm->pc - code) < code_size - 4u);
    switch (vm->pc[0]) {
      default:             panic ("Unknown opcode");

      CASE(op_halt)        run_queue_pop_front (); unmake_vm (vm); return;
      CASE(op_enframe)     push (vm, make_fixnum (vm, vm->bp));
                           push (vm, make_fixnum (vm, 0)); // slot for return
                           NEXT (1);
      CASE(op_call)        TRACE1 ("%u", vm->pc[1]);
                           vm->bp = vm->sp - vm->pc[1] + 1;
                           vm->stack[vm->bp-1] =
                             make_fixnum (vm, vm->pc - code + 4);
                           vm->pc = code + decode16 (vm->pc + 2);
      CASE(op_tailcall)  { unsigned nargs = vm->pc[1];
                           memmove (vm->stack + vm->bp,
                                    vm->stack + vm->sp - nargs + 1, 
                                    nargs * sizeof vm->stack[0]);
                           vm->sp = vm->bp + nargs - 1;
                           vm->pc = code + decode16 (vm->pc + 2); }
      CASE(op_nparams)     TRACE1 ("%u", vm->pc[1]);
                           if (vm->pc[1] != vm->sp - vm->bp + 1)
                             panic_traceback (vm, "Bad arity");
                           NEXT (2);
      CASE(op_return)    { unsigned old_bp = untag (vm, a_fixnum,
                                                    vm->stack[vm->bp - 2]);
                           vm->pc = code + untag (vm, a_fixnum,
                                                  vm->stack[vm->bp - 1]);
                           vm->stack[vm->bp - 2] = vm->stack[vm->sp];
                           vm->sp = vm->bp - 2; 
                           vm->bp = old_bp; }
      CASE(op_global)    { unsigned g = decode16 (vm->pc + 1);
                           TRACE1 ("%u", g);
                           if (n_globals <= g)
                             panic_traceback (vm, "Uninitialized global");
                           push (vm, globals[g]);
                           NEXT (3); }
      CASE(op_local)       TRACE1 ("%u", vm->pc[1]);
                           push (vm, vm->stack[vm->bp + vm->pc[1]]); NEXT (2);
      CASE(op_branch)      NEXT (sym_f == pop (vm) ? decode16 (vm->pc + 1) : 3);
      CASE(op_jump)        NEXT (decode16 (vm->pc + 1));
      CASE(op_pop)         pop (vm);                            NEXT (1);
      CASE(op_char)        TRACE1 ("%c", vm->pc[1]);
                           push (vm, make_fixnum (vm, vm->pc[1])); NEXT (2);
      CASE(op_nil)         push (vm, nil);                      NEXT (1);
      CASE(op_false)       push (vm, sym_f);                    NEXT (1);
      CASE(op_true)        push (vm, sym_t);                    NEXT (1);

      CASE(op_eqP)         prim2_eqP (vm);        NEXT (1);
      CASE(op_nullP)       prim1_nullP (vm);      NEXT (1);
      CASE(op_booleanP)    prim1_booleanP (vm);   NEXT (1);
      CASE(op_charP)       prim1_charP (vm);      NEXT (1);
      CASE(op_pairP)       prim1_pairP (vm);      NEXT (1);
      CASE(op_cons)        prim2_cons (vm);       NEXT (1);
      CASE(op_car)         prim1_car (vm);        NEXT (1);
      CASE(op_cdr)         prim1_cdr (vm);        NEXT (1);
      CASE(op_set_carB)    prim2_set_carB (vm);   NEXT (1);
      CASE(op_read_char)   prim0_read_char (vm);  NEXT (1);
      CASE(op_peek_char)   prim0_peek_char (vm);  NEXT (1);
      CASE(op_write_char)  prim1_write_char (vm); NEXT (1);
      CASE(op_abort)       prim0_abort (vm);      NEXT (1);

      CASE(op_symbolP)     prim1_symbolP (vm);    NEXT (1);
      CASE(op_string_Gsymbol) prim1_string_Gsymbol (vm); NEXT (1);
      CASE(op_symbol_Gstring) prim1_symbol_Gstring (vm); NEXT (1);

      CASE(op_add)         prim2_add (vm);        NEXT (1);
      CASE(op_sub)         prim2_sub (vm);        NEXT (1);
      CASE(op_mul)         prim2_mul (vm);        NEXT (1);
      CASE(op_div)         prim2_div (vm);        NEXT (1);
      CASE(op_rem)         prim2_rem (vm);        NEXT (1);

      CASE(op_fixnumP)     prim1_fixnumP (vm);    NEXT (1);

      CASE(op_fixnum)      TRACE1 ("%d", decode_int32 (vm->pc + 1));
                           push (vm,
                                 make_fixnum (vm, decode_int32 (vm->pc + 1)));
                           NEXT (5);

      CASE(op_lt)          prim2_lt (vm);         NEXT (1);

      CASE(op_pidP)        prim1_pidP (vm);       NEXT (1);
      CASE(op_self)        push (vm, entag (a_pid, vm->pid)); NEXT (1);

      CASE(op_spawn)       TRACE1 ("%u", vm->pc[1]);
                         { VM *nvm = make_vm ();
                           unsigned n = vm->pc[1];
                           nvm->pc = code + decode16 (vm->pc + 2);

                           // Push a halt continuation on, first:
                           nvm->stack[0] = make_fixnum (vm, 0);
                           nvm->stack[1] = make_fixnum (vm, find_halt ());

                           memcpy (nvm->stack + 2,
                                   vm->stack + vm->sp - n + 1,
                                   n * sizeof nvm->stack[0]);
                           nvm->bp = 2;
                           nvm->sp = 2 + n - 1;
                           run_queue_push_back (nvm);

                           vm->sp -= n;
                           push (vm, entag (a_pid, nvm->pid));
                           NEXT (4); }

      CASE(op_send)        prim2_send (vm);       NEXT (1);
      CASE(op_receive)     if (receive (vm))
                             NEXT (1);
                           else
                             goto yield;

      // XXX use of this opcode had better be serialized!
      CASE(op_defglobal)   if (globals_size <= n_globals)
                             panic ("Too many globals");
                           globals[n_globals++] = pop (vm);
                           NEXT (1);
    }
  }
 yield:
  run_queue_advance ();
}

static void running (void) {
  for (;;)
    run (100);
}

static FILE *open_file (const char *filename, const char *mode) {
  if (0 == strcmp (filename, "-"))
    return 'r' == mode[0] ? stdin : stdout;
  {
    FILE *f = fopen (filename, mode);
    if (!f)
      panic (strerror (errno));
    return f;
  }
}

static void load_stream (FILE *f) {
  unsigned char *p = code;
  while (p < code + code_size) {
    size_t n = fread (p, 1, code + code_size - p, f);
    if (0 == n)
      break;
    p += n;
  }
  if (ferror (f))
    panic (strerror (errno));
  if (!feof (f))
    panic ("Code too big");
}

static void load_file (const char *filename) {
  FILE *f = open_file (filename, "rb");
  load_syms (f);
  load_stream (f);
  fclose (f);
}

int main (int argc, char **argv) {
  if (2 != argc)
    panic ("Usage: elv filename");
  load_file (argv[1]);
  make_startup_vm ();
  running ();
  return 0;
}
