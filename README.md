# elv
A very basic Lisp + Erlang-style processes, on a virtual machine

I wrote this many years ago, descended from
https://github.com/darius/ichbins, but judged it a dead end. It's
archived here for your entertainment.

Undocumented, but these lines should build and run it:

    $ make
    $ (cd tests; ./testallmeta)

When developing this I didn't depend on the ability to bootstrap;
instead there was a version of the compiler that ichbins could compile
(`older/elvc.scm`) along with a parallel version (`selvc.scm`) that
could take advantage of the extra features of itself and the
VM. ichbins.scm itself and its bootstrapping process were also
included in this repo. So were the corresponding variants of the test
scripts. I've cut all that out to reduce clutter (except for keeping
an archive copy of `elvc.scm` in `older/` so that the cut stuff can be
reconstructed without undue effort). If you want to actively develop
this system, as a weird challenge or learning experience or something,
then it might be easiest to bring back the non-bootstrapped version of
the compiler, just so then you can use gdb on its C-compiled output --
as the VM has no debugger. OTOH you might rather make debugging nicer
on the VM! Whatevs.
