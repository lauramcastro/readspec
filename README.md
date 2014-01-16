readspec
========

ReadSpec application: making PBT easier to read for humans

compiling & running
-------------------

Just clone the repository and execute

    make && ./run

You should then see an erlang shell in which you can try the following provided examples:

    cd(priv).
    c(prop_simple).
    readspec:suite(simple_eqc, fun simple_eqc:prop_simple/0, 20).

or

    cd(priv).
    c(prop_register).
    readspec:suite(register_eqc, fun register_eqc:prop_register/0, 35).

This will create (each time) a suite.cucumberl (overwritten) file with a human-readable version of a test case example generated by QC from the corresponding test property/model.

troubleshooting
---------------

If you encounter any errors while executing the examples above, make sure you load the readspec module before changing into the priv directory:

    l(readspec).
