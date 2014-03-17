readSpec
========

`readSpec`: making PBT easier to read for humans.

### dependencies

In order to use `readSpec` you need a [Quviq QuickCheck](http://www.quviq.com) licence.

### compiling & running

Just clone the repository and execute:

    make && ./run

You should then see an erlang shell in which you can try the examples provided
in the `priv` folder.

* Simple property example: `simple_eqc.erl`.

   This module contains a property to test the `delete` operation of the `lists`
   module. By running:

   ````
    cd(priv).
    readspec:suite(simple_eqc, prop_simple).
   ````

   you will get a `?MODULE.feature` file with some samples of test cases that
   QuickCheck would run when testing the property, written in human-readable,
   semi-natural, [Cucumber](http://cukes.info/)-style language.

   Now, the `prop_simple` property is faulty, as you can see if you run a
   sufficient number of test cases:


    eqc:quickcheck(eqc:numtests(1000, simple_eqc:prop_simple())).


   The counterexamples that QuickCheck returns can also be saved to a
   `?PROPERTY.counterexample.feature` in the same human-readable,
   semi-natural, Cucumber-like format by running:


    readspec:counterexample(simple_eqc, prop_simple, eqc:current_counterexample()).


Enjoy! And do not forget that comments & bug reports, as well as contributions,
are welcome!

### troubleshooting

You can enable some debugging messages by commenting out the following line in
the `include/readspec.hrl` file:

    -define(DEBUG(IOString, Args), ok).

and uncommenting the following one:

    -define(DEBUG(IOString, Args), io:format(IOString, Args)).

You can slightly alter the output format (i.e. line length) by modifying the
`PRETTYPR_OPTIONS` macro in that same file.

    -define(PRETTYPR_OPTIONS, [{encoding, utf8}, {paper, 120}, {ribbon, 120}]).

