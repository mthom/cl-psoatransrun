# cl-psoatransrun
### Mark Thom <markjordanthom@gmail.com>

A CL implementation of PSOATransRun, realizing the [PSOA
RuleML](http://wiki.ruleml.org/index.php/PSOA_RuleML) data and rule
language.

The software required to build and run cl-psoatransrun are as follows:

* [Quicklisp](https://www.quicklisp.org/beta/)
* A major Common Lisp implementation ([SBCL](http://sbcl.org),
[CCL](https://ccl.clozure.com/), etc.) -- one on which Quicklisp will
run
* One of [XSB Prolog](http://xsb.sourceforge.net) or [Scryer
  Prolog](http://github.com/mthom/scryer-prolog)

Once a CL, Quicklisp and either of the two Prologs have been installed
after following the instructions on their linked sites, these
Quicklisp commands should be executed at the CL REPL to install the
libraries on which cl-psoatransrun depends:

```
* (ql:quickload "drakma")
* (ql:quickload "esrap")
* (ql:quickload "pathname-utils")
* (ql:quickload "rutils")
* (ql:quickload "trivia")
* (ql:quickload "usocket")
* (ql:quickload "uiop")
```

Based on these libraries, cl-psoatransrun can be compiled into the
Lisp image. The global variables at the top of the file
[prolog-engine-client.lisp](http://github.com/mthom/cl-psoatransrun/tree/master/prolog-engine-client.lisp),
`*default-scryer-prolog-path*` and `*default-xsb-prolog-path*` must be
changed to reflect the local paths of the corresponding executables on
your machine. Similarly, a local copy of the [PSOATransRun test
suite](https://github.com/RuleML/PSOATransRunComponents/tree/master/PSOATransRun/test)
should be downloaded, and the global variable `*test-suite-directory*`
of
[psoatransrun-tests.lisp](http://github.com/mthom/cl-psoatransrun/tree/master/psoatransrun-tests.lisp)
must be changed to its path.

Next, the ASDF build file "psoatransrun.asd" must be loaded into CL
as in:

```
* (load "psoatransrun.asd")
* (asdf:load-system "psoatransrun")
```

(Quicklisp will have downloaded and installed the ASDF build system
during its own above install, so there's no need to load ASDF
explicitly via Quickload.)

The cl-psoatransrun REPL is entered with a target KB passed as a string
by entering:

```
* (psoatransrun:psoa-load-and-repl "<target KB>")
```

at the CL REPL. "<target KB>" must be replaced with a well-formed
string representing a PSOA RuleML knowledge base with a single Assert,
as in:

```
* (psoatransrun:psoa-load-and-repl "RuleML(
  Assert(
    _r(_p1->_v)
    _r(_p1->_f(_v))
	_o#_r(_p1->_g(_v) _p2->_f(0))
  )
)")

Translated KB:

:- use_module(standard, [local_datime/1, datime/1]).

:- auto_table.

sloterm('_26999', '_p1', '_v').
sloterm('_27000', '_p1', '_f'('_v')).
sloterm('_o', '_p2', '_f'(0)).
sloterm('_o', '_p1', '_g'('_v')).
memterm('_26999', '_r').
memterm('_27001', '_26999').
memterm('_27000', '_r').
memterm('_27002', '_27000').
memterm('_o', '_r').
>
```

As in the example, the RuleML KB is translated to Prolog, which is printed
to the screen, and a prompt appears where RuleML queries can be entered, as in:

```
> _r(_p1->_f(?x))
?x=_v
```

`psoatransrun:psoa-load-and-repl` also has a keyword argument
`:system` specifying a Prolog engine for use as a backend. Its default
value is `:xsb`, which launches XSB Prolog at the path
`*default-xsb-prolog-path*` as a subprocess of CL; if `:system` is set
to `:scryer`, Scryer Prolog is loaded from the path
`*default-scryer-prolog-path*`.

Support for Scryer Prolog is currently in its infancy, as Scryer is
less mature than XSB and its support for tabling is less robust. For
these reasons, XSB Prolog is the current default backend.

To run the above-referenced test suite, the form
```
* (psoatransrun-tests:run-test-suite)
``` 

is evaluated. `psoatransrun-tests:run-test-suite` also supports the
keyword argument `:system` with default `:xsb`.

cl-psoatransrun has been tested on Linux with both Scryer and XSB as
backends. It's expected to work on Linux, macOS, and other UNIX-like
systems. Scryer does not yet run on Windows, although cl-psoatransrun
has been tested and confirmed to work with XSB versions 3.6 and 3.8 on
Windows.

## Command-line Options from PSOATransRun and their cl-psoatransrun counterparts

Command-line options in the original Java-based PSOATransRun have been
reproduced in cl-psoatransrun as global variables to be set
interactively from within the Lisp environment. They are listed in the
table below.

cl-psoatransrun provides a few additional options not considered
within the purview of the PSOA RuleML language. They are indicated in
the third column.

| cl-psoatransrun Global Variable | PSOATransRun Command Line Option | Within PSOA RuleML      |
| ------------------------------- | -------------------------------- | ----------------------- |
| `*static-objectification-only*` | `-s`,`--staticOnly`              | :heavy_check_mark:      |
| `*all-solutions*`               | `-a`,`--allAns`                  | :heavy_check_mark:      |
| `*print-caret-before-expr*`     | N/A                              |                         |

All three variables are described
[here](https://github.com/mthom/cl-psoatransrun/blob/9e141425b7a25758714164403e072b0df580a3f6/psoatransrun.lisp#L36).


