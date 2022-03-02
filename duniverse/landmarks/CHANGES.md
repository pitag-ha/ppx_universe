version 1.4, 15 aug 2021
------------------------
* switch to github actions
* add a link to https://github.com/LexiFi/landmarks-starter
* split landmarks into landmarks and landmarks-ppx
* enabled instrumentation (PR #23, @nojb)
* use floatarray #24 (@nojb)
* use ppxlib instead of OMP (PR #22)
* improvements on the doc and examples (PR #15, #16, @ostera)
* primitive support for objects (allow to annotate methods)
* dynamic landmarks
* auto now automatically benchmark all methods

version 1.3, 6 dec 2018
-----------------------
* migrate from jbuilder to dune
* migrate to opam 2.0
* adds two new primitives: push/pop_profiling_state
* prepare migration for Pervasives deprecation
* redefine 'Stdlib.raise' in the 'Landmark' to allow using landmark with -no-stdlib (as 'raise' is used to wrap exception in the generated code).

version 1.2, 20 may 2018
------------------------
* improved documentation (issue #8, PR #11, @maroneze)
* migrate the build to dune/jbuilder
* support for OCAML 4.02 (issue #6)
* landmarks-viewer is now a separate git repository
* fix issue #13 (@pirbo & @nilsbecker)

version 1.1, 9 Jan 2017
-----------------------
* fix 'debug' option (issue #7)
* ability to specify a directory for outputing in temporary files (issue #4)
* fix problem with labelled argument (issue #3)
* prepend a digest to top-levels identifiers generated by the ppx rewriter to avoid name clashes (issue #2)
* module qualification of landmark names (issue #1)

version 1.0, 8 Jul 2016
-----------------------
* first release