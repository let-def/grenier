v0.12 Tue Mar 30 12:03:05 CEST 2021
--------------------------

Balmap: alternative to Stdlib Maps and Sets based on baltree.
Dbseq: fast sequence datastructure for DeBruijn-indexed environments.
State elimination: convert e-NFA to regular expressions.

Fixed many bugs in and increased expressiveness of Valmari implementation.

v0.11 Wed Mar  4 10:07:08 CET 2020
--------------------------

Collect a few type-level idioms in the `Strong` library.
Drop support for OCaml 4.03, OCaml 4.04 is now the oldest supported version.
Fix compilation with the first released version of OCaml 4.10.

v0.10 Mon Jan 13 08:31:36 CET 2020
--------------------------

Fix compilation with OCaml 4.10.
Add a low-level interface to `Dset`.

v0.9 Thu Dec 12 16:18:07 CET 2019
--------------------------

Add a new algorithm `Dset`: construct two set of resources and compute their
difference efficiently.

v0.8 Tue Sep 17 11:59:03 CEST 2019
--------------------------

Important bugfix in the balancing algorithm of Baltree.
Use dune binary instead of jbuilder.
Remove references to Pervasives for OCaml 4.08 compatibility.

v0.7 Sun Jul  8 17:11:24 CEST 2018
--------------------------

Dune & dune-release port contributed by Rudi Grinberg.
Valmari DFA minimization implementation.

v0.6 Tue Oct 10 10:11:08 CEST 2017
--------------------------

Fix support for safe-string / 4.06

v0.5 Thu Jan 12 21:49:31 CET 2017
--------------------------

HyperLogLog: add serialization, improve estimation quality 
