# nessy

The nessy framework encapsulates a JavaScript and a PolyDSL syntaxtree (DOM).
It features EDSLs for JavaScript and PolyDSL and is by that capable to produce
DOMs for both languages.

PolyDSL is compiled down to JavaScript via DOM transformations. The language
frontend (e.g. Lexer and Parser) will be implemented in the PolyDSL DOM. The
whole compiler for PolyDSL languages will be implemented in PolyDSL itself.

One goal of the project is to produce a fully working compiler in PolyDSL
without writing one line in a PolyDSL language before bootstrapping the
compiler to it's target platform.

Another goal is to create a platform which is capable of standing against
competitors like .NET and JVM. In the long term there will be native code
generation for *nix-Systems and Windows.

