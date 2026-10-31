# Bundled TypeScript standard library declarations

`lib.*.d.ts` from the npm package `typescript@5.9.3` (directory `lib/`), Apache License 2.0 (`LICENSE.txt`),
Copyright (c) Microsoft Corporation.

They are the roots of the predefined Web SDK ("JavaScript Standard Library", web-browser-impl) and of the Node.js
SDKs (`consulo.javascript.library.JavaScriptStandardLibrary`, implemented by
`consulo.typescript.library.TypeScriptStandardLibraryImpl`). A module sees the libraries of its language level and of
the environment libraries of its JavaScript extensions (the DOM of Web) - `consulo.javascript.impl.library.JavaScriptLibraries`.

Only the files reachable through `/// <reference lib="..." />` from `esnext`, `dom`, `dom.iterable`,
`dom.asynciterable`, `scripthost` and `webworker.importscripts` are bundled (no `webworker`, `tsc` or locales).

TypeScript 7 (the native port) no longer ships these files in its npm package: refresh them from the last 5.x release
or from the `src/lib` generated output of the TypeScript repository. After a refresh, run
`TypeScriptBundledLibParsingTest`: a declaration after a parse error can be lost for the stub index.
