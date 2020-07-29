# runhs

Stack wrapper for single-file Haskell programs.

Declare package dependencies in your single-file Haskell program. Easily and reliably run your program, compile it, or load it in GHCi or Ghcid.


## Install

1. First, install [Stack](https://haskellstack.org).

2. Add Stack's install directory to your `PATH`.

  * **Windows:** Installing Stack automatically adds its install directory to your `PATH`.

  * **Most Linux:** `echo 'export PATH="${HOME}/.local/bin:${PATH}"' >> ~/.profile && export PATH="${HOME}/.local/bin:${PATH}"`

  * **MacOS Pre-Catalina:** `echo 'export PATH="${HOME}/.local/bin:${PATH}"' >> ~/.bash_profile && export PATH="${HOME}/.local/bin:${PATH}"`

  * **MacOS Catalina and newer:** `echo 'export PATH="${HOME}/.local/bin:${PATH}"' >> ~/.zshrc && export PATH="${HOME}/.local/bin:${PATH}"`

3. ~~Finally, install _runhs_: `stack install runhs`.~~ WORK IN PROGRESS!


## Usage

Add a block comment at the start of any single-file Haskell program in the following format:

```
{-
resolver: lts-16.6
packages:
  - bytestring
  - containers
-}
```

The comment delimiters `{-` and `-}` must appear on their own lines, and it must be the first block comment in your file.

The `resolver` property is required and must be a valid [Stackage resolver](https://www.stackage.org/snapshots). The `packages` property is required if your Haskell program depends on external packages, but may be omitted if your Haskell program does not.

Once you have the appropriate head matter at the top of your file, you may load your program in _Watch Mode_ or in _Interactive Mode_ (a.k.a. _REPL Mode_), or run your program as a script, or compile your program, as follows.

```
runhs (watch|repl|script|compile) <file> [<args>]
```


## Copyright and License

Copyright Daniel Brice (c) 2020

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

  * Neither the name of Daniel Brice nor the names of other contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
