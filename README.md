s7 Scheme
=========

s7 is a Scheme implementation intended as an extension language for
other applications.  It is descended from tinyScheme, and tries to be
compatible with the r5rs and r7rs Scheme standards.

s7's real home page is
[ccrma.stanford.edu/software/snd/snd/s7.html](https://ccrma.stanford.edu/software/snd/snd/s7.html).
(See also Carlos Carrasco's "[A love letter to s7
Scheme](http://carloscarrasco.com/a-love-letter-to-s7-scheme.html)".)
The s7 maintainers periodically release new tarballs, with the only 
version number being in the main header file.  The releases archived
here in the [Conservatory](https://conservatory.github.io/) come from
[2016-11-18](https://github.com/Conservatory/s7/commit/20ab60ef10fedbfa14827810cebdf523f5740167)
and
[2016-11-21](https://github.com/Conservatory/s7/commit/52e61e0b7f8132efe5df466901a34584ebbd2ceb).

s7 is [open source software](https://opensource.org/definition), under
a somewhat informally applied BSD-style open source license.  The
source code quotes the
[Snd](https://ccrma.stanford.edu/software/snd/index.html) license,
since s7 is the default extension language for Snd, and s7's
maintainers appear to also be the Snd maintainers.  The [exact license
wording](https://github.com/Conservatory/s7/blob/master/s7.c) in s7
is:

      apparently tinyScheme is under the BSD license, so I guess s7 is too.
      Here is Snd's verbiage which can apply here:
  
      The authors hereby grant permission to use, copy, modify, distribute,
      and license this software and its documentation for any purpose.  
      No written agreement, license, or royalty fee is required.  
      Modifications to this software may be copyrighted by their authors 
      and need not follow the licensing terms described here.
  
      followed by the usual all-caps shouting about liability.

On the other hand, s7 is derived from tinyScheme, and the [tinyScheme
license](http://tinyscheme.sourceforge.net/license.txt) is just a
standard [3-clause BSD
license](https://opensource.org/licenses/BSD-3-Clause):

      Copyright (c) 2000, Dimitrios Souflis
      All rights reserved.
      
      Redistribution and use in source and binary forms, with or
      without modification, are permitted provided that the
      following conditions are met:
      
      Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
      
      Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials
      provided with the distribution.
      
      Neither the name of Dimitrios Souflis nor the names of the
      contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.
      
      THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
      CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
      INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
      MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
      DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
      LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
      EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
      LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
      OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
      CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
      STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
      ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
      ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

...which, as you can see, is not quite the same wording as the Snd
license.

Either way, however, s7 is essentially under a BSD-style open source
license.
