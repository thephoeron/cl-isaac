# CL-ISAAC

[![Quicklisp](https://quickdocs.org/badge/cl-isaac.svg)](https://quickdocs.org/cl-isaac/)
[![BSD Simplified License](https://img.shields.io/badge/license-BSD%20Simplified-blue.svg)](./LICENSE)

Optimized Common Lisp library of Bob Jenkins' ISAAC-32 and ISAAC-64 algorithms, fast cryptographic random number generators: Indirection, Shift, Accumulate, Add, and Count. Available in Quicklisp.

The C reference implementations are available at:

http://burtleburtle.net/bob/rand/isaacafa.html

This Common Lisp version is roughly as fast as Jenkins' optimised *rand.c* when compiled with SBCL.

## CHANGELOG

All notable changes to this project are documented in [CHANGELOG.md](./CHANGELOG.md)

## ISSUES

If you find any bugs, please report them at: https://github.com/thephoeron/cl-isaac/issues

## USAGE

Make sure you have the latest Quicklisp distribution. Include CL-ISAAC as a dependency in your system definition, or evaluate `(ql:quickload "cl-isaac")` from the REPL.

First, you need to create an isaac context.

**WARNING - Thread Safety**: If your application is multi-threaded and you are passing in a seed variable, you must add a lock to the seed variable. With Bordeaux Threads it can be done with `bt2:with-lock`

If you have a 64-bit implementation of Lisp on an x86-64 platform, you can set `:is64 t` to take advantage of the ISAAC-64 algorithm. There are four functions that do this:

**isaac:init-self-seed** &key (count 1) (is64 nil) => *`<isaac context>`*

RECOMMENDED: First round seeds with random bits generated by `init-kernel-seed`, each additional round below count scrambles and modular adds the context values to more random bits from a new isaac context.  A `count` > 3 is recommended for optimal speed vs. cryptographic strength.

**isaac:init-kernel-seed** &key (is64 nil) => *`<isaac context>`*

Seeds with values from `/dev/arandom` on BSD or `/dev/urandom` on Linux. Reads 1024 bytes from the device.

**isaac:init-common-lisp-random-seed** &key (is64 nil) => *`<isaac context>`*

Seeds with values from your Common Lisp implementation's random function. Consumes 256 32-bit values from #'random.

**isaac:init-null-seed** &key (is64 nil) => *`<isaac context>`*

Seeds with all 0s. Always results in the same stream. For comparing with Jenkins' reference implementations.

These are functions you can pass an isaac context to. They will modify the isaac context and return a random value:

**isaac:rand32** `<isaac context>` => *`<random 32-bit value>`*

Uses the ISAAC-32 algorithm to generate a new random value.

**isaac:rand-bits** `<isaac context>` `<N>` => *`<random N-bit value>`*

Uses the ISAAC-32 algorithm to generate random values between 0 and (1- (expt 2 N)). This function always consumes one or more ISAAC-32 words. Note that the N parameter is different from the CL random function parameter.

Examples:

```lisp
(isaac:rand-bits ctx 1) => [0,1]                         ; (consumes 1 ISAAC-32 word)
(isaac:rand-bits ctx 2) => [0,1,2,3]                     ; (ditto)
(isaac:rand-bits ctx 3) => [0,1,2,3,4,5,6,7]             ; (ditto)
(isaac:rand-bits ctx 32) => [0,1,...,(1- (expt 2 32))]   ; (ditto)
(isaac:rand-bits ctx 33) => [0,1,...,(1- (expt 2 33))]   ; (consumes 2 words)
(isaac:rand-bits ctx 512) => [0,1,...,(1- (expt 2 512))] ; (consumes 16 words)
```

Conversely, the ISAAC-64 algorithm uses 64-bit word sizes, and scales accordingly.

## QUICK RECIPE

Generate a random 128-bit session ID as a 0-padded hexadecimal string:

```lisp

* (ql:quickload "cl-isaac")
* (defparameter *isaac-ctx* (isaac:init-kernel-seed))
* (format nil "~32,'0x" (isaac:rand-bits *isaac-ctx* 128))
    => "078585213B0EF01B1B9BECB291EF38F0"
```

Generate a random 512-bit token using the ISAAC-64 algorithm:

```lisp

* (defparameter *isaac64-ctx* (isaac:init-kernel-seed :is64 t))
* (format nil "~64,'0x" (isaac:rand-bits-64 *isaac64-ctx* 512))
    => "6F00D098A342450CD7A2C27D941625ED70E7F7F4DD0BD46D8D1597361F0AA49180728D9BA062A14E6795F579D5B04B01F92310F18921A7397C57CF09012E104F"
```

## FAQ

**Q)** My Common Lisp implementation already uses the Mersenne Twister, what are the advantages of ISAAC?

**A1)** The Mersenne Twister is not a cryptographic PRNG. This means that it is possible for someone to predict future values based on previously observed values (just over 600 of them). As such, MT is particularly undesirable for things like web session IDs. You can still use MT for crypto, but you must use a cryptographic hash function on the MT output.

**A2)** cl-isaac appears to be roughly as fast as the Mersenne Twister #'random of CMUCL 19d on x86 before even considering the above-mentioned hash function overhead requirement of MT.

**A3)** cl-isaac is not implemented as an x86 VOP like CMUCL's Mersenne Twister, but instead in 100% standard ANSI Common Lisp (except for the kernel seed interface). This should mean comparable performance on all architectures targeted by your lisp compiler. The non-x86 MT implementation is apparently an order-of-magnitude slower.

**Q)** How "random" can I expect these values to be?

**A)** Very. From Bob Jenkins' website: "Cycles are guaranteed to be at least (expt 2 40) values long, and they are (expt 2 8295) values long on average. The results are uniformly distributed, unbiased, and unpredictable unless you know the seed. [...] Why not use RC4? RC4 is three times slower, more biased, has a shorter minimum and average cycle length, and is proprietary. No way is known to break either RC4 or ISAAC; both are immune to Gaussian elimination."

Note that there is a $1000 prize you can win from Jenkins if you find a flaw in ISAAC (but all flaws in CL-ISAAC are of course mine).

## PLATFORM SUPPORT

This library supports and is tested on the following platforms and Lisp implementations.

### macOS

- [LispWorks][] 8.0.1 (64-bit)
- [Allegro CL][] 10.1
- [SBCL][] 2.3.4
- [ABCL][] 1.9.2
- [ECL][] 21.2.1
- [CLISP][] 2.49.92
- [CLASP CL][] 2.3.0-38-gd5637675b
- [Clozure CL][] 1.12.1

### Windows

- [SBCL][] 2.3.2

### Linux

- [SBCL][] 2.3.4

## SUPPORTING THIS PROJECT

You can support this project by [sponsoring the contributors](https://github.com/thephoeron/cl-isaac/graphs/contributors) through GitHub Sponsors.

## CONTRIBUTING

If you find any bugs or would like to see CL-ISAAC work on your platform, please create an issue on [the master GitHub repository](https://github.com/thephoeron/cl-isaac).

To contribute to CL-ISAAC, please fork and create a pull request.

## LICENSE

This library is released under a BSD-like license.  Please see LICENSE file for more information.

[Parachute]: https://shinmera.github.io/parachute
[GitHubCopilotX]: https://github.com/features/preview/copilot-x
[@thephoeron]: https://github.com/thephoeron
[AliveExtension]: https://marketplace.visualstudio.com/items?itemName=rheller.alive
[BAPHOMET]: https://github.com/thephoeron/baphomet
[LispWorks]: https://www.lispworks.com/
[SBCL]: https://www.sbcl.org/
[Allegro CL]: https://franz.com/products/allegrocl/
[Clozure CL]: https://ccl.clozure.com/
[ABCL]: https://abcl.org/
[ECL]: https://common-lisp.net/project/ecl/
[CLISP]: https://clisp.sourceforge.io/
[CLASP CL]: https://github.com/clasp-developers/clasp