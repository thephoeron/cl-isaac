cl-isaac
========

Doug Hoyte's Optimized Common Lisp version of Bob Jenkins' ISAAC-32 algorithm, a fast cryptographic random number generator, ready for ASDF and Quicklisp.
 
UPDATE 02/09/2013
-----------------

Minor modifications to make functional under SBCL. Original at: http://hcsw.org/downloads/isaac.lisp

-- "the Phoeron" Colin J.E. Lupton

From Doug Hoyte's original source
=================================

(C) May 2008 Doug Hoyte, HCSW
BSD license: you can do anything you want with it (but no warranty).

Optimised Common Lisp implementation of Bob Jenkins' ISAAC-32 algorithm:
Indirection, Shift, Accumulate, Add, and Count. More details and
the C reference implementations can be found here:

ISAAC: a fast cryptographic random number generator
http://burtleburtle.net/bob/rand/isaacafa.html

This lisp implementation is roughly as fast as Jenkins' optimised rand.c
when compiled with a good native-code lisp compiler. It also performs
well when byte-code compiled.

USAGE
-----

First, create an isaac context. There are three functions that do this:

    isaac:init-kernel-seed => <isaac context>
      *RECOMMENDED* Seeds with values from /dev/arandom on BSD
      or /dev/urandom on Linux. Reads 1024 bytes from the device.

    isaac:init-common-lisp-random-seed => <isaac context>
      Seeds with values from your Common Lisp implementation's
      random function. Consumes 256 32-bit values from #'random.

    isaac:init-null-seed => <isaac context>
      Seeds with all 0s. Always results in the same stream.
      For comparing with Jenkins' reference implementations.

These are functions you can pass an isaac context to. They will modify the isaac context and return a random value:

    isaac:rand32 <isaac context> => <random 32-bit value>
      Uses the ISAAC-32 algorithm to generate a new random value.

    isaac:rand-bits <isaac context> <N> => <random N-bit value>
      Uses the ISAAC-32 algorithm to generate random values between
      0 and (1- (expt 2 N)). This function always consumes one or more
      ISAAC-32 words. Note that the N parameter is different from
      the CL random function parameter.   Examples:
        (isaac:rand-bits ctx 1) => [0,1] (consumes 1 ISAAC-32 word)
        (isaac:rand-bits ctx 2) => [0,1,2,3] (ditto)
        (isaac:rand-bits ctx 3) => [0,1,2,3,4,5,6,7] (ditto)
        (isaac:rand-bits ctx 32) => [0,1,...,(1- (expt 2 32))] (ditto)
        (isaac:rand-bits ctx 33) => [0,1,...,(1- (expt 2 33))] (consumes 2 words)
        (isaac:rand-bits ctx 512) => [0,1,...,(1- (expt 2 512))] (consumes 16 words)

QUICK RECIPE
------------

Generate a 128-bit session ID as a 0-padded hexadecimal string:

    (ql:quickload "cl-isaac")
    (defvar my-isaac-ctx (isaac:init-kernel-seed))
    (format nil "~32,'0x" (isaac:rand-bits my-isaac-ctx 128))
      => "078585213B0EF01B1B9BECB291EF38F0"

FAQ
---

Q) My Common Lisp implementation already uses the Mersenne Twister, what are the advantages of ISAAC?

A1) The Mersenne Twister is not a cryptographic PRNG. This means that it is possible for someone to predict future values based on previously observed values (just over 600 of them). As such, MT is particularly undesirable for things like web session IDs. You can still use MT for crypto, but you must use a cryptographic hash function on the MT output.

A2) cl-isaac appears to be roughly as fast as the Mersenne Twister #'random of CMUCL 19d on x86 before even considering the above-mentioned hash function overhead requirement of MT.

A3) cl-isaac is not implemented as an x86 VOP like CMUCL's Mersenne Twister, but instead in 100% standard ANSI Common Lisp (except for the kernel seed interface). This should mean comparable performance on all architectures targeted by your lisp compiler. The non-x86 MT implementation is apparently an order-of-magnitude slower.

Q) How "random" can I expect these values to be?

A) Very. From Bob Jenkins' website: "Cycles are guaranteed to be at least (expt 2 40) values long, and they are (expt 2 8295) values long on average. The results are uniformly distributed, unbiased, and unpredictable unless you know the seed. [...] Why not use RC4? RC4 is three times slower, more biased, has a shorter minimum and average cycle length, and is proprietary. No way is known to break either RC4 or ISAAC; both are immune to Gaussian elimination."

Note that there is a $1000 prize you can win from Jenkins if you find a flaw in ISAAC (but all flaws in isaac.lisp are of course mine).
