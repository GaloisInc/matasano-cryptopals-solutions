{-
<div class="container">

<div class="row">

<div class="col-md-12">

### [the cryptopals crypto challenges](/)

</div>

</div>

<div class="row">

<div class="col-md-12">

-   [Challenges](/)
-   [Set 3](/sets/3)
-   [Challenge 18](/sets/3/challenges/18)

</div>

</div>

<div class="row">

<div class="col-md-2">

</div>

<div class="col-md-10">

### Implement CTR, the stream cipher mode

The string:

    L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ==

... decrypts to something approximating English in CTR mode, which is an
AES block cipher mode that turns AES into a stream cipher, with the
following parameters:

          key=YELLOW SUBMARINE
          nonce=0
          format=64 bit unsigned little endian nonce,
                 64 bit little endian block count (byte count / 16)

CTR mode is very simple.

Instead of encrypting the plaintext, CTR mode encrypts a running
counter, producing a 16 byte block of keystream, which is XOR'd against
the plaintext.

For instance, for the first 16 bytes of a message with these parameters:

    keystream = AES("YELLOW SUBMARINE",
                    "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00")

... for the next 16 bytes:

    keystream = AES("YELLOW SUBMARINE",
                    "\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00")

... and then:

    keystream = AES("YELLOW SUBMARINE",
                    "\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00")

CTR mode does not require padding; when you run out of plaintext, you
just stop XOR'ing keystream and stop generating keystream.

Decryption is identical to encryption. Generate the same keystream, XOR,
and recover the plaintext.

Decrypt the string at the top of this function, then use your CTR
function to encrypt and decrypt other things.

<div class="panel panel-warning">

<div class="panel-heading">

### This is the only block cipher mode that matters in good code. {.panel-title}

</div>

<div class="panel-body">

Most modern cryptography relies on CTR mode to adapt block ciphers into
stream ciphers, because most of what we want to encrypt is better
described as a stream than as a sequence of blocks. Daniel Bernstein
once quipped to Phil Rogaway that good cryptosystems don't need the
"decrypt" transforms. Constructions like CTR are what he was talking
about.

</div>

</div>

</div>

</div>

</div>

<div style="text-align:center">

[Cryptography Services](https://cryptoservices.github.io/) | [NCC
Group](https://www.nccgroup.trust/us/)

</div>
-}

module Set3.C18 () where
