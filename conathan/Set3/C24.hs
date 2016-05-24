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
-   [Challenge 24](/sets/3/challenges/24)

</div>

</div>

<div class="row">

<div class="col-md-2">

</div>

<div class="col-md-10">

### Create the MT19937 stream cipher and break it

You can create a trivial stream cipher out of any PRNG; use it to
generate a sequence of 8 bit outputs and call those outputs a keystream.
XOR each byte of plaintext with each successive byte of keystream.

Write the function that does this for MT19937 using a 16-bit seed.
Verify that you can encrypt and decrypt properly. This code should look
similar to your CTR code.

Use your function to encrypt a known plaintext (say, 14 consecutive 'A'
characters) prefixed by a random number of random characters.

From the ciphertext, recover the "key" (the 16 bit seed).

Use the same idea to generate a random "password reset token" using
MT19937 seeded from the current time.

Write a function to check if any given password token is actually the
product of an MT19937 PRNG seeded with the current time.

</div>

</div>

</div>

<div style="text-align:center">

[Cryptography Services](https://cryptoservices.github.io/) | [NCC
Group](https://www.nccgroup.trust/us/)

</div>
-}

module Set3.C24 () where
