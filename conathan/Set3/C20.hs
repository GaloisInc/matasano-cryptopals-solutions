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
-   [Challenge 20](/sets/3/challenges/20)

</div>

</div>

<div class="row">

<div class="col-md-2">

</div>

<div class="col-md-10">

### Break fixed-nonce CTR statistically

[In this file](/static/challenge-data/20.txt) find a similar set of
Base64'd plaintext. Do with them exactly what you did with the first,
but solve the problem differently.

Instead of making spot guesses at to known plaintext, treat the
collection of ciphertexts the same way you would repeating-key XOR.

Obviously, CTR encryption appears different from repeated-key XOR, *but
with a fixed nonce they are effectively the same thing.*

To exploit this: take your collection of ciphertexts and truncate them
to a common length (the length of the smallest ciphertext will work).

Solve the resulting concatenation of ciphertexts as if for repeating-
key XOR, with a key size of the length of the ciphertext you XOR'd.

</div>

</div>

</div>

<div style="text-align:center">

[Cryptography Services](https://cryptoservices.github.io/) | [NCC
Group](https://www.nccgroup.trust/us/)

</div>
-}

module Set3.C20 () where
