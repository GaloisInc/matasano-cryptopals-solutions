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
-   [Challenge 22](/sets/3/challenges/22)

</div>

</div>

<div class="row">

<div class="col-md-2">

</div>

<div class="col-md-10">

### Crack an MT19937 seed

Make sure your MT19937 accepts an integer seed value. Test it (verify
that you're getting the same sequence of outputs given a seed).

Write a routine that performs the following operation:

-   Wait a random number of seconds between, I don't know, 40 and 1000.
-   Seeds the RNG with the current Unix timestamp
-   Waits a random number of seconds again.
-   Returns the first 32 bit output of the RNG.

You get the idea. Go get coffee while it runs. Or just simulate the
passage of time, although you're missing some of the fun of this
exercise if you do that.

From the 32 bit RNG output, discover the seed.

</div>

</div>

</div>

<div style="text-align:center">

[Cryptography Services](https://cryptoservices.github.io/) | [NCC
Group](https://www.nccgroup.trust/us/)

</div>
-}

module Set3.C22 () where
