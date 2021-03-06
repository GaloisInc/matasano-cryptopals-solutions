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
-   [Challenge 23](/sets/3/challenges/23)

</div>

</div>

<div class="row">

<div class="col-md-2">

</div>

<div class="col-md-10">

### Clone an MT19937 RNG from its output

The internal state of MT19937 consists of 624 32 bit integers.

For each batch of 624 outputs, MT permutes that internal state. By
permuting state regularly, MT19937 achieves a period of 2\*\*19937,
which is Big.

Each time MT19937 is tapped, an element of its internal state is
subjected to a tempering function that diffuses bits through the result.

The tempering function is invertible; you can write an "untemper"
function that takes an MT19937 output and transforms it back into the
corresponding element of the MT19937 state array.

To invert the temper transform, apply the inverse of each of the
operations in the temper transform in reverse order. There are two kinds
of operations in the temper transform each applied twice; one is an XOR
against a right-shifted value, and the other is an XOR against a
left-shifted value AND'd with a magic number. So you'll need code to
invert the "right" and the "left" operation.

Once you have "untemper" working, create a new MT19937 generator, tap it
for 624 outputs, untemper each of them to recreate the state of the
generator, and splice that state into a new instance of the MT19937
generator.

The new "spliced" generator should predict the values of the original.

<div class="panel panel-warning">

<div class="panel-heading">

### Stop and think for a second. {.panel-title}

</div>

<div class="panel-body">

How would you modify MT19937 to make this attack hard? What would happen
if you subjected each tempered output to a cryptographic hash?

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

module Set3.C23 () where
