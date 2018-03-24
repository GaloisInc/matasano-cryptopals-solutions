# demo to convince us that my CTR mode cipher works

build & run & examine output files:
```
stack build && stack exec bmp-ctr-mode-fun && open ciphertext.bmp plaintext.bmp
```

`plaintext.bmp` should be a greyscale image with gradually darkening shading top to bottom.

`ciphertext.bmp` should look fairly random.
