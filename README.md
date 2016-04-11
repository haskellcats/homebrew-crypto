![alt text][logo]

# Homebrew Crypto

"The simplest possible thing that will work."

"... with some observations about basic deficiencies."

Slideshow: https://docs.google.com/presentation/d/1mFALpF2JP59rJggwswXPDEi9mBF6YoNfCfAflsEoL9w/edit?usp=sharing

## Stream Cipher

This uses SHA512 on a counter that increases by 1 when more key data is
required. 

## RSA

Very basic RSA, no padding. Also includes signing and signature verification.
Includes two trivial ways to crack a key pair given the public key.

## Diffie Hellman Merkle

Very basic DH exchange.

## Math

Powers mod n, euclidian algorithm, modular inverse, and Miller Rabin prime
test all copy pasted. Uses the arithmoi Haskell library for factorization and
totient, which is not used in any actual crypto. Special thanks to lpsmith
in #haskell-blah for the chinese remainder theorem code.

## Driver program

```
./Homebrew -g 2048  # generate huge prime and save to disk for diffie hellman
./Homebrew -p 12345 # wait for connection
./Homebrew localhost 12345 # initiate connection
```

The driver program establishes a two way encrypted channel by doing a DH
handshake to agree on a large integer. This integer is loaded into the simple
stream cipher (one for each direction) which will encrypt or decrypt the
communications.

Disclaimer: Even if you know what you're doing, it's probably a liability to
use this code for anything serious! Demonstration-grade only.

## Eavesdropper program

```
./MITM localhost 12345 -p 54321
```

Uses the async library to seamlessly mediate alice and bob's conversation
until their handshake can be reversed and their communications decrypted,
all without them knowing.

[logo]: https://raw.githubusercontent.com/haskellcats/homebrew-crypto/master/cryptosystem.png "Cryptosystem Diagram"
