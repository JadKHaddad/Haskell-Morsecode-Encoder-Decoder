# Haskell-Morsecode-Encoder-Decoder

```sh
cabal update
cabal install docopt --lib
cabal install split --lib
cabal install aeson --lib

ghc ./main.hs -o morsecode_encoder_decoder

./morsecode_encoder_decoder
```

```sh
Morsecode Encoder/Decoder 1.0

Usage:
  morsecode_encoder_decoder encode <string>
  morsecode_encoder_decoder decode <string>
  morsecode_encoder_decoder version
```

## Docker

Build
```sh
docker build -t haskell-morse:1.0 .
```
Run
```sh
docker run --rm -it haskell-morse:1.0 [OPTIONS]
```

## TODO

Fix decoder: add remove spaces add /