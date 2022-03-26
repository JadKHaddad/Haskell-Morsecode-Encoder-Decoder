# build: docker build -t haskell-morse:1.0 .
# run: docker run --rm -it haskell-morse:1.0 [OPTIONS]

FROM haskell:8.10.7-slim-buster as builder

COPY . /home/app/

WORKDIR /home/app/

RUN cabal update
RUN cabal install docopt --lib
RUN cabal install split --lib
RUN cabal install aeson --lib

RUN ghc -O2 ./main.hs -o morsecode_encoder_decoder

FROM debian:buster-slim as runner

COPY . /home/app/
COPY --from=builder /home/app/morsecode_encoder_decoder /home/app/morsecode_encoder_decoder


ENTRYPOINT [ "/home/app/morsecode_encoder_decoder" ]