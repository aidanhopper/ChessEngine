FROM haskell:latest
WORKDIR /engine
RUN cabal update
RUN cabal install --lib scotty wai-cors aeson
COPY ./*.hs /engine
RUN ghc -package array -package wai -package text Main.hs -o main
EXPOSE 6000
CMD ["./main"]
