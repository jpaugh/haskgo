FROM haskell:7.10
WORKDIR /opt/server

RUN cabal update

COPY ./*.cabal /opt/server/
RUN cabal install --only-dependencies -j3

COPY . /opt/server
CMD ["cabal", "repl"]
