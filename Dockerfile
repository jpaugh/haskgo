FROM haskell:7.10
WORKDIR /opt/server

RUN cabal update
RUN cabal install hscolour

COPY ./*.cabal /opt/server/
RUN cabal install --only-dependencies --enable-documentation -j3

COPY . /opt/server
RUN cabal configure && cabal haddock --hyperlink-source --executables --internal

CMD ["cabal", "repl"]
