FROM haskell:8-slim-buster as build

WORKDIR /UML2KIV

RUN cabal update

# First build dependencies only for better caching
COPY ./UML2KIV.cabal .
RUN cabal build --only-dependencies -j4 -O2

# Now copy sources and build the project
COPY ./src ./src
RUN cabal build -j4 -O2

FROM debian:buster-slim

# Set locale to use UTF-8
RUN apt-get -q update \
    && apt-get install -y locales \
    && localedef -c -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8 \
    && rm -rf /var/lib/apt/lists/*
ENV LANG en_US.utf8

WORKDIR /UML2KIV

COPY --from=build /UML2KIV/dist-newstyle/build/x86_64-linux/ghc-8.10.7/UML2KIV-0.1.0.0/x/UML2KIV/opt/build/UML2KIV/UML2KIV .

WORKDIR /work

ENTRYPOINT [ "/UML2KIV/UML2KIV" ]
