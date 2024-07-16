FROM ubuntu:16.04

RUN rm /bin/sh && ln -s /bin/bash /bin/sh

## ensure locale is set during build
ENV LANG=C.UTF-8

## Haskell environment
RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main' > \
      /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
      zlib1g-dev \
      libtinfo-dev \
      libsqlite3-0 \
      libsqlite3-dev \
      ca-certificates \
      build-essential \
      libgmp-dev \
      autoconf \
      automake \
      curl \
      g++ \
      python3 \
      git

ENV PATH=/root/.cabal/bin:/root/.local/bin:/opt/cabal/bin:/opt/ghc/8.6.5/bin:$PATH

## node.js
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash - \
    && apt-get install -y nodejs

# Install micro editor 
RUN curl https://getmic.ro | bash
RUN mv micro /usr/local/bin

# Install ghc-8.6.5 and cabal-2.4.1.0. This cabal version wanted for build ghcjs.
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
  BOOTSTRAP_HASKELL_GHC_VERSION=8.6.5 \
  sh
ENV PATH=/root/.ghcup/bin:$PATH

RUN ghcup install cabal 2.4.1.0 && ghcup set cabal 2.4.1.0

## build GHCJS
WORKDIR /opt

RUN cabal update

RUN cabal install alex-3.2.6
RUN cabal install happy-1.19.9

RUN git clone https://github.com/ghcjs/ghcjs.git

WORKDIR /opt/ghcjs

RUN git checkout 04c61d21e13fcbd5de8ca03bea5bc81a862d83d3

RUN git submodule update --init --recursive 

RUN ./utils/makePackages.sh  
    
RUN ./utils/makeSandbox.sh 

RUN cabal install

ENV PATH=/opt/ghcjs/.cabal-sandbox/bin:$PATH

RUN ghcjs-boot -v2 -s ./lib/boot/

# Prepare to work with a project

RUN ghcup install cabal 3.2.0.0 && ghcup set cabal 3.2.0.0

WORKDIR /home/frontend

ENV CABAL_DIR=/home/.frontend_cabal_cache

COPY ./start.sh /home/start.sh

ENTRYPOINT ["/bin/sh", "/home/start.sh"]