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

# ENV PATH=/root/.cabal/bin:/root/.local/bin:/opt/cabal/bin:/opt/ghc/8.6.5/bin:$PATH

## node.js
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash - \
    && apt-get install -y nodejs

# Install micro editor 
RUN curl https://getmic.ro | bash
RUN mv micro /usr/local/bin

# Set host $USER as default in docker container
ARG uid
ARG username

RUN echo "uid: $uid"
RUN echo "username: $username"

ENV USER=${username}

ENV UID=${uid}

RUN echo "UID: $UID"
RUN echo "USER: $USER"

RUN adduser --uid $UID $USER
USER $USER

ENV HOME=/home/$USER
WORKDIR $HOME

# Install ghc-8.6.5 and cabal-2.4.1.0. This cabal version wanted for build ghcjs.
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
  BOOTSTRAP_HASKELL_GHC_VERSION=8.6.5 \
  sh
ENV PATH=$HOME/.ghcup/bin:$HOME/.cabal:$HOME/.cabal/bin:$PATH

RUN ghcup install cabal 2.4.1.0 && ghcup set cabal 2.4.1.0

## build GHCJS

RUN cabal update

RUN cabal install alex-3.2.6
RUN cabal install happy-1.19.9

RUN git clone https://github.com/ghcjs/ghcjs.git

WORKDIR $HOME/ghcjs

RUN git checkout 04c61d21e13fcbd5de8ca03bea5bc81a862d83d3

RUN git submodule update --init --recursive 

RUN ./utils/makePackages.sh  
    
RUN ./utils/makeSandbox.sh 

RUN cabal install

ENV PATH=$HOME/ghcjs/.cabal-sandbox/bin:$PATH

RUN ghcjs-boot -v2 -s ./lib/boot/

# Prepare to work with a project

RUN ghcup install cabal 3.2.0.0 && ghcup set cabal 3.2.0.0

WORKDIR $HOME

ARG docker_cabal_cache
ARG docker_front_path

ENV DOCKER_CABAL_CACHE=${docker_cabal_cache}
ENV DOCKER_FRONT_PATH=${docker_front_path}

RUN mkdir $DOCKER_CABAL_CACHE

RUN mkdir $DOCKER_FRONT_PATH

ENV CABAL_DIR=$DOCKER_CABAL_CACHE

ADD ./docker_entrypoint.sh ./docker_entrypoint.sh

ENTRYPOINT ["./docker_entrypoint.sh"]