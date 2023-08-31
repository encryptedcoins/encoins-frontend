# GHCJS

System setup where installing ghcjs was succeeded. In another system setup the steps could be different.

        OS: Manjaro 23.0.0 Uranos
        Kernel: x86_64 Linux 6.4.6-1-MANJARO
        DE: KDE 5.108.0 / Plasma 5.27.7

## GHC-8.6 version for encoins project

1. I use `ghcup` for tool management. Thus there should not exist (or be visible in a PATH) any alternative of cabal setup (for example in `/home/$USERNAME/.cabal` ) except ones that installed by  `ghcup`.
Also,no extra `ghc-pkg` setups, for example in `/usr/bin` PATH. Highly likely installed globally `happy` should have version `1.19.11`. Likely, in your case there are another toolings installed with different version that slips into GHCJS compiling.

1. GHCUP: GHC set to 8.6.5, Cabal set to 3.2.0.0
2. git clone --branch ghc-8.6 https://github.com/ghcjs/ghcjs.git
3. cd ghcjs
4. git submodule update --init --recursive
5. Replace `_AC_PROG_CC_C99` with `AC_PROG_CC_C99` in `ghc/aclocal.m4` if you run into error:

        configure:6892: error: possibly undefined macro: _AC_PROG_CC_C99 If this token and others are legitimate, please use m4_pattern_allow

6. ./utils/makePackages.sh
7. cabal new-configure
8. cabal new-build
9. Make `ghcjs-run` visible by adding to `utils/dist-newstyle-wrapper.sh` the lines:

```
elif [ "$PGM" = "ghcjs-run" ]; then
  exec "$DISTDIR/ghcjs-run/ghcjs-run" ${1+"$@"}
```

10.  Make executables being global:

```
ln -sf `pwd`/utils/dist-newstyle-wrapper.sh /home/$USER/.local/bin/ghcjs && \
ln -sf `pwd`/utils/dist-newstyle-wrapper.sh /home/$USER/.local/bin/ghcjs-pkg && \
ln -sf `pwd`/utils/dist-newstyle-wrapper.sh /home/$USER/.local/bin/haddock-ghcjs && \
ln -sf `pwd`/utils/dist-newstyle-wrapper.sh /home/$USER/.local/bin/hsc2hs-ghcjs && \
ln -sf `pwd`/utils/dist-newstyle-wrapper.sh /home/$USER/.local/bin/ghcjs-boot && \
ln -sf `pwd`/utils/dist-newstyle-wrapper.sh /home/$USER/.local/bin/ghcjs-run
```

11.  `ghcjs-boot -s ./lib/boot --no-haddock -j8`
I turned off building haddock documentation with `--no-haddock` because of an error `Haddock's resource directory does not exist!` which could be fixed for sure.
