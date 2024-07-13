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


## GHCJS 8.10.7

1. Set environment with GHCUP: GHC set to 8.10.7, Cabal set to 3.2.0.0
2. Prepare right version of ghcjs

```shell
git clone https://github.com/obsidiansystems/ghcjs.git
cd ghcjs 
git checkout 44d9653517887b393c5d6b510f674c97a75b2958
git submodule update --init --recursive
```

3. Make ghc

```shell
(cd ghc && ./boot && ./configure && make)
```

3. Fix optimizer, otherwise the js size left in 80 times more then it was in ghcjs-8.6.5. 
In `src/Gen2/Optimizer.hs` do

```haskell        
-- optimize = id
optimize = renameLocalVars . removeDeadVars . dataflow
```

4. Prepare ghcjs repository 

```shell
./utils/updatePatches.sh
./utils/makePackages.sh
```

5. Install ghcjs and co. 

```shsell 
cabal v2-configure
cabal v2-build
cabal v2-install --overwrite-policy=always --install-method=copy --installdir=inplace/bin
```

6. Boot ghcjs libraries 

First install [emscripten](https://emscripten.org/docs/getting_started/index.html) to somewhere with version [3.1.14](https://github.com/ghcjs/ghcjs/issues/837)

``` shell
source ~/emsdk/emsdk_env.sh
./inplace/bin/ghcjs-boot -s ./lib/boot
```

7. GHCJS binaries to your executable PATH. 
E.g. copy built executes from `inplace/bin` to `~/.local/bin/` or include it to `PATH` another way.