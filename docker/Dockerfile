# FROM haskell:8.6.3 as builder

# RUN stack install --only-dependencies --resolver="lts-13.5" base64-bytestring
# RUN stack install --only-dependencies --resolver="lts-13.5" binary
# RUN stack install --only-dependencies --resolver="lts-13.5" binary-orphans
# RUN stack install --only-dependencies --resolver="lts-13.5" bytestring
# RUN stack install --only-dependencies --resolver="lts-13.5" containers
# RUN stack install --only-dependencies --resolver="lts-13.5" cookie
# RUN stack install --only-dependencies --resolver="lts-13.5" data-default
# RUN stack install --only-dependencies --resolver="lts-13.5" digestive-functors
# RUN stack install --only-dependencies --resolver="lts-13.5" entropy
# RUN stack install --only-dependencies --resolver="lts-13.5" formatting
# RUN stack install --only-dependencies --resolver="lts-13.5" http-types
# RUN stack install --only-dependencies --resolver="lts-13.5" mtl
# RUN stack install --only-dependencies --resolver="lts-13.5" mustache
# RUN stack install --only-dependencies --resolver="lts-13.5" pandoc

# RUN apt-get update
# RUN apt-get install -y libpq-dev

# RUN stack install --only-dependencies --resolver="lts-13.5" postgresql-orm
# RUN stack install --only-dependencies --resolver="lts-13.5" postgresql-simple
# RUN stack install --only-dependencies --resolver="lts-13.5" pretty-simple
# RUN stack install --only-dependencies --resolver="lts-13.5" scotty
# RUN stack install --only-dependencies --resolver="lts-13.5" scotty-cookie
# RUN stack install --only-dependencies --resolver="lts-13.5" stm
# RUN stack install --only-dependencies --resolver="lts-13.5" string-conversions
# RUN stack install --only-dependencies --resolver="lts-13.5" text
# RUN stack install --only-dependencies --resolver="lts-13.5" time
# RUN stack install --only-dependencies --resolver="lts-13.5" transformers
# RUN stack install --only-dependencies --resolver="lts-13.5" wai
# RUN stack install --only-dependencies --resolver="lts-13.5" wai-extra
# RUN stack install --only-dependencies --resolver="lts-13.5" wai-middleware-static

# COPY . .

# RUN stack build --force-dirty --copy-bins && rm -rf .stack-work

# ENTRYPOINT /root/.local/bin/app

###########################################################

# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
FROM fpco/stack-build:lts-14.27 as dependencies
# RUN mkdir /opt/build
# WORKDIR /opt/build

RUN stack install --only-dependencies --resolver="lts-14.27" Cabal Glob HTTP HUnit HsYAML JuicyPixels MissingH Only QuickCheck RSA SHA StateVar adjunctions aeson aeson-pretty ansi-terminal ansi-wl-pprint appar array asn1-encoding asn1-parse asn1-types async attoparsec authenticate-oauth auto-update base base-compat base-orphans base16-bytestring base64-bytestring basement bifunctors binary binary-orphans bitarray blaze-builder blaze-html blaze-markup bsb-http-chunked byteorder bytestring bytestring-builder cabal-doctest call-stack case-insensitive cereal clock cmark-gfm cmdargs colour comonad connection containers contravariant cookie crypto-api crypto-pubkey-types cryptonite data-default data-default-class data-default-instances-containers data-default-instances-dlist data-default-instances-old-locale deepseq digest directory distributive dlist doctemplates easy-file entropy exceptions expiring-cache-map extra fail fast-logger filepath formatting free ghc-boot-th ghc-prim haddock-library happy hashable haskell-src-exts hourglass hslogger hslua hslua-module-system hslua-module-text http-client http-client-tls http-date http-test http-types http2 hxt hxt-charproperties hxt-regex-xmlschema hxt-unicode integer-gmp integer-logarithms invariant iproute ipynb kan-extensions lens lens-aeson lucid

RUN stack install --only-dependencies --resolver="lts-14.27" memory mime-types mmorph monad-control mtl nats network network-byte-order network-uri old-locale old-time optparse-applicative pandoc pandoc-types parallel parsec pem postgresql-libpq postgresql-orm postgresql-simple pretty pretty-simple primitive process profunctors psqueues pureMD5 random reflection regex-base regex-compat regex-pcre-builtin regex-posix resourcet rts safe scientific scotty scotty-cookie semigroupoids semigroups simple-sendfile skylighting skylighting-core socks split splitmix stm streaming-commons string-conversions syb tagged tagsoup tasty tasty-hunit template-haskell temporary texmath text th-abstraction time time-compat time-locale-compat time-manager tls transformers transformers-base transformers-compat typed-process unbounded-delays unicode-transforms unix unix-compat unix-time unliftio-core unordered-containers utf8-string uuid-types vault vector void wai wai-extra wai-logger wai-middleware-static warp wcwidth word8 wreq x509 x509-store x509-system x509-validation xhtml xml zip-archive zlib












# GHC dynamically links its compilation targets to lib gmp
# RUN apt-get update \
#   && apt-get download libgmp10
# RUN mv libgmp*.deb libgmp.deb

# # Docker build should not use cached layer if any of these is modified
# COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
# RUN stack build --system-ghc --dependencies-only

# -------------------------------------------------------------------------------------------
FROM fpco/stack-build:lts-14.27 as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack

WORKDIR /opt/build

RUN apt update
RUN apt install -y ssh-client

RUN pwd
COPY . /opt/build
#RUN git clone https://github.com/chrissound/BerylliumBlueberry.git . # hi1ia23zzzzzzzzzzzz
RUN rm stack.yaml.lock # https://github.com/commercialhaskell/stack/issues/5045

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# -------------------------------------------------------------------------------------------
# # Base image for stack build so compiled artifact from previous
# # stage should run
FROM ubuntu:18.04 as app
RUN mkdir -p /opt/app

# # Install lib gmp
# COPY --from=dependencies /opt/build/libgmp.deb /tmp
# RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

RUN apt update
RUN apt install -y postgresql-client

COPY --from=build /opt/build/bin /opt/app/
COPY ./static/main-img.jpg /data/static/
COPY ./static/blog.css /data/static/
RUN ls -altr /data/static/

WORKDIR /data

CMD ["/opt/app/blog3000-app"]
