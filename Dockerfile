FROM alpine@sha256:c19173c5ada610a5989151111163d28a67368362762534d8a8121ce95cf2bd5a AS base

RUN apk update && \
    apk add bash perl alpine-sdk wget curl libc-dev xz


################################################################################
# Intermediate layer that assembles 'stack' tooling
FROM base AS stack

ENV STACK_VERSION=1.9.3
ENV STACK_SHA256="c9bf6d371b51de74f4bfd5b50965966ac57f75b0544aebb59ade22195d0b7543  stack-${STACK_VERSION}-linux-x86_64-static.tar.gz"

RUN echo "Downloading stack" &&\
    cd /tmp &&\
    wget -P /tmp/ "https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64-static.tar.gz" &&\
    if ! echo -n "${STACK_SHA256}" | sha256sum -c -; then \
        echo "stack-${STACK_VERSION} checksum failed" >&2 &&\
        exit 1 ;\
    fi ;\
    tar -xvzf /tmp/stack-${STACK_VERSION}-linux-x86_64-static.tar.gz &&\
    cp -L /tmp/stack-${STACK_VERSION}-linux-x86_64-static/stack /usr/bin/stack &&\
    rm /tmp/stack-${STACK_VERSION}-linux-x86_64-static.tar.gz &&\
    rm -rf /tmp/stack-${STACK_VERSION}-linux-x86_64-static
################################################################################


FROM stack as stack_intermediate

ENV GHC_VERSION=8.4.4
ENV GHC_INSTALL_PATH=/opt/ghc

RUN wget https://github.com/redneb/ghc-alt-libc/releases/download/ghc-${GHC_VERSION}-musl/ghc-${GHC_VERSION}-x86_64-unknown-linux-musl.tar.xz && \
    tar xf ghc-${GHC_VERSION}-x86_64-unknown-linux-musl.tar.xz

RUN apk update && \
    apk add bash perl alpine-sdk wget make curl git libc-dev xz coreutils zlib-dev shadow gmp-dev


WORKDIR ghc-${GHC_VERSION}

RUN ./configure --prefix=${GHC_INSTALL_PATH} && \
        make install

ENV PATH=${GHC_INSTALL_PATH}/bin:$PATH

COPY --from=stack /usr/bin/stack /usr/bin/stack
RUN stack config set system-ghc --global true
RUN mkdir /build
COPY . /build
RUN cd /build && make build && make install

FROM tezos/tezos:mainnet
COPY --from=stack_intermediate /root/.local/bin/backerei /home/tezos
WORKDIR /home/tezos
ENTRYPOINT ["./backerei"]
