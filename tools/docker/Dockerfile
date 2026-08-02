FROM ubuntu:24.04

WORKDIR /velox
COPY . /velox

RUN apt-get -qq update \
    && DEBIAN_FRONTEND=noninteractive apt-get -qq install --no-install-recommends \
        bison \
        clang \
        flex \
        make \
        python3 \
        racket \
    && rm -rf /var/lib/apt/lists/*

RUN make && ./compile.sh

CMD ["bin/vm"]
