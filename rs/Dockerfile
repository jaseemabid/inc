FROM buildpack-deps:sid

ENV RUSTUP_HOME=/usr/local/rustup \
  CARGO_HOME=/usr/local/cargo \
  PATH=/usr/local/cargo/bin:$PATH

RUN curl -s https://sh.rustup.rs | sh -s -- -y --no-modify-path --default-toolchain nightly \
  && chmod -R a+w $RUSTUP_HOME $CARGO_HOME \
  && rustup --version \
  && cargo --version \
  && rustc --version;

# http://whitfin.io/speeding-up-rust-docker-builds
RUN USER=root cargo new --bin /dep
WORKDIR /dep
COPY ./Cargo.lock ./Cargo.lock
COPY ./Cargo.toml ./Cargo.toml

RUN cargo build --tests
RUN rm src/*.rs

RUN mkdir /inc
WORKDIR /inc
ADD . /inc
RUN cargo build --tests

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt update && \
    apt install -y gdb gdbserver && \
    rm -rf /var/lib/apt/lists/*
