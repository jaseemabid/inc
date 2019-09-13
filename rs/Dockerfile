FROM buildpack-deps:sid

ENV DEBIAN_FRONTEND=noninteractive \
    RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH

RUN apt update && \
    apt install -y gdb gdbserver && \
    rm -rf /var/lib/apt/lists/*

RUN curl -s https://sh.rustup.rs | sh -s -- -y --no-modify-path --default-toolchain nightly && \
    chmod -R a+w $RUSTUP_HOME $CARGO_HOME && \
    rustup --version && \
    cargo --version && \
    rustc --version;

# http://whitfin.io/speeding-up-rust-docker-builds
RUN USER=root cargo new --bin /inc
WORKDIR /inc
COPY ./Cargo.lock ./Cargo.lock
COPY ./Cargo.toml ./Cargo.toml

RUN cargo build

RUN rm src/*.rs
ADD . /inc

RUN cargo build
