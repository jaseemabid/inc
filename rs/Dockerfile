FROM buildpack-deps:sid

ENV DEBIAN_FRONTEND=noninteractive \
    RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH

RUN apt update && \
    apt install -y gdb gdbserver && \
    rm -rf /var/lib/apt/lists/*

RUN curl -s https://sh.rustup.rs | sh -s -- -y \
      --no-modify-path \
      --profile minimal \
      --default-toolchain nightly && \
    chmod -R a+w $RUSTUP_HOME $CARGO_HOME && \
    rustup --version && \
    cargo --version && \
    rustc --version;

WORKDIR /inc
ADD . /inc

RUN cargo build --tests --lib --bins
