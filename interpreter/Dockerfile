FROM ocaml/opam:debian-ocaml-5.1 AS builder

RUN sudo apt-get update && \
    sudo apt-get install -y \
    build-essential \
    m4 \
    pkg-config \
    && sudo rm -rf /var/lib/apt/lists/*

RUN opam install -y dune

WORKDIR /home/opam/app
COPY --chown=opam:opam tmpl_dcr.opam .
COPY --chown=opam:opam Makefile .

RUN make setup

COPY --chown=opam:opam . .

RUN opam exec -- make build


FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y \
    libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /home/opam/app/_build/install/default/bin/tmpl_dcr .

ENTRYPOINT [ "./tmpl_dcr" ]