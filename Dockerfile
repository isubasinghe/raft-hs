FROM haskell:9.0 AS builder
WORKDIR /haskellapp

COPY src ./src
COPY app ./app
COPY dist-newstyle ./dist-newstyle
COPY test ./test
COPY *.yaml .
COPY *.lock .
COPY *.cabal .
COPY *.hs .
COPY *.md .
RUN ["stack", "setup"]
RUN ["stack", "build", "--local-bin-path", "build", "--copy-bins"]




FROM ubuntu:latest
WORKDIR /root/
COPY --from=builder /haskellapp/build/raft-hs-exe .
RUN ["chmod", "+x", "raft-hs-exe"]
CMD ["./raft-hs-exe"]
