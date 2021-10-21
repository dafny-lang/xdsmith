FROM ubuntu:20.10

ARG DEBIAN_FRONTEND=noninteractive
ENV PATH="/root/.dotnet:${PATH}"
ENV DOTNET_ROOT="/root/.dotnet"

WORKDIR /workspace

RUN apt update && apt install -y \
    software-properties-common \
    build-essential \
    wget \
    git \
    unzip \
    vim \
    nodejs \
    npm \
    golang \
    tmux \
    htop

# Racket
RUN add-apt-repository -y ppa:plt/racket \
 && apt install -y racket

RUN git clone https://gitlab.flux.utah.edu/xsmith/xsmith.git \
 && cd xsmith/xsmith \
 && raco pkg install -D --auto

# Java
RUN wget -O- https://apt.corretto.aws/corretto.key | apt-key add -

RUN add-apt-repository -y 'deb https://apt.corretto.aws stable main' \
 && apt install -y java-11-amazon-corretto-jdk

# C#
RUN wget https://dot.net/v1/dotnet-install.sh \
 && chmod +x dotnet-install.sh \
 && ./dotnet-install.sh -c Current \
 && rm dotnet-install.sh

RUN npm install bignumber.js

# Usually, running
# RUN raco pkg install -D --auto --name xdsmith ./work-dir
# is a better idea, but we want to utilize
# Docker's caching, so let's install packages manually

RUN raco pkg install -D --auto rosette

# Dafny

RUN echo "2"

RUN git clone https://github.com/dafny-lang/dafny.git --recurse-submodules

COPY apply-patch.rkt /workspace/apply-patch.rkt
RUN racket apply-patch.rkt

RUN cd dafny \
  && make exe \
  && make runtime \
  && make z3-ubuntu

RUN mkdir work-dir
COPY fuzzer.rkt /workspace/work-dir/fuzzer.rkt
COPY basic.rkt /workspace/work-dir/basic.rkt
COPY synth.rkt /workspace/work-dir/synth.rkt
COPY types.rkt /workspace/work-dir/types.rkt
COPY pretty.rkt /workspace/work-dir/pretty.rkt
COPY differ.rkt /workspace/work-dir/differ.rkt
COPY differ-verify.rkt /workspace/work-dir/differ-verify.rkt
COPY oracle.rkt /workspace/work-dir/oracle.rkt
COPY lib.rkt /workspace/work-dir/lib.rkt
COPY info.rkt /workspace/work-dir/info.rkt

RUN rm -rf /var/lib/apt/lists/*

ENTRYPOINT ["racket"]
