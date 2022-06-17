FROM ubuntu:22.04

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

# Xsmith
RUN git clone https://gitlab.flux.utah.edu/xsmith/xsmith.git \
 && cd xsmith/xsmith \
 && raco pkg install -D --auto

# Java
RUN wget -O- https://apt.corretto.aws/corretto.key | apt-key add -

RUN add-apt-repository -y 'deb https://apt.corretto.aws stable main' \
 && apt install -y java-1.8.0-amazon-corretto-jdk

RUN rm -rf /var/lib/apt/lists/*

# C#
RUN wget https://dot.net/v1/dotnet-install.sh \
 && chmod +x dotnet-install.sh \
 && ./dotnet-install.sh -c Current \
 && rm dotnet-install.sh

# Rosette
RUN raco pkg install -D --auto rosette

RUN mkdir work-dir \
  && cd work-dir \
  && npm install bignumber.js

# Usually, running
# RUN raco pkg install -D --auto --name xdsmith ./xdsmith
# is a better idea, but we want to utilize
# Docker's caching, so let's install packages manually

# Dafny

RUN echo "1"

RUN git clone https://github.com/dafny-lang/dafny.git --recurse-submodules

COPY xdsmith/apply-patch.rkt /workspace/apply-patch.rkt
RUN racket apply-patch.rkt work-dir dafny

RUN cd dafny \
  && make exe \
  && make z3-ubuntu

COPY xdsmith /workspace/xdsmith

RUN find xdsmith -type d -name 'compiled' -prune -exec rm -rf {} \;

WORKDIR /workspace/work-dir

ENTRYPOINT ["racket"]
