# XDsmith

XDsmith is a random testing framework for the Dafny toolchain. It uses [Xsmith](https://www.flux.utah.edu/project/xsmith) to generate Dafny programs that are suitable for random testing.

## Installation

We recommend that you use Docker to run XDsmith. Therefore, you will need to install Docker, either via the installer from the website or from your OS’s software manager. After that, run `docker build -t test .` from the the project directory. Then, you should be able to run `docker run --entrypoint bash -it test` to start the XDsmith environment.
