# Wierton's OoO Processor

## Requirements

- Java 8 (verify with `javac --version`)
  ```bash
  # Example configuration for Java environment
  export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
  export PATH=$JAVA_HOME/bin:$PATH
  ```
- SBT version 1.3.13 (configured in `project/build.properties`)
- GNU Make
- Verilator (default uncore simulator)
- MIPS cross-compiler toolchain

## Building

To build the project, run: