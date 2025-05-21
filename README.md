# Wierton's OoO Processor 🚀

This document provides instructions for setting up the environment, building, and running tests for Wierton's Out-of-Order (OoO) Processor.

## <ins>Prerequisites</ins>

Ensure you have the following software installed on your system:

  * **Java 8**: Verify your installation using `javac -version`.
    ```bash
    # Example Java environment configuration (optional, if not already set)
    export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
    export PATH=$JAVA_HOME/bin:$PATH
    ```
  * **SBT**: Version 1.3.13 is required (this should be configured in `project/build.properties`).
  * **GNU Make**: A standard build automation tool.
  * **Verilator**: The default uncore simulator.
  * **MIPS Cross-Compiler Toolchain**: Needed for compiling workloads.
    ```bash
    sudo apt update
    sudo apt install gcc-mips-linux-gnu
    ```

-----

## <ins>Building the Processor</ins>

To build the Wierton's OoO Processor, navigate to the root directory of this project and run:

```bash
make
```

-----

## <ins>Running CPU Tests (`cputests`)</ins>

This section outlines the steps to execute the `cputests` suite.

### 1\. Fetch CPU Test Workloads

The test workloads are sourced from the `nexus-am` repository.

```bash
# Navigate to the parent directory of this project
cd ..
# Clone the nexus-am repository (if you haven't already)
git clone git@github.com:nju-mips/nexus-am.git
# Return to this project's directory
cd wierton-ooo-processor # Or your project's directory name
```

*(Assuming `wierton-ooo-processor` is the name of the current project directory. Please adjust if your project directory has a different name.)*

### 2\. Build the Reference Implementation (NEMU)

A golden reference model (NEMU) is used for comparison.

```bash
# Navigate to the parent directory of this project
cd ..

# Clone the NEMU MIPS32 repository (master branch, if you haven't already)
git clone git@github.com:nju-mips/nemu-mips32.git -b master
cd nemu-mips32

# Configure NEMU for woopdiff
make woopdiff_defconfig

# Compile NEMU (use -j followed by the number of CPU cores for faster compilation)
make -j$(nproc) # Or make -j4

# Return to this project's directory
cd ../wierton-ooo-processor # Or your project's directory name
```

### 3\. Execute the Test Suite

Run the `cputests` using the provided Makefile target.

```bash
make run-cputests
```

-----

## <ins>Running Benchmarks (`microbench` / `dhrystone`)</ins>

Follow these steps to run the `microbench` and `dhrystone` benchmarks.

### 1\. Build Reference Implementation with SDL Support (NEMU)

This step is similar to the `cputests` setup but configures NEMU with AM's IOE support (which might require SDL).

```bash
# Navigate to the parent directory of this project
cd ..

# Clone the NEMU MIPS32 repository (master branch, if you haven't already)
# If you cloned it in the previous section, you might only need to reconfigure and rebuild.
# Ensure you are in the nemu-mips32 directory.
# If not already cloned:
git clone git@github.com:nju-mips/nemu-mips32.git -b master
cd nemu-mips32 # If not already in this directory

# Configure NEMU for noop (includes AM's IOE support)
make noop_defconfig

# Compile NEMU
make -j$(nproc) # Or make -j4

# Return to this project's directory
cd ../wierton-ooo-processor # Or your project's directory name
```

### 2\. Execute Microbench

Run the `microbench` suite using its Makefile target.

```bash
make run-microbench
```

### 3\. Execute Dhrystone

Run the `dhrystone` benchmark using its Makefile target.

```bash
make run-dhrystone
```
