# Bookie, a build agent for ATS

* Bookie will initialize a project with a set of common defaults used with `ats-postiats`. 
* Bookie will build your ats project (instead of a make file)

Bookie uses haskell's `shake` under the hood to build and initialize the project.



## Usage

### To make a project

There are two ways to make a project with bookie.  
* `bookie init`
  * After target prompt:
  * Create a folder named `target`.
  * Create a `bookie.yaml` file there with common defaults
  * Create a `src` directory

* `bookie initwithquery`
  * Asks for much more detail about the yaml that will be generated.


### To build a project  

* `bookie build` Builds your project in the build.yaml directory


### To add files

* `bookie add` Add `hats` `dats` `sats` or `c` files to your project.


## Installation

Installation in a few steps

1. Get stack
2. clone this repo `git clone https://github.com/plow-technologies/bookie.git`
3. `stack install` 


This should install bookie for you.  
To test that it builds things, try going to the testProject and typing `bookie build`.

## `bookie.yaml` all fields
This yaml file tries to capture many of the fields you normally find in a Makefile.

``` yaml
ats-project-version: '0.0.1'      # The version of the project
ats-home: /usr/local              # The install directory of your ats build
ats-source-dir: src               # The source folder of your project
ats-source-files:                 # The list of files to build
- test.dats                     
ats-flags:                        # Flags to pass to the compiler
- -O2
- -DATS_MEMALLOC_LIBC
ats-build-dir: ats-work           # The working directory... where your files end up
ats-target: testProject           # The target executable to create
ats-opt: /usr/local/bin/patsopt   # The name of the atsopt binary
ats-cc: /usr/local/bin/patscc     # The name of the atscc binary
```
