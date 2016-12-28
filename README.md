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
