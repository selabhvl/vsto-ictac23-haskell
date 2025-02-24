# Soundness checker

This project provides methods for validating the result of applying an operation to an interval-based feature model. It also makes it possible to convert from a simple feature model evolution plan to an interval-based feature model.

Table of Contents
=================

   * [Soundness checker](#soundness-checker)
   * [Table of Contents](#table-of-contents)
      * [Build and run](#build-and-run)
         * [Prerequisites](#prerequisites)
      * [Overview](#overview)

## Build and run
### Prerequisites

- [GHC](https://www.haskell.org/platform/mac.html)
- [Stack](https://docs.haskellstack.org/en/stable/README/)


```sh
stack build
```
builds the project.

```sh
stack run
```
This outputs an example interval-based feature model.

```sh
stack run tcs-csv output.csv False 3
```
Generates performance data as CSV. True/False controls whether Maude should check each step of the plan. Integer is number of iterations to smooth out execution time,
in case you don't want to use Criterion below:

```sh
stack run tcs-criterion
stack run tcs-criterion -- "FMEP" "Maude wo"
stack run tcs-criterion -- --match glob "SingleOp/*linear*"
```

Generate [`criterion`](https://hackage.haskell.org/package/criterion) performance report (all/filtered by matching benchmarks).
Example: [report.html](report.html)

## Overview
See the [`src/`](https://github.com/idamotz/Master/tree/master/soundness-checker/src) folder for the implementation.  
[`src/Types.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/Types.hs) contains all the types used in the project, and provides insight into the structures of the project. [`src/Validate.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/Validate.hs) contains the function `validate` which returns a list of errors resulting from applying an operation to an interval-based feature model. [`src/Apply.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/Apply.hs) contains the function `apply`, which applies an operation to an interval-based feature model. [`src/Convert.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/Convert.hs) contains the function `convert` which translates a feature model evolution plan into an interval-based feature model. [`src/TreeSequence.hs`](https://github.com/idamotz/Master/blob/master/soundness-checker/src/TreeSequence.hs) contains the function `toTreeSequence` which converts an interval-based feature model into a list of time points associated with feature models. This representation can be used to verify correctness of `validate`, as there exists an agreed-upon semantics for feature models.
