#Maude version

This directory contains the Maude source code of the soundness checker. 
Several Maude exmaples of feature model evaluation plans are also provided. 


Table of Contents
=================

   * [Maude Version](#Maude-version)
   * [Table of Contents](#table-of-contents)
      * [How to execute the examples](#run)
      * [Overview of the examples](#overview)

## What to ignore in the `featuremodel.maude` file?
1. The atomic entities, for instance, feature_exists, group_has`type_
2. Events, for instance, `_start`, `_stops`
3. Logs, for instance `empty`, `_,_`
4. The log in the `_#_#_` constructor

## How to execute the examples

 
```sh
load example-*.maude 
```
loads the example file and the `featuremodel.maude` file. 

```sh
rew init .
```
This executes the example and shows the validity of the given evolution plan written in the example file.

## Overview of the examples
### example-car.maude
A car example.

### example-ictac.maude
This example is illustrated by Fig.4 in the ictac23 paper.

### example-tcs1.maude
This example shows that even with just a tiny change added to the very end of the plan, 
the execution of soundness checking still needs to run through the entire modified plan.
In this case, two cycle-detections are required.

### example-tcs2.maude
This example contains 4 moveFeature operations.  
The added operations at time 6 will create a cycle at time 7.
