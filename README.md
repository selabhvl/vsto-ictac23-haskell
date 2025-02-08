# "Modular Soundness Checking for Feature Model Evolution Plans"

Authors:
Crystal Chang Din, Charaf Eddine Dridi, Ida Sandberg Motzfeldt, Violet Ka I Pun, Volker Stolz, Ingrid Chieh Yu

This is the artefact for the revised version of our [ICTAC'23 paper](https://doi.org/10.1007/978-3-031-47963-2_25):

Ida Sandberg Motzfeldt, Ingrid Chieh Yu, Crystal Chang Din, Violet Ka I Pun, Volker Stolz:
"Modular Soundness Checking of Feature Model Evolution Plans". ICTAC 2023: 417-437

Based on/forked from [the master thesis by Ida Sandberg Motzfeldt](https://github.com/idamotz/Master/).

## Abstract

Feature model evolution plans (FMEPs) describe how feature models for software product lines (SPLs) evolve over time. While different feature models can exist for different points in time over the lifetime of the product line, an FMEP describes how to compute a feature model for a given time point. SPLs capitalize on the variability and reusability of the software through combining optional and mandatory features. As business requirements change over time, FMEPs should support intermediate update. A plan hence contains updates to an initial model by adding, deleting, moving or changing elements at different points in time, in line with the evolving business requirements on the SPL, potentially affecting feature models that should be derived in the future from the plan.

A recurring challenge in maintaining FMEPs is that updates may lead to inconsistent intermediate feature models, most notably so-called paradoxes. A paradox may not materialise at the first point in time an update on the plan is performed to obtain a particular feature model, but may only in combination with a later modification prescribed by the plan create a structurally invalid model. Correspondingly, a single modification to a plan may require multiple checks over the liftetime of the affected elements to rule out paradoxes.

Current approaches require the analysis from the point in time an update is applied to an FMEP throughout the entire lifetime of the plan. Based on our initial work [ICTAC'23], we define a so-called interval-based feature model (IBFM) to represent FMEPs, with a precise definition of spatial and temporal scopes that narrow the time interval and the sub-models that an update can affect. We propose a rule system for updating IBFMs, and also prove the soundness of the proposed rules and show their modularity, i.e., that each rule operates strictly

