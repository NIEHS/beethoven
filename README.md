# Near Real Time Air Pollution Model 
## Group Project for the Spatiotemporal Exposures and Toxicology group with help from friends :smiley:

## GitHub Push/Pull Workflow
1) Each collaborator has a local copy of the github repo - suggested location is ddn/gs1/username/home
2) Work locally
3) Push to remote
4) Kyle [or delegate] will pull to MAIN local copy on SET group ddn location

## Repo Rules 
1) To PUSH changes to the repo, the changes must be made to a non-MAIN branch
2) Then a PULL request must be made
3) Then it requires the REVIEW of 1 person (can be anyone)
4) Then the change from the branch is MERGED to the MAIN branch
   
## Overall Project Workflow

```mermaid

flowchart LR
    subgraph "Baked-In Reanalysis"
    direction LR
    A[AQS Data] --> B[Generate Covariates]--> C[Fit Base Learners]-->D[Fit Meta Learners]-->E[Summary Stats]-->F[Predictions]
    end
```
##  Unit and Integration Testing 

We will utilize various testing approaches to ensure functionality and quality of code

### Processes to test or check 
1) relative paths
2) variable names
3) variable types
4) variable sizes
5) Output of one module is the expectation of the input of the next module
   
### Unit Test Relevant Class Types and Features

Each of the following are the classes of variables for the I/O that will need to be tested

```mermaid

---
title: Data Classes by Module
---
classDiagram
    class AirPollutionData
    AirPollutionData : +Type1  sf
    AirPollutionData : +Type2 sftime
    AirPollutionData : +Geometry point

    class GeographicCovariates
    GeographicCovariates : +Type1  sf
    GeographicCovariates : +Type2 sftime
    GeographicCovariates : +Geometry point

    class BaseLearners
    BaseLearners : +Type1 S3.model
    BaseLearners : +Type2 S4.model
    BaseLearners : +Geometry1 point
    BaseLearners : +Geometry2 NA

    class MetaLearners
    MetaLearners : +Type1 S3.model
    MetaLearners : +Type2 S4.model
    MetaLearners : +Geometry1 point
    MetaLearners : +Geometry2 NA

    class SummaryStats
    SummaryStats : +Type1 sf
    SummaryStats : +Type2 sftime
    SummaryStats : +Geometry1 point
    SummaryStats : +Geometry2 polygon

```

```mermaid

```


