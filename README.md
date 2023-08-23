# Near Real Time Air Pollution Model 
## Group Project for the Spatiotemporal Exposures and Toxicology group with help from friends :smiley:


## Overall Project Workflow

```mermaid

flowchart LR
    subgraph "Baked-In Reanalysis"
    direction LR
    A[AQS Data] --> B[Generate Covariates]--> C[Fit Base Learners]-->D[Fit Meta Learners]-->E[Summary Stats]-->F[Predictions]
    end
```

## Integrating Unit and Integration Testing 

We will utilize various testing approaches to ensure functionality and quality of code

