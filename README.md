[![test-coverage](https://github.com/Spatiotemporal-Exposures-and-Toxicology/NRTAPmodel/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/NRTAPmodel/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/Spatiotemporal-Exposures-and-Toxicology/NRTAPmodel/graph/badge.svg?token=T6QZW69X55)](https://codecov.io/gh/Spatiotemporal-Exposures-and-Toxicology/NRTAPmodel)
[![R-CMD-check](https://github.com/Spatiotemporal-Exposures-and-Toxicology/NRTAPmodel/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/NRTAPmodel/actions/workflows/check-standard.yaml)
[![lint](https://github.com/Spatiotemporal-Exposures-and-Toxicology/NRTAPmodel/actions/workflows/lint.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/NRTAPmodel/actions/workflows/lint.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

# Air Pollution Data for the Masses: An Open-Access, Test-Driven, and Reproducible Pipeline  PM<sub>2.5</sub> Hybrid Model 
 Group Project for the Spatiotemporal Exposures and Toxicology group with help from friends :smiley: :cowboy_hat_face: :earth_americas:

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

Targets: Make-like Reproducible Analysis Pipeline
 1) AQS Data
 2) Generate Covariates
 3) Fit Base Learners
 4) Fit Meta Learners
 5) Predictions
 6) Summary Stats
    
##  Unit and Integration Testing 

We will utilize various testing approaches to ensure functionality and quality of code

### Processes to test or check 
1) data type
2) data name
3) data size
4) relative paths
5) output of one module is the expectation of the input of the next module

### Test Drive Development
Starting from the end product, we work backwards while articulating the tests needed at each stage.

#### Key Points of Unit and Integration Testing
File Type
1. NetCDF
2. Numeric, double precision
3. NA
4. Variable Names Exist
5. Naming Convention

Stats 
1. Non-negative variance ($\sigma^2$)
2. Mean is reasonable ($\mu$)
3. SI Units

Domain 
1. In the US (+ buffer)
2. In Time range (2018-2022)

Geographic 
1. Projections
2. Coordinate names (e.g. lat/lon)
3. Time in acceptable format 


### Frameworks for Testing of this project (with help from ChatGPT)

#### Test Driven Development (TDD)- Key Steps
1. **Write a Test**: Before you start writing any code, you write a test case for the functionality you want to implement. This test should fail initially because you haven't written the code to make it pass yet. The test defines the expected behavior of your code.

2. **Run the Test**: Run the test to ensure it fails. This step confirms that your test is correctly assessing the functionality you want to implement.

3. **Write the Minimum Code**: Write the minimum amount of code required to make the test pass. Don't worry about writing perfect or complete code at this stage; the goal is just to make the test pass.

4. **Run the Test Again**: After writing the code, run the test again. If it passes, it means your code now meets the specified requirements.

5. **Refactor (if necessary)**: If your code is working and the test passes, you can refactor your code to improve its quality, readability, or performance. The key here is that you should have test coverage to ensure you don't introduce new bugs while refactoring.

6. **Repeat**: Continue this cycle of writing a test, making it fail, writing the code to make it pass, and refactoring as needed. Each cycle should be very short and focused on a small piece of functionality.

7. **Complete the Feature**: Keep repeating the process until your code meets all the requirements for the feature you're working on.

TDD helps ensure that your code is reliable and that it remains functional as you make changes and updates. It also encourages a clear understanding of the requirements and promotes better code design.




## Base Learners 
Potential base learners we can use: 
1) PrestoGP (lasso + GP)
2) XGBOOST
3) RF
4) CNN
5) UMAP covariates
6) Encoder NN covariates


## Pipeline
- Disclaimer: this flowchart is created by `targets::tar_mermaid()` and is in progress.

```mermaid
graph LR
  style Legend fill:#FFFFFF00,stroke:#000000;
  style Graph fill:#FFFFFF00,stroke:#000000;
  subgraph Legend
    direction LR
    x0a52b03877696646([""Outdated""]):::outdated --- xbf4603d6c2c2ad6b([""Stem""]):::none
    xbf4603d6c2c2ad6b([""Stem""]):::none --- xf0bce276fe2b9d3e>""Function""]:::none
  end
  subgraph Graph
    direction LR
    x0c3b521dbc4a88da>"mr"]:::outdated --> x27cbfd6373fe535c>"get_aqs_data"]:::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x898f6ef09eba37cc>"join_yx"]:::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x4f7bafe8b9d8e6ad>"combine"]:::outdated
    xe8c8a998b5b20a7b>"export_res"]:::outdated --> x77eb19f175e33a01(["meta_exported"]):::outdated
    x266bf942e3db9701(["meta_fit"]):::outdated --> x77eb19f175e33a01(["meta_exported"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x7a43086901f0d2f8(["covariates_predict_modis_mod09"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x7a43086901f0d2f8(["covariates_predict_modis_mod09"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x7a43086901f0d2f8(["covariates_predict_modis_mod09"]):::outdated
    x421b919e1bf83506(["status_modis_mod09"]):::outdated --> x7a43086901f0d2f8(["covariates_predict_modis_mod09"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x8320e923873cce59(["covariates_nei"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x8320e923873cce59(["covariates_nei"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> x8320e923873cce59(["covariates_nei"]):::outdated
    xb8edf20a967a6a4b(["status_nei"]):::outdated --> x8320e923873cce59(["covariates_nei"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> xbf17a0cf1ae5f999(["covariates_modis_mod09"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xbf17a0cf1ae5f999(["covariates_modis_mod09"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> xbf17a0cf1ae5f999(["covariates_modis_mod09"]):::outdated
    x421b919e1bf83506(["status_modis_mod09"]):::outdated --> xbf17a0cf1ae5f999(["covariates_modis_mod09"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> xbe2572147348199b(["status_modis_vnp46"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xbe2572147348199b(["status_modis_vnp46"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x9be289792a2fbebc(["sites_spat"]):::outdated
    x7a022cd753c91551>"read_locs"]:::outdated --> x9be289792a2fbebc(["sites_spat"]):::outdated
    x22fcb267911855d6>"combine_final"]:::outdated --> xd5ade8190f4bed8b(["covariates_final"]):::outdated
    x23a325691b5510e3(["covariates_combined_sp"]):::outdated --> xd5ade8190f4bed8b(["covariates_final"]):::outdated
    x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated --> xd5ade8190f4bed8b(["covariates_final"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xd5ade8190f4bed8b(["covariates_final"]):::outdated
    x82ff1e205817dacb(["sites_time"]):::outdated --> xd5ade8190f4bed8b(["covariates_final"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x4aeebcaf210c4d17(["covariates_narrmono"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x4aeebcaf210c4d17(["covariates_narrmono"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> x4aeebcaf210c4d17(["covariates_narrmono"]):::outdated
    x1a3c0d7579f0fbf1(["status_narrmono"]):::outdated --> x4aeebcaf210c4d17(["covariates_narrmono"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xb8dff327793c8660(["covar_prediction_grid"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x82ff1e205817dacb(["sites_time"]):::outdated
    x7a022cd753c91551>"read_locs"]:::outdated --> x82ff1e205817dacb(["sites_time"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> xcca304422a78068a(["covariates_modis_mod11"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xcca304422a78068a(["covariates_modis_mod11"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> xcca304422a78068a(["covariates_modis_mod11"]):::outdated
    xb6e57d3d3ffff164(["status_modis_mod11"]):::outdated --> xcca304422a78068a(["covariates_modis_mod11"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x9eb5b2f9a5e616d3(["covariates_modis_mcd19"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x9eb5b2f9a5e616d3(["covariates_modis_mcd19"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> x9eb5b2f9a5e616d3(["covariates_modis_mcd19"]):::outdated
    x8bf4ce576c24f57b>"calculate_single"]:::outdated --> xeb27ff83e4a31702(["covariates_ecoregion"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xeb27ff83e4a31702(["covariates_ecoregion"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> xeb27ff83e4a31702(["covariates_ecoregion"]):::outdated
    x9c92e3a678d4dd36(["status_ecoregion"]):::outdated --> xeb27ff83e4a31702(["covariates_ecoregion"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> xd7521266010dc049(["covariates_predict_narrplevels"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> xd7521266010dc049(["covariates_predict_narrplevels"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xd7521266010dc049(["covariates_predict_narrplevels"]):::outdated
    x256412792a163b91(["status_narrplevels"]):::outdated --> xd7521266010dc049(["covariates_predict_narrplevels"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> xaabe745cf0e8d0aa(["covariates_predict_geos"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> xaabe745cf0e8d0aa(["covariates_predict_geos"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xaabe745cf0e8d0aa(["covariates_predict_geos"]):::outdated
    x0f891876c073bd4f(["status_geos"]):::outdated --> xaabe745cf0e8d0aa(["covariates_predict_geos"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> xe51b995b72cf7b75(["covariates_narrplevels"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xe51b995b72cf7b75(["covariates_narrplevels"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> xe51b995b72cf7b75(["covariates_narrplevels"]):::outdated
    x256412792a163b91(["status_narrplevels"]):::outdated --> xe51b995b72cf7b75(["covariates_narrplevels"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> xa910eb0e4ac3b503(["covariates_modis_mod13"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xa910eb0e4ac3b503(["covariates_modis_mod13"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> xa910eb0e4ac3b503(["covariates_modis_mod13"]):::outdated
    x928b772015a85715(["status_modis_mod13"]):::outdated --> xa910eb0e4ac3b503(["covariates_modis_mod13"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> x7ca9d1f32d84ce07(["status_nlcd"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x7ca9d1f32d84ce07(["status_nlcd"]):::outdated
    xe3bb960370d9434e(["data_full"]):::outdated --> xddfacdb844e4f645(["base_prep_xgboost"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xddfacdb844e4f645(["base_prep_xgboost"]):::outdated
    xd477411e8a4437d3(["base_fit_cnn"]):::outdated --> x266bf942e3db9701(["meta_fit"]):::outdated
    x747842e5c63795de(["base_fit_rf"]):::outdated --> x266bf942e3db9701(["meta_fit"]):::outdated
    x01598b8cc1972570(["base_fit_xgboost"]):::outdated --> x266bf942e3db9701(["meta_fit"]):::outdated
    xe3bb960370d9434e(["data_full"]):::outdated --> x266bf942e3db9701(["meta_fit"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x266bf942e3db9701(["meta_fit"]):::outdated
    x34dfd1e6316d5965(["grid_filled"]):::outdated --> xe5589da4c5d2805a(["summary_state"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x519cb6eadc9738b7(["covariates_modis_vnp46"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x519cb6eadc9738b7(["covariates_modis_vnp46"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> x519cb6eadc9738b7(["covariates_modis_vnp46"]):::outdated
    xbe2572147348199b(["status_modis_vnp46"]):::outdated --> x519cb6eadc9738b7(["covariates_modis_vnp46"]):::outdated
    x22fcb267911855d6>"combine_final"]:::outdated --> xc6c80e911c6f83cd(["covariates_predict_final"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> xc6c80e911c6f83cd(["covariates_predict_final"]):::outdated
    x1a390f5b99d335ba(["covariates_predict_combined_sp"]):::outdated --> xc6c80e911c6f83cd(["covariates_predict_final"]):::outdated
    xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated --> xc6c80e911c6f83cd(["covariates_predict_final"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xc6c80e911c6f83cd(["covariates_predict_final"]):::outdated
    x4f7bafe8b9d8e6ad>"combine"]:::outdated --> x1a390f5b99d335ba(["covariates_predict_combined_sp"]):::outdated
    x5ba756b7977f5fbd(["covariates_predict_ecoregion"]):::outdated --> x1a390f5b99d335ba(["covariates_predict_combined_sp"]):::outdated
    xddc8d4acb6955b97(["covariates_predict_koppen"]):::outdated --> x1a390f5b99d335ba(["covariates_predict_combined_sp"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x1a390f5b99d335ba(["covariates_predict_combined_sp"]):::outdated
    x4fe4f34ba5b5dc0d>"configure_cv"]:::outdated --> x85150cce73975dce(["cv_config"]):::outdated
    xd5ade8190f4bed8b(["covariates_final"]):::outdated --> x85150cce73975dce(["cv_config"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x1e9dfd4c30bff545(["covariates_predict_sedac_groads"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x1e9dfd4c30bff545(["covariates_predict_sedac_groads"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x1e9dfd4c30bff545(["covariates_predict_sedac_groads"]):::outdated
    x0b5c3c8f9e90952c(["status_sedac_groads"]):::outdated --> x1e9dfd4c30bff545(["covariates_predict_sedac_groads"]):::outdated
    xd5ade8190f4bed8b(["covariates_final"]):::outdated --> xe3bb960370d9434e(["data_full"]):::outdated
    x898f6ef09eba37cc>"join_yx"]:::outdated --> xe3bb960370d9434e(["data_full"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xe3bb960370d9434e(["data_full"]):::outdated
    x25c4f7a3c9b1e459(["sites_pm"]):::outdated --> xe3bb960370d9434e(["data_full"]):::outdated
    x4f7bafe8b9d8e6ad>"combine"]:::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    xaabe745cf0e8d0aa(["covariates_predict_geos"]):::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    xd0d490a9f060f983(["covariates_predict_gmted"]):::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    x2ff114299dc8e1f2(["covariates_predict_hms"]):::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    x0eaa3e69ddba2620(["covariates_predict_modis_mcd19"]):::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    x42fe841665001c0a(["covariates_predict_modis_mod06"]):::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    x7a43086901f0d2f8(["covariates_predict_modis_mod09"]):::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    x1447ab5d4f154b26(["covariates_predict_modis_mod11"]):::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    x9feedc99f60fb8e2(["covariates_predict_modis_mod13"]):::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    x7acc397cee3e14fe(["covariates_predict_modis_vnp46"]):::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    x2d0b0f3a52aa94f9(["covariates_predict_nei"]):::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    x4f37a13ace9387f3(["covariates_predict_nlcd"]):::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    x5b6b66da6038376a(["covariates_predict_tri"]):::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xd8415744ab00df0d(["covariates_predict_combined_spt"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x0eaa3e69ddba2620(["covariates_predict_modis_mcd19"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x0eaa3e69ddba2620(["covariates_predict_modis_mcd19"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x0eaa3e69ddba2620(["covariates_predict_modis_mcd19"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> x9c92e3a678d4dd36(["status_ecoregion"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x9c92e3a678d4dd36(["status_ecoregion"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> xbdc7c17f3b8f8d4d(["covariates_predict_narrmono"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> xbdc7c17f3b8f8d4d(["covariates_predict_narrmono"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xbdc7c17f3b8f8d4d(["covariates_predict_narrmono"]):::outdated
    x1a3c0d7579f0fbf1(["status_narrmono"]):::outdated --> xbdc7c17f3b8f8d4d(["covariates_predict_narrmono"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> x1a3c0d7579f0fbf1(["status_narrmono"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x1a3c0d7579f0fbf1(["status_narrmono"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> x97ccb184baa85125(["status_gmted"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x97ccb184baa85125(["status_gmted"]):::outdated
    x8bf4ce576c24f57b>"calculate_single"]:::outdated --> x5bfd045458ac6b54(["covariates_koppen"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x5bfd045458ac6b54(["covariates_koppen"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> x5bfd045458ac6b54(["covariates_koppen"]):::outdated
    x82cb1ad0d4385e86(["status_koppen"]):::outdated --> x5bfd045458ac6b54(["covariates_koppen"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> x82cb1ad0d4385e86(["status_koppen"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x82cb1ad0d4385e86(["status_koppen"]):::outdated
    x4f7bafe8b9d8e6ad>"combine"]:::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    x39a05072aea743f2(["covariates_geos"]):::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    x25add895167aa681(["covariates_gmted"]):::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    xaf8a0760bd264f54(["covariates_hms"]):::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    x9eb5b2f9a5e616d3(["covariates_modis_mcd19"]):::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    xac4ef65e42f64136(["covariates_modis_mod06"]):::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    xbf17a0cf1ae5f999(["covariates_modis_mod09"]):::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    xcca304422a78068a(["covariates_modis_mod11"]):::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    xa910eb0e4ac3b503(["covariates_modis_mod13"]):::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    x519cb6eadc9738b7(["covariates_modis_vnp46"]):::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    x8320e923873cce59(["covariates_nei"]):::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    x522c9b3c9932fa50(["covariates_nlcd"]):::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    x3cc7cedf973cab92(["covariates_tri"]):::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x74a6100978aa4f0a(["covariates_combined_spt"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x7acc397cee3e14fe(["covariates_predict_modis_vnp46"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x7acc397cee3e14fe(["covariates_predict_modis_vnp46"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x7acc397cee3e14fe(["covariates_predict_modis_vnp46"]):::outdated
    xbe2572147348199b(["status_modis_vnp46"]):::outdated --> x7acc397cee3e14fe(["covariates_predict_modis_vnp46"]):::outdated
    xddfacdb844e4f645(["base_prep_xgboost"]):::outdated --> x01598b8cc1972570(["base_fit_xgboost"]):::outdated
    x85150cce73975dce(["cv_config"]):::outdated --> x01598b8cc1972570(["base_fit_xgboost"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x968fcd6bb91bfee5(["covariates_sedac_population"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x968fcd6bb91bfee5(["covariates_sedac_population"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> x968fcd6bb91bfee5(["covariates_sedac_population"]):::outdated
    xd17aa3e428dcb2fd(["status_sedac_population"]):::outdated --> x968fcd6bb91bfee5(["covariates_sedac_population"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> x256412792a163b91(["status_narrplevels"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x256412792a163b91(["status_narrplevels"]):::outdated
    x27cbfd6373fe535c>"get_aqs_data"]:::outdated --> x25c4f7a3c9b1e459(["sites_pm"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> xd17aa3e428dcb2fd(["status_sedac_population"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xd17aa3e428dcb2fd(["status_sedac_population"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> x0b5c3c8f9e90952c(["status_sedac_groads"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x0b5c3c8f9e90952c(["status_sedac_groads"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x3cc7cedf973cab92(["covariates_tri"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x3cc7cedf973cab92(["covariates_tri"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> x3cc7cedf973cab92(["covariates_tri"]):::outdated
    xe98cd32cc9814568(["status_tri"]):::outdated --> x3cc7cedf973cab92(["covariates_tri"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x2a142c78e9a889c8(["covariates_predict_sedac_population"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x2a142c78e9a889c8(["covariates_predict_sedac_population"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x2a142c78e9a889c8(["covariates_predict_sedac_population"]):::outdated
    xd17aa3e428dcb2fd(["status_sedac_population"]):::outdated --> x2a142c78e9a889c8(["covariates_predict_sedac_population"]):::outdated
    x72e3a67227f9c3bd(["base_prep_rf"]):::outdated --> x747842e5c63795de(["base_fit_rf"]):::outdated
    x85150cce73975dce(["cv_config"]):::outdated --> x747842e5c63795de(["base_fit_rf"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x2d0b0f3a52aa94f9(["covariates_predict_nei"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x2d0b0f3a52aa94f9(["covariates_predict_nei"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x2d0b0f3a52aa94f9(["covariates_predict_nei"]):::outdated
    xb8edf20a967a6a4b(["status_nei"]):::outdated --> x2d0b0f3a52aa94f9(["covariates_predict_nei"]):::outdated
    xe3bb960370d9434e(["data_full"]):::outdated --> xe79f77cc2db38f4d(["base_prep_cnn"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xe79f77cc2db38f4d(["base_prep_cnn"]):::outdated
    xe79f77cc2db38f4d(["base_prep_cnn"]):::outdated --> xd477411e8a4437d3(["base_fit_cnn"]):::outdated
    x85150cce73975dce(["cv_config"]):::outdated --> xd477411e8a4437d3(["base_fit_cnn"]):::outdated
    x4f7bafe8b9d8e6ad>"combine"]:::outdated --> x23a325691b5510e3(["covariates_combined_sp"]):::outdated
    xeb27ff83e4a31702(["covariates_ecoregion"]):::outdated --> x23a325691b5510e3(["covariates_combined_sp"]):::outdated
    x5bfd045458ac6b54(["covariates_koppen"]):::outdated --> x23a325691b5510e3(["covariates_combined_sp"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x23a325691b5510e3(["covariates_combined_sp"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x522c9b3c9932fa50(["covariates_nlcd"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x522c9b3c9932fa50(["covariates_nlcd"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> x522c9b3c9932fa50(["covariates_nlcd"]):::outdated
    x7ca9d1f32d84ce07(["status_nlcd"]):::outdated --> x522c9b3c9932fa50(["covariates_nlcd"]):::outdated
    x8bf4ce576c24f57b>"calculate_single"]:::outdated --> x5ba756b7977f5fbd(["covariates_predict_ecoregion"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x5ba756b7977f5fbd(["covariates_predict_ecoregion"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x5ba756b7977f5fbd(["covariates_predict_ecoregion"]):::outdated
    x9c92e3a678d4dd36(["status_ecoregion"]):::outdated --> x5ba756b7977f5fbd(["covariates_predict_ecoregion"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> x6493583ad3488b14(["status_hms"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x6493583ad3488b14(["status_hms"]):::outdated
    xc6c80e911c6f83cd(["covariates_predict_final"]):::outdated --> x34dfd1e6316d5965(["grid_filled"]):::outdated
    x266bf942e3db9701(["meta_fit"]):::outdated --> x34dfd1e6316d5965(["grid_filled"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x34dfd1e6316d5965(["grid_filled"]):::outdated
    x24ddd600646592ff>"predict_meta"]:::outdated --> x34dfd1e6316d5965(["grid_filled"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x1447ab5d4f154b26(["covariates_predict_modis_mod11"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x1447ab5d4f154b26(["covariates_predict_modis_mod11"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x1447ab5d4f154b26(["covariates_predict_modis_mod11"]):::outdated
    xb6e57d3d3ffff164(["status_modis_mod11"]):::outdated --> x1447ab5d4f154b26(["covariates_predict_modis_mod11"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x9feedc99f60fb8e2(["covariates_predict_modis_mod13"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x9feedc99f60fb8e2(["covariates_predict_modis_mod13"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x9feedc99f60fb8e2(["covariates_predict_modis_mod13"]):::outdated
    x928b772015a85715(["status_modis_mod13"]):::outdated --> x9feedc99f60fb8e2(["covariates_predict_modis_mod13"]):::outdated
    x34dfd1e6316d5965(["grid_filled"]):::outdated --> x884e151c74202405(["summary_urban_rural"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> xd0d490a9f060f983(["covariates_predict_gmted"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> xd0d490a9f060f983(["covariates_predict_gmted"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xd0d490a9f060f983(["covariates_predict_gmted"]):::outdated
    x97ccb184baa85125(["status_gmted"]):::outdated --> xd0d490a9f060f983(["covariates_predict_gmted"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x5b6b66da6038376a(["covariates_predict_tri"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x5b6b66da6038376a(["covariates_predict_tri"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x5b6b66da6038376a(["covariates_predict_tri"]):::outdated
    xe98cd32cc9814568(["status_tri"]):::outdated --> x5b6b66da6038376a(["covariates_predict_tri"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> xb8edf20a967a6a4b(["status_nei"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xb8edf20a967a6a4b(["status_nei"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x2ff114299dc8e1f2(["covariates_predict_hms"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x2ff114299dc8e1f2(["covariates_predict_hms"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x2ff114299dc8e1f2(["covariates_predict_hms"]):::outdated
    x6493583ad3488b14(["status_hms"]):::outdated --> x2ff114299dc8e1f2(["covariates_predict_hms"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x39a05072aea743f2(["covariates_geos"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x39a05072aea743f2(["covariates_geos"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> x39a05072aea743f2(["covariates_geos"]):::outdated
    x0f891876c073bd4f(["status_geos"]):::outdated --> x39a05072aea743f2(["covariates_geos"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> xde2877a5305e32a8(["covariates_sedac_groads"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xde2877a5305e32a8(["covariates_sedac_groads"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> xde2877a5305e32a8(["covariates_sedac_groads"]):::outdated
    x0b5c3c8f9e90952c(["status_sedac_groads"]):::outdated --> xde2877a5305e32a8(["covariates_sedac_groads"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> x0f891876c073bd4f(["status_geos"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x0f891876c073bd4f(["status_geos"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x4f37a13ace9387f3(["covariates_predict_nlcd"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x4f37a13ace9387f3(["covariates_predict_nlcd"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x4f37a13ace9387f3(["covariates_predict_nlcd"]):::outdated
    x7ca9d1f32d84ce07(["status_nlcd"]):::outdated --> x4f37a13ace9387f3(["covariates_predict_nlcd"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> xb98b0bb5a3a3e056(["status_modis_mod06"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xb98b0bb5a3a3e056(["status_modis_mod06"]):::outdated
    xe3bb960370d9434e(["data_full"]):::outdated --> x72e3a67227f9c3bd(["base_prep_rf"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x72e3a67227f9c3bd(["base_prep_rf"]):::outdated
    x8bf4ce576c24f57b>"calculate_single"]:::outdated --> xddc8d4acb6955b97(["covariates_predict_koppen"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> xddc8d4acb6955b97(["covariates_predict_koppen"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xddc8d4acb6955b97(["covariates_predict_koppen"]):::outdated
    x82cb1ad0d4385e86(["status_koppen"]):::outdated --> xddc8d4acb6955b97(["covariates_predict_koppen"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x25add895167aa681(["covariates_gmted"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x25add895167aa681(["covariates_gmted"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> x25add895167aa681(["covariates_gmted"]):::outdated
    x97ccb184baa85125(["status_gmted"]):::outdated --> x25add895167aa681(["covariates_gmted"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> x421b919e1bf83506(["status_modis_mod09"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x421b919e1bf83506(["status_modis_mod09"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> xb6e57d3d3ffff164(["status_modis_mod11"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xb6e57d3d3ffff164(["status_modis_mod11"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> xe98cd32cc9814568(["status_tri"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xe98cd32cc9814568(["status_tri"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> x42fe841665001c0a(["covariates_predict_modis_mod06"]):::outdated
    xb8dff327793c8660(["covar_prediction_grid"]):::outdated --> x42fe841665001c0a(["covariates_predict_modis_mod06"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x42fe841665001c0a(["covariates_predict_modis_mod06"]):::outdated
    xb98b0bb5a3a3e056(["status_modis_mod06"]):::outdated --> x42fe841665001c0a(["covariates_predict_modis_mod06"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> xac4ef65e42f64136(["covariates_modis_mod06"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xac4ef65e42f64136(["covariates_modis_mod06"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> xac4ef65e42f64136(["covariates_modis_mod06"]):::outdated
    xb98b0bb5a3a3e056(["status_modis_mod06"]):::outdated --> xac4ef65e42f64136(["covariates_modis_mod06"]):::outdated
    x8ad79874a2d71502>"fastdown"]:::outdated --> x928b772015a85715(["status_modis_mod13"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> x928b772015a85715(["status_modis_mod13"]):::outdated
    xfcb38684c450014d>"calculate_multi"]:::outdated --> xaf8a0760bd264f54(["covariates_hms"]):::outdated
    x0c3b521dbc4a88da>"mr"]:::outdated --> xaf8a0760bd264f54(["covariates_hms"]):::outdated
    x9be289792a2fbebc(["sites_spat"]):::outdated --> xaf8a0760bd264f54(["covariates_hms"]):::outdated
    x6493583ad3488b14(["status_hms"]):::outdated --> xaf8a0760bd264f54(["covariates_hms"]):::outdated
    x7f17507fb63dde34>"meta_run"]:::outdated --> x7f17507fb63dde34>"meta_run"]:::outdated
    xe2c9527ed7587005>"check_file_status"]:::outdated --> xe2c9527ed7587005>"check_file_status"]:::outdated
    x440fee13d5fd163e>"batch_base_learner"]:::outdated --> x440fee13d5fd163e>"batch_base_learner"]:::outdated
  end
  classDef outdated stroke:#000000,color:#000000,fill:#78B7C5;
  classDef none stroke:#000000,color:#000000,fill:#94a4ac;
  linkStyle 0 stroke-width:0px;
  linkStyle 1 stroke-width:0px;
  linkStyle 264 stroke-width:0px;
  linkStyle 265 stroke-width:0px;
  linkStyle 266 stroke-width:0px;
```


