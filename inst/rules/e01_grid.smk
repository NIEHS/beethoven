########################################################################################
##############################            GRID            ##############################
rule prediction_h3r8:
    input:
        dt_feat_merge_sptmpl="output/dt_feat_merge_sptmpl.qs"

    output:
        chr_pred_h3r8="output/chr_pred_h3r8.qs",
        sf_pred_grid_sp="output/sf_pred_grid_sp.qs"

    params:
        int_grid_res=lambda wildcards: config["int_grid_res"]

    container:
        "inst/container/sif/container_covariates.sif"

    script:
        "../scripts/e01_grid.R"

##############################            NARR            ##############################
rule prediction_narr:
    input:
        chr_init_dates="output/chr_init_dates.qs",
        chr_iter_narr="output/chr_iter_narr.qs",
        chr_pred_h3r8="output/chr_pred_h3r8.qs",
        sf_pred_grid_sp="output/sf_pred_grid_sp.qs"

    output:
        dt_pred_narr_sptmpl="output/dt_pred_narr_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        "inst/container/sif/container_covariates.sif"

    script:
        "../scripts/e02_narr.R"

##############################            HMS             ##############################
rule prediction_hms:
    input:
        chr_init_dates="output/chr_init_dates.qs",
        chr_pred_h3r8="output/chr_pred_h3r8.qs",
        sf_pred_grid_sp="output/sf_pred_grid_sp.qs"

    output:
        dt_pred_hms_sptmpl="output/dt_pred_hms_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        "inst/container/sif/container_covariates.sif"

    script:
        "../scripts/e03_hms.R"

##############################           MERGE            ##############################
rule prediction_merge:
    input:
        chr_pred_h3r8="output/chr_pred_h3r8.qs",
        dt_pred_narr_sptmpl="output/dt_pred_narr_sptmpl.qs",
        dt_pred_hms_sptmpl="output/dt_pred_hms_sptmpl.qs"

    output:
        dt_pred_merge_sptmpl="output/dt_pred_merge_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        "inst/container/sif/container_covariates.sif"

    script:
        "../scripts/e99_merge.R"

########################################################################################