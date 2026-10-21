########################################################################################
##############################            AQS             ##############################
rule calculate_aqs:
    input:
        chr_init_dates="output/chr_init_dates.qs",
        list_dl_aqs="output/list_dl_aqs.qs",
        list_dl_narr="output/list_dl_narr.qs",
        list_dl_hms="output/list_dl_hms.qs"

    output:
        sf_feat_aqs_sp="output/sf_feat_aqs_sp.qs",
        dt_feat_aqs_sptmpl="output/dt_feat_aqs_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        "inst/container/sif/container_covariates.sif"

    script:
        "../scripts/c01_aqs.R"

##############################            GEOS            ##############################
rule calculate_geos:
    input:
        chr_init_dates="output/chr_init_dates.qs",
        sf_feat_aqs_sp="output/sf_feat_aqs_sp.qs",

    output:
        dt_feat_geos_sptmpl="output/dt_feat_geos_sptmpl.parquet"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        None

    conda:
        "beethoven"

    script:
        "../scripts/c02_geos.R"

##############################            HMS             ##############################
rule calculate_hms:
    input:
        chr_init_dates="output/chr_init_dates.qs",
        sf_feat_aqs_sp="output/sf_feat_aqs_sp.qs",

    output:
        dt_feat_hms_sptmpl="output/dt_feat_hms_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        "inst/container/sif/container_covariates.sif"

    script:
        "../scripts/c03_hms.R"

##############################           MERGE            ##############################
rule calculate_merge:
    input:
        dt_feat_aqs_sptmpl="output/dt_feat_aqs_sptmpl.qs",
        # dt_feat_narr_sptmpl="output/dt_feat_narr_sptmpl.qs",
        dt_feat_hms_sptmpl="output/dt_feat_hms_sptmpl.qs",
        dt_feat_geos_sptmpl="output/dt_feat_geos_sptmpl.parquet"

    output:
        dt_feat_merge_sptmpl="output/dt_feat_merge_sptmpl.parquet"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        "inst/container/sif/container_covariates.sif"

    script:
        "../scripts/c99_merge.R"

########################################################################################