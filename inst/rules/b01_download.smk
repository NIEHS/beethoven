########################################################################################
##############################            AQS             ##############################
rule download_aqs:
    input:
        int_init_years="output/int_init_years.qs"

    output:
        list_dl_aqs="output/list_dl_aqs.qs"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        "inst/container/sif/container_covariates.sif"

    script:
        "../scripts/b01_aqs.R"

##############################            NARR            ##############################
rule download_narr:
    input:
        int_init_years="output/int_init_years.qs"

    output:
        chr_iter_narr="output/chr_iter_narr.qs",
        list_dl_narr="output/list_dl_narr.qs"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        "inst/container/sif/container_covariates.sif"

    script:
        "../scripts/b02_narr.R"

##############################            HMS             ##############################
rule download_hms:
    input:
        chr_init_dates="output/chr_init_dates.qs"

    output:
        list_dl_hms="output/list_dl_hms.qs"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        "inst/container/sif/container_covariates.sif"

    script:
        "../scripts/b03_hms.R"

########################################################################################
