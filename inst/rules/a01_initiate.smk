########################################################################################
# Generate all dates in date range.
rule initiate:
    output:
        chr_init_dates="output/init/chr_init_dates.qs",
        int_init_years="output/init/int_init_years.qs",
        list_init_dates="output/init/list_init_dates.qs",
        list_init_datesj="output/init/list_init_datesj.qs"

    params:
        start=config["chr_config_daterange"]["start"],
        end=config["chr_config_daterange"]["end"],

    container:
        "inst/container/sif/container_covariates.sif"

    script:
        "../scripts/a01_initiate.R"

########################################################################################
