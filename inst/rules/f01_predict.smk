########################################################################################
##############################            BASE            ##############################
rule predict_base:
    input:
        dt_pred_merge_sptmpl="output/dt_pred_merge_sptmpl.qs",
        list_base_fit_elnet="output/list_base_fit_elnet.qs"

    output:
        dt_pred_base_sptmpl="output/dt_pred_base_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        "inst/container/sif/container_models.sif"

    script:
        "../scripts/f01_base.R"

########################################################################################