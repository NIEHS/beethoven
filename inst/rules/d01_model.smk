########################################################################################
##############################            RSET            ##############################
rule model_cvrset:
    input:
        dt_feat_merge_sptmpl="output/dt_feat_merge_sptmpl.parquet"

    output:
        list_base_params_static="output/list_base_params_static.qs",
        list_base_cv_rset="output/list_base_cv_rset.qs"
    
    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        "inst/container/sif/container_models.sif"

    script:
        "../scripts/d01_cvrset.R"

##############################            SPARK           ##############################
rule model_spark:
    input:
        list_base_params_static="output/list_base_params_static.qs",
        dt_feat_merge_sptmpl="output/dt_feat_merge_sptmpl.parquet"

    output:
        dt_base_xgb_pred="output/dt_base_xgb_pred.qs"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        None

    conda:
        "sdamadeus"

    script:
        "../scripts/d02_spark.R"

##############################            BASE            ##############################
rule model_base:
    input:
        list_base_params_static="output/list_base_params_static.qs",
        list_base_cv_rset="output/list_base_cv_rset.qs"

    output:
        list_base_fit_elnet="output/list_base_fit_elnet.qs",
        dt_base_aqs_sptmpl="output/dt_base_aqs_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        "inst/container/sif/container_models.sif"

    script:
        "../scripts/d02_base.R"

##############################            META            ##############################

########################################################################################