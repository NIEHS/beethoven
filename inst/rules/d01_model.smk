########################################################################################
##############################            RSET            ##############################
rule model_cvrset:
    input:
        dt_feat_merge_sptmpl="output/dt_feat_merge_sptmpl.parquet",
        sf_feat_aqs_sp="output/sf_feat_aqs_sp.qs",

    output:
        list_base_params_static="output/list_base_params_static.qs",
        list_base_cv_idx="output/list_base_cv_idx.qs"
    
    params:
        int_base_iter=lambda wildcards: config["int_base_iter"],
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        None

    conda:
        "beethoven"

    script:
        "../scripts/d01_cvrset.R"

##############################            BASE            ##############################
rule model_base:
    input:
        list_base_params_static="output/list_base_params_static.qs",
        dt_feat_merge_sptmpl="output/dt_feat_merge_sptmpl.parquet",
        list_base_cv_idx="output/list_base_cv_idx.qs"

    output:
        dt_base_xgb_pred="output/dt_base_xgb_pred.parquet",
        dt_base_xgb_eval="output/dt_base_xgb_eval.qs"

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
        "../scripts/d02_base.R"

##############################            META            ##############################
rule model_meta:
    input:
        list_base_params_static="output/list_base_params_static.qs",
        dt_base_xgb_pred="output/dt_base_xgb_pred.parquet"

    output:
        dt_meta_xgb_pred="output/dt_meta_xgb_pred.parquet",
        dt_meta_xgb_eval="output/dt_meta_xgb_eval.qs"

    params:
        int_meta_iter=lambda wildcards: config["int_meta_iter"],
        chr_config_dir=os.path.join(
            os.environ["HOME"],
            config["chr_config_dir"],
        )

    container:
        None

    conda:
        "beethoven"

    script:
        "../scripts/d03_meta.R"

########################################################################################