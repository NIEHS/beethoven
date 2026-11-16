########################################################################################
##############################            BASE            ##############################
rule predict_base:
    input:
        db_pred_merge_sptmpl="output/grid/db_pred_merge_sptmpl.qs",
        list_base_xgb_model="output/base/list_base_xgb_model.qs"

    output:
        db_pred_base_sptmpl="output/pred/db_pred_base_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/f01_base.R"

##############################            META            ##############################
rule predict_meta:
    input:
        db_pred_base_sptmpl="output/pred/db_pred_base_sptmpl.qs",
        list_meta_xgb_model="output/meta/list_meta_xgb_model.qs"

    output:
        db_pred_meta_sptmpl="output/pred/db_pred_meta_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/f02_meta.R"


########################################################################################