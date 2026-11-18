########################################################################################
##############################            BASE            ##############################
rule predict_grid_base:
    input:
        db_grid_feat_sptmpl="output/grid/feat/db_grid_feat_sptmpl.qs",
        list_base_xgb_model="output/model/base/list_base_xgb_model.qs",
        list_meta_xgb_model="output/model/meta/list_meta_xgb_model.qs"

    output:
        db_grid_pred_base_mean_sptmpl="output/grid/pred/base/mean/db_grid_pred_base_mean_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/f01_base.R"

##############################            META            ##############################
rule predict_grid_meta:
    input:
        db_grid_pred_base_mean_sptmpl="output/grid/pred/base/mean/db_grid_pred_base_mean_sptmpl.qs",
        list_meta_xgb_model="output/model/meta/list_meta_xgb_model.qs"

    output:
        db_grid_pred_meta_mean_sptmpl="output/grid/pred/meta/mean/db_grid_pred_meta_mean_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/f02_meta.R"

########################################################################################