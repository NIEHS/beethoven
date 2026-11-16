########################################################################################
##############################            BASE            ##############################
rule prepare_base_cv:
    input:
        db_train_feat_sptmpl="output/train/feat/db_train_feat_sptmpl.qs",
        sf_train_sites_sp="output/train/sites/sf_train_sites_sp.qs",

    output:
        list_base_params_static="output/cv/base/list_base_params_static.qs",
        list_base_cv_idx="output/cv/base/list_base_cv_idx.qs"
    
    params:
        int_base_iter=config["int_base_iter"]

    container:
        None

    conda:
        "beethoven"

    script:
        "../scripts/d01_cvbase.R"

rule fit_base:
    input:
        list_base_params_static="output/cv/base/list_base_params_static.qs",
        db_train_feat_sptmpl="output/train/feat/db_train_feat_sptmpl.qs",
        list_base_cv_idx="output/cv/base/list_base_cv_idx.qs"

    output:
        db_train_pred_base_oof_sptmpl="output/train/pred/base/oof/db_train_pred_base_oof_sptmpl.qs",
        dt_base_xgb_eval="output/eval/base/dt_base_xgb_eval.qs",
        list_base_xgb_model="output/model/base/list_base_xgb_model.qs"

    container:
        None

    conda:
        "beethoven"

    script:
        "../scripts/d02_base.R"

##############################            META            ##############################
rule prepare_meta_cv:
    input:
        list_base_params_static="output/cv/base/list_base_params_static.qs",
        sf_train_sites_sp="output/train/sites/sf_train_sites_sp.qs"

    output:
        list_meta_cv_idx="output/cv/meta/list_meta_cv_idx.qs"
    
    params:
        int_meta_iter=config["int_meta_iter"]

    container:
        None

    conda:
        "beethoven"

    script:
        "../scripts/d03_cvmeta.R"

rule fit_meta:
    input:
        list_base_params_static="output/cv/base/list_base_params_static.qs",
        db_train_pred_base_oof_sptmpl="output/train/pred/base/oof/db_train_pred_base_oof_sptmpl.qs",
        list_meta_cv_idx="output/cv/meta/list_meta_cv_idx.qs"

    output:
        db_train_pred_meta_oof_sptmpl="output/train/pred/meta/oof/db_train_pred_meta_oof_sptmpl.qs",
        dt_meta_xgb_eval="output/eval/meta/dt_meta_xgb_eval.qs",
        list_meta_xgb_model="output/model/meta/list_meta_xgb_model.qs"

    container:
        None

    conda:
        "beethoven"

    script:
        "../scripts/d04_meta.R"

########################################################################################