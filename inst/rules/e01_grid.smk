########################################################################################
##############################            GRID            ##############################
rule prediction_grid:
    input:
        chr_init_dates="output/chr_init_dates.qs",
        db_feat_merge_sptmpl="output/feat/db_feat_merge_sptmpl.qs"

    output:
        sf_pred_grid_sp="output/grid/sf_pred_grid_sp.qs",
        db_pred_grid_sptmpl="output/grid/db_pred_grid_sptmpl.parquet"

    params:
        int_grid_res=lambda wildcards: config["int_grid_res"]

    conda:
        "beethoven"

    script:
        "../scripts/e01_grid.R"


##############################            GEOS            ##############################
rule prediction_geos:
    input:
        chr_init_dates="output/chr_init_dates.qs",
        sf_pred_grid_sp="output/grid/sf_pred_grid_sp.qs"

    output:
        db_pred_geos_sptmpl="output/grid/db_pred_geos_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/e02_geos.R"

##############################            HMS             ##############################
rule prediction_hms:
    input:
        chr_init_dates="output/chr_init_dates.qs",
        sf_pred_grid_sp="output/grid/sf_pred_grid_sp.qs"

    output:
        db_pred_hms_sptmpl="output/grid/db_pred_hms_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/e03_hms.R"

##############################           MERGE            ##############################
rule prediction_merge:
    input:
        db_pred_grid_sptmpl="output/grid/db_pred_grid_sptmpl.parquet",
        db_pred_geos_sptmpl="output/grid/db_pred_geos_sptmpl.qs",
        db_pred_hms_sptmpl="output/grid/db_pred_hms_sptmpl.qs"

    output:
        db_pred_merge_sptmpl="output/grid/db_pred_merge_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/e99_merge.R"

########################################################################################