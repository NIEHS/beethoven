########################################################################################
##############################            GRID            ##############################
rule prepare_grid_sites:
    input:
        chr_init_dates="output/init/chr_init_dates.qs",
        db_train_feat_sptmpl="output/train/feat/db_train_feat_sptmpl.qs"

    output:
        sf_grid_sites_sp="output/grid/sites/sf_grid_sites_sp.qs",
        db_grid_index_sptmpl="output/grid/index/db_grid_index_sptmpl.parquet"

    params:
        int_grid_res=lambda wildcards: config["int_grid_res"]

    conda:
        "beethoven"

    script:
        "../scripts/e01_grid.R"


##############################            GEOS            ##############################
rule prepare_grid_feat_geos:
    input:
        chr_init_dates="output/init/chr_init_dates.qs",
        sf_grid_sites_sp="output/grid/sites/sf_grid_sites_sp.qs"

    output:
        db_grid_feat_geos_sptmpl="output/grid/feat/db_grid_feat_geos_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/e02_geos.R"

##############################            HMS             ##############################
rule prepare_grid_feat_hms:
    input:
        chr_init_dates="output/init/chr_init_dates.qs",
        sf_grid_sites_sp="output/grid/sites/sf_grid_sites_sp.qs"

    output:
        db_grid_feat_hms_sptmpl="output/grid/feat/db_grid_feat_hms_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/e03_hms.R"

##############################          SPACETIME         ##############################
rule prepare_grid_feat_spacetime:
    input:
        chr_init_dates="output/init/chr_init_dates.qs",
        sf_grid_sites_sp="output/grid/sites/sf_grid_sites_sp.qs"

    output:
        db_grid_feat_spacetime_sptmpl="output/grid/feat/db_grid_feat_spacetime_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/e04_spacetime.R"

##############################           MERGE            ##############################
rule prepare_grid_feat:
    input:
        db_grid_index_sptmpl="output/grid/index/db_grid_index_sptmpl.parquet",
        db_grid_feat_geos_sptmpl="output/grid/feat/db_grid_feat_geos_sptmpl.qs",
        db_grid_feat_hms_sptmpl="output/grid/feat/db_grid_feat_hms_sptmpl.qs",
        db_grid_feat_spacetime_sptmpl="output/grid/feat/db_grid_feat_spacetime_sptmpl.qs"

    output:
        db_grid_feat_sptmpl="output/grid/feat/db_grid_feat_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/e99_merge.R"

########################################################################################