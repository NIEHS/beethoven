# --- Hourly ---------------------------------------------------------------------------
# --- Grid -----------------------------------------------------------------------------
rule prepare_grid_sites:
    input:
        chr_init_dates="output/init/chr_init_dates.qs",
        list_meta_xgb_model="output/model/meta/list_meta_xgb_model.qs"

    output:
        sf_grid_sites_sp="output/grid/sites/sf_grid_sites_sp.qs",
        db_grid_index_sptmpl="output/grid/index/db_grid_index_sptmpl.parquet"

    params:
        int_grid_res=lambda wildcards: config["int_grid_res"]

    conda:
        "beethoven"

    script:
        "../scripts/e01_grid.R"

# --- GEOS -----------------------------------------------------------------------------
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

# --- HRRR -----------------------------------------------------------------------------
rule prepare_grid_feat_hrrr:
    input:
        chr_init_dates="output/init/chr_init_dates.qs",
        sf_grid_sites_sp="output/grid/sites/sf_grid_sites_sp.qs"

    output:
        db_grid_feat_hrrr_sptmpl="output/grid/feat/db_grid_feat_hrrr_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/e03_hrrr.R"

# --- Daily ----------------------------------------------------------------------------
# --- HMS ------------------------------------------------------------------------------
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
        "../scripts/e10_hms.R"

# --- Static ---------------------------------------------------------------------------

# --- Derived --------------------------------------------------------------------------
# --- Spacetime ------------------------------------------------------------------------
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
        "../scripts/e30_spacetime.R"

# --- Merge ----------------------------------------------------------------------------
rule prepare_grid_feat:
    input:
        db_grid_index_sptmpl="output/grid/index/db_grid_index_sptmpl.parquet",
        db_grid_feat_geos_sptmpl="output/grid/feat/db_grid_feat_geos_sptmpl.qs",
        db_grid_feat_hrrr_sptmpl="output/grid/feat/db_grid_feat_hrrr_sptmpl.qs",
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

# --------------------------------------------------------------------------------------
