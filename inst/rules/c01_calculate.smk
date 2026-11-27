# --- Hourly ---------------------------------------------------------------------------
# --- AQS ------------------------------------------------------------------------------
rule prepare_train_obs_aqs:
    input:
        chr_init_dates="output/init/chr_init_dates.qs",
        list_dl_aqs="output/download/list_dl_aqs.qs",
        list_dl_geos="output/download/list_dl_geos.qs",
        list_dl_hrrr="output/download/list_dl_hrrr.qs",
        list_dl_hms="output/download/list_dl_hms.qs"

    output:
        sf_train_sites_sp="output/train/sites/sf_train_sites_sp.qs",
        db_train_obs_aqs_sptmpl="output/train/obs/db_train_obs_aqs_sptmpl.parquet"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/c01_aqs.R"

# --- GEOS -----------------------------------------------------------------------------
rule prepare_train_feat_geos:
    input:
        chr_init_dates="output/init/chr_init_dates.qs",
        sf_train_sites_sp="output/train/sites/sf_train_sites_sp.qs",

    output:
        db_train_feat_geos_sptmpl="output/train/feat/db_train_feat_geos_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/c02_geos.R"

# --- HRRR -----------------------------------------------------------------------------
rule prepare_train_feat_hrrr:
    input:
        chr_init_dates="output/init/chr_init_dates.qs",
        sf_train_sites_sp="output/train/sites/sf_train_sites_sp.qs",

    output:
        db_train_feat_hrrr_sptmpl="output/train/feat/db_train_feat_hrrr_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/c03_hrrr.R"

# --- Daily ----------------------------------------------------------------------------
# --- HMS ------------------------------------------------------------------------------
rule prepare_train_feat_hms:
    input:
        chr_init_dates="output/init/chr_init_dates.qs",
        sf_train_sites_sp="output/train/sites/sf_train_sites_sp.qs",

    output:
        db_train_feat_hms_sptmpl="output/train/feat/db_train_feat_hms_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/c10_hms.R"

# --- Static ---------------------------------------------------------------------------

# --- Derived --------------------------------------------------------------------------
# --- Spacetime ------------------------------------------------------------------------
rule prepare_train_feat_spacetime:
    input:
        chr_init_dates="output/init/chr_init_dates.qs",
        sf_train_sites_sp="output/train/sites/sf_train_sites_sp.qs",

    output:
        db_train_feat_spacetime_sptmpl="output/train/feat/db_train_feat_spacetime_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/c30_spacetime.R"

# --- Merge ----------------------------------------------------------------------------
rule prepare_train_feat:
    input:
        db_train_obs_aqs_sptmpl="output/train/obs/db_train_obs_aqs_sptmpl.parquet",
        db_train_feat_geos_sptmpl="output/train/feat/db_train_feat_geos_sptmpl.qs",
        db_train_feat_hrrr_sptmpl="output/train/feat/db_train_feat_hrrr_sptmpl.qs",
        db_train_feat_hms_sptmpl="output/train/feat/db_train_feat_hms_sptmpl.qs",
        db_train_feat_spacetime_sptmpl="output/train/feat/db_train_feat_spacetime_sptmpl.qs"

    output:
        db_train_feat_sptmpl="output/train/feat/db_train_feat_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/c99_merge.R"

# --------------------------------------------------------------------------------------
