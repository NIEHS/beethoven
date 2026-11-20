# --- Hourly ---------------------------------------------------------------------------
# --- AQS ------------------------------------------------------------------------------
rule download_aqs:
    input:
        int_init_years="output/init/int_init_years.qs"

    output:
        list_dl_aqs="output/download/list_dl_aqs.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/b01_aqs.R"

# --- GEOS -----------------------------------------------------------------------------
rule download_geos:
    input:
        chr_init_dates="output/init/chr_init_dates.qs"

    output:
        list_dl_geos="output/download/list_dl_geos.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/b02_geos.R"

# --- HRRR -----------------------------------------------------------------------------
rule download_hrrr:
    input:
        chr_init_dates="output/init/chr_init_dates.qs"

    output:
        list_dl_hrrr="output/download/list_dl_hrrr.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/b03_hrrr.R"

# --- Daily ----------------------------------------------------------------------------
# --- HMS ------------------------------------------------------------------------------
rule download_hms:
    input:
        chr_init_dates="output/init/chr_init_dates.qs"

    output:
        list_dl_hms="output/download/list_dl_hms.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    conda:
        "beethoven"

    script:
        "../scripts/b10_hms.R"

# --- Static ---------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

