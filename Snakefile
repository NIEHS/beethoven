# --- Configuration --------------------------------------------------------------------
configfile: "config/config.yaml"

# --- Run summary ----------------------------------------------------------------------
print(
    f"Running {{beethoven}} pipeline: "
    f"{config['chr_config_daterange']['start']} - "
    f"{config['chr_config_daterange']['end']}"
)

# --- Pipeline endpoint ----------------------------------------------------------------
rule all:
    input:
        "output/model/meta/list_meta_xgb_model.qs"

# --- Pipeline rules -------------------------------------------------------------------
include: "inst/rules/a01_initiate.smk"
include: "inst/rules/b01_download.smk"
include: "inst/rules/c01_calculate.smk"
include: "inst/rules/d01_model.smk"
include: "inst/rules/e01_grid.smk"
include: "inst/rules/f01_predict.smk"
