########################################################################################
# Path to BEETHOVEN configuration file.
configfile: "config/config.yaml"

########################################################################################
print(
    f"Running {{beethoven}} pipeline: "
    f"{config['chr_config_daterange']['start']} - "
    f"{config['chr_config_daterange']['end']}"
)

########################################################################################
# Define pipeline endpoint.
rule all:
    input:
        "output/dt_base_xgb_eval.qs"

###############################      PIPELINE RULES      ###############################
include: "inst/rules/a01_initiate.smk"
include: "inst/rules/b01_download.smk"
include: "inst/rules/c01_calculate.smk"
include: "inst/rules/d01_model.smk"
include: "inst/rules/e01_grid.smk"
include: "inst/rules/f01_predict.smk"
