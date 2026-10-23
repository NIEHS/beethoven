########################################################################################
##############################            AQS             ##############################
rule calculate_aqs:
    input:
        chr_init_dates="output/chr_init_dates.qs",
        list_dl_aqs="output/list_dl_aqs.qs",
        list_dl_narr="output/list_dl_narr.qs",
        list_dl_hms="output/list_dl_hms.qs"

    output:
        sf_feat_aqs_sp="output/sf_feat_aqs_sp.qs",
        db_feat_aqs_sptmpl="output/db_feat_aqs_sptmpl.parquet"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    container:
        None

    conda:
        "beethoven"

    script:
        "../scripts/c01_aqs.R"

##############################            GEOS            ##############################
rule calculate_geos:
    input:
        chr_init_dates="output/chr_init_dates.qs",
        sf_feat_aqs_sp="output/sf_feat_aqs_sp.qs",

    output:
        db_feat_geos_sptmpl="output/db_feat_geos_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    container:
        None

    conda:
        "beethoven"

    script:
        "../scripts/c02_geos.R"

##############################            HMS             ##############################
rule calculate_hms:
    input:
        chr_init_dates="output/chr_init_dates.qs",
        sf_feat_aqs_sp="output/sf_feat_aqs_sp.qs",

    output:
        db_feat_hms_sptmpl="output/db_feat_hms_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    container:
        None

    conda:
        "beethoven"

    script:
        "../scripts/c03_hms.R"

##############################           MERGE            ##############################
rule calculate_merge:
    input:
        db_feat_aqs_sptmpl="output/db_feat_aqs_sptmpl.parquet",
        db_feat_geos_sptmpl="output/db_feat_geos_sptmpl.qs",
        db_feat_hms_sptmpl="output/db_feat_hms_sptmpl.qs",

    output:
        db_feat_merge_sptmpl="output/db_feat_merge_sptmpl.qs"

    params:
        chr_config_dir=os.path.join(os.environ["HOME"], config["chr_config_dir"])

    container:
        None

    conda:
        "beethoven"

    script:
        "../scripts/c99_merge.R"

########################################################################################