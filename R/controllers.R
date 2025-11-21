#############################      CONTROLLER  SETUP    #############################
#####################################################################################
#####################################################################################
#####################################################################################

controller_general_beethoven <- crew.cluster::crew_controller_slurm(
    name = "controller_general_beethoven",
    workers = 1000,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal,highmem,geo",
        script_lines = "apptainer exec /mnt/container_models.sif ",
        log_output = "/mnt/slurm/general_%j.out",
        log_error = "/mnt/slurm/general_%j.err",
        n_tasks = 1,
        memory_gigabytes_required = 12
    ),
    tasks_max = 1
)

#############################      DOWNLOAD    #############################

scriptlines_download_geo <- glue::glue(
    "#SBATCH --job-name=download \
#SBATCH --partition=geo \
#SBATCH --gres=gpu:1 
#SBATCH --ntasks=1 \
#SBATCH --output=/mnt/slurm/%x_%j.out \
#SBATCH --error=/mnt/slurm/%x_%j.err \
apptainer exec --nv ",
    "--env-file /mnt/beethoven_env.txt ",
    "/mnt/container_models.sif \\"
)

controller_download_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_download_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_download_geo
    ),
    tasks_max = 1
)


controller_initiate <- crew.cluster::crew_controller_slurm(
    name = "controller_initiate",
    workers = 1000,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal,highmem,geo",
        script_lines = "apptainer exec /mnt/container_models.sif ",
        log_output = "/mnt/slurm/initial_%j.out",
        log_error = "/mnt/slurm/initial_%j.err",
        n_tasks = 1,
        memory_gigabytes_required = 12
    ),
    tasks_max = Inf
)

controller_download_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_download_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal,highmem,geo",
        script_lines = "apptainer exec /mnt/container_models.sif ",
        log_output = "/mnt/slurm/download_%j.out",
        log_error = "/mnt/slurm/download_%j.err",
        n_tasks = 1,
        memory_gigabytes_required = 25
    ),
    tasks_max = Inf
)

controller_download_norm_big <- crew.cluster::crew_controller_slurm(
    name = "controller_download_norm_big",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal,highmem,geo",
        script_lines = "apptainer exec /mnt/container_models.sif ",
        log_output = "/mnt/slurm/download_%j.out",
        log_error = "/mnt/slurm/download_%j.err",
        n_tasks = 1,
        memory_gigabytes_required = 50
    ),
    tasks_max = Inf
)

controller_download_noenv <- crew.cluster::crew_controller_slurm(
    name = "controller_download_noenv",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal,highmem,geo",
        script_lines = "apptainer exec /mnt/container_models.sif ",
        log_output = "/mnt/slurm/download_%j.out",
        log_error = "/mnt/slurm/download_%j.err",
        n_tasks = 1,
        memory_gigabytes_required = 50
    ),
    tasks_max = 1
)


#############################      CALCULATE FIT    #############################

scriptlines_calc_fit_geo <- glue::glue(
    "#SBATCH --job-name=calc_fit \
#SBATCH --partition=geo \
#SBATCH --gres=gpu:1 
#SBATCH --ntasks=1 \
#SBATCH --output=/mnt/slurm/%x_%j.out \
#SBATCH --error=/mnt/slurm/%x_%j.err \
apptainer exec --nv ",
    "--env-file /mnt/beethoven_env.txt ",
    "/mnt/container_models.sif \\"
)

controller_calc_fit_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_fit_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_calc_fit_geo
    ),
    tasks_max = 1
)


scriptlines_calc_fit_norm <- glue::glue(
    "#SBATCH --partition=normal \
#SBATCH --job-name=calc_fit \
#SBATCH --ntasks=1 \
#SBATCH --output=/mnt/slurm/%x_%j.out \
#SBATCH --error=/mnt/slurm/%x_%j.err \
  apptainer exec ",
    "--env-file /mnt/beethoven_env.txt ",
    "/mnt/container_models.sif \\"
)

controller_calc_fit_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_fit_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_calc_fit_norm,
        memory_gigabytes_required = 25
    ),
    tasks_max = 1
)

controller_calc_fit_norm_big <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_fit_norm_big",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_calc_fit_norm,
        memory_gigabytes_required = 50
    ),
    tasks_max = 1
)

controller_calc_fit_norm_huge <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_fit_norm_huge",
    workers = 100,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_calc_fit_norm,
        memory_gigabytes_required = 100
    ),
    tasks_max = 1
)


#############################      CALCULATE PREDICT    #############################

scriptlines_calc_pred_geo <- glue::glue(
    "#SBATCH --job-name=calc_pred \
#SBATCH --partition=geo \
#SBATCH --gres=gpu:1 
#SBATCH --ntasks=1 \
#SBATCH --output=/mnt/slurm/%x_%j.out \
#SBATCH --error=/mnt/slurm/%x_%j.err \
apptainer exec --nv ",
    "--env-file /mnt/beethoven_env.txt ",
    "/mnt/container_models.sif \\"
)

controller_calc_pred_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_pred_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_calc_pred_geo
    ),
    tasks_max = 1
)


scriptlines_calc_pred_norm <- glue::glue(
    "#SBATCH --partition=normal \
#SBATCH --job-name=calc_pred \
#SBATCH --ntasks=1 \
#SBATCH --output=/mnt/slurm/%x_%j.out \
#SBATCH --error=/mnt/slurm/%x_%j.err \
  apptainer exec ",
    "--env-file /mnt/beethoven_env.txt ",
    "/mnt/container_models.sif \\"
)

controller_calc_pred_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_pred_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_calc_pred_norm,
        memory_gigabytes_required = 25
    ),
    tasks_max = 1
)

controller_calc_pred_norm_big <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_pred_norm_big",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_calc_pred_norm,
        memory_gigabytes_required = 50
    ),
    tasks_max = 1
)

controller_calc_pred_norm_huge <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_pred_norm_huge",
    workers = 100,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_calc_pred_norm,
        memory_gigabytes_required = 100
    ),
    tasks_max = 1
)

#############################      baselearners   #############################

scriptlines_baselearners_geo <- glue::glue(
    "#SBATCH --job-name=baselearners \
#SBATCH --partition=geo \
#SBATCH --gres=gpu:1 
#SBATCH --ntasks=1 \
#SBATCH --output=/mnt/slurm/%x_%j.out \
#SBATCH --error=/mnt/slurm/%x_%j.err \
apptainer exec --nv ",
    "--env-file /mnt/beethoven_env.txt ",
    "/mnt/container_models.sif \\"
)

controller_baselearners_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_baselearners_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_baselearners_geo
    ),
    tasks_max = 1
)


scriptlines_baselearners_norm <- glue::glue(
    "#SBATCH --partition=normal \
#SBATCH --job-name=baselearners \
#SBATCH --ntasks=1 \
#SBATCH --output=/mnt/slurm/%x_%j.out \
#SBATCH --error=/mnt/slurm/%x_%j.err \
  apptainer exec ",
    "--env-file /mnt/beethoven_env.txt ",
    "/mnt/container_models.sif \\"
)

controller_baselearners_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_baselearners_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_baselearners_norm,
        memory_gigabytes_required = 25
    ),
    tasks_max = 1
)

controller_baselearners_norm_big <- crew.cluster::crew_controller_slurm(
    name = "controller_baselearners_norm_big",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_baselearners_norm,
        memory_gigabytes_required = 50
    ),
    tasks_max = 1
)

controller_baselearners_norm_huge <- crew.cluster::crew_controller_slurm(
    name = "controller_baselearners_norm_huge",
    workers = 100,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_baselearners_norm,
        memory_gigabytes_required = 100
    ),
    tasks_max = 1
)


#############################      metalearners   #############################

scriptlines_metalearners_geo <- glue::glue(
    "#SBATCH --job-name=metalearners \
#SBATCH --partition=geo \
#SBATCH --gres=gpu:1 
#SBATCH --ntasks=1 \
#SBATCH --output=/mnt/slurm/%x_%j.out \
#SBATCH --error=/mnt/slurm/%x_%j.err \
apptainer exec --nv ",
    "--env-file /mnt/beethoven_env.txt ",
    "/mnt/container_models.sif \\"
)

controller_metalearners_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_metalearners_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_metalearners_geo
    ),
    tasks_max = 1
)


scriptlines_metalearners_norm <- glue::glue(
    "#SBATCH --partition=normal \
#SBATCH --job-name=metalearners \
#SBATCH --ntasks=1 \
#SBATCH --output=/mnt/slurm/%x_%j.out \
#SBATCH --error=/mnt/slurm/%x_%j.err \
  apptainer exec ",
    "--env-file /mnt/beethoven_env.txt ",
    "/mnt/container_models.sif \\"
)

controller_metalearners_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_metalearners_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_metalearners_norm,
        memory_gigabytes_required = 25
    ),
    tasks_max = 1
)

controller_metalearners_norm_big <- crew.cluster::crew_controller_slurm(
    name = "controller_metalearners_norm_big",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_metalearners_norm,
        memory_gigabytes_required = 50
    ),
    tasks_max = 1
)

controller_metalearners_norm_huge <- crew.cluster::crew_controller_slurm(
    name = "controller_metalearners_norm_huge",
    workers = 100,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_metalearners_norm,
        memory_gigabytes_required = 100
    ),
    tasks_max = 1
)


#############################      predict   #############################

scriptlines_predict_geo <- glue::glue(
    "#SBATCH --job-name=predict \
#SBATCH --partition=geo \
#SBATCH --gres=gpu:1 
#SBATCH --ntasks=1 \
#SBATCH --output=/mnt/slurm/%x_%j.out \
#SBATCH --error=/mnt/slurm/%x_%j.err \
apptainer exec --nv ",
    "--env-file /mnt/beethoven_env.txt ",
    "/mnt/container_models.sif \\"
)

controller_predict_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_predict_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_predict_geo
    ),
    tasks_max = 1
)


scriptlines_predict_norm <- glue::glue(
    "#SBATCH --partition=normal \
#SBATCH --job-name=predict \
#SBATCH --ntasks=1 \
#SBATCH --output=/mnt/slurm/%x_%j.out \
#SBATCH --error=/mnt/slurm/%x_%j.err \
  apptainer exec ",
    "--env-file /mnt/beethoven_env.txt ",
    "/mnt/container_models.sif \\"
)

controller_predict_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_predict_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_predict_norm,
        memory_gigabytes_required = 25
    ),
    tasks_max = 1
)

controller_predict_norm_big <- crew.cluster::crew_controller_slurm(
    name = "controller_predict_norm_big",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_predict_norm,
        memory_gigabytes_required = 50
    ),
    tasks_max = 1
)

controller_predict_norm_huge <- crew.cluster::crew_controller_slurm(
    name = "controller_predict_norm_huge",
    workers = 100,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_predict_norm,
        memory_gigabytes_required = 100
    ),
    tasks_max = 1
)


#############################      AQS   #############################

scriptlines_aqs <- glue::glue(
    "#SBATCH --job-name=aqs \
#SBATCH --partition=normal \
#SBATCH --ntasks=1 \
#SBATCH --output=/mnt/slurm/%x_%j.out \
#SBATCH --error=/mnt/slurm/%x_%j.err \
apptainer exec ",
    "--env-file /mnt/beethoven_env.txt ",
    "/mnt/container_models.sif \\"
)

controller_aqs <- crew.cluster::crew_controller_slurm(
    name = "controller_aqs",
    workers = 100,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal,highmem,geo",
        n_tasks = 1,
        script_lines = scriptlines_aqs,
        log_output = "/mnt/slurm/%x_%j.out",
        log_error = "/mnt/slurm/%x_%j.err",
        memory_gigabytes_required = 50
    ),
    tasks_max = 1
)
