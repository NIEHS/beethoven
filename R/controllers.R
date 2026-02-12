#############################      SCRIPTLINES      #############################
#####################################################################################

#############################      CONTROLLER  SETUP    #############################
#####################################################################################

controller_general_beethoven <- crew.cluster::crew_controller_slurm(
    name = "controller_general_beethoven",
    workers = 1000,
    options_cluster = crew.cluster::crew_options_slurm(
        partition = "normal",
        memory_gigabytes_required = 12,
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        verbose = TRUE,
        script_lines = "#SBATCH --job-name=general_beethoven",
    ),
    tasks_max = Inf
)

#############################      DOWNLOAD    #############################

controller_download_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_download_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "geo",
        memory_gigabytes_required = 12,
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=download_geo",
    ),
    tasks_max = Inf
)


controller_initiate <- crew.cluster::crew_controller_slurm(
    name = "controller_initiate",
    workers = 1000,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        memory_gigabytes_required = 12,
        n_tasks = 1,
        log_output = "slurm/initiate_%j.out",
        log_error = "slurm/initiate_%j.err",
        script_lines = "#SBATCH --job-name=initiate"
    ),
    tasks_max = Inf
)

controller_download_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_download_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        memory_gigabytes_required = 25,
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=download_norm"
    ),
    tasks_max = Inf
)

controller_download_norm_big <- crew.cluster::crew_controller_slurm(
    name = "controller_download_norm_big",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        memory_gigabytes_required = 50,
        n_tasks = 1,
        log_output = "slurm/download_big_%j.out",
        log_error = "slurm/download_big_%j.err",
        script_lines = "#SBATCH --job-name=download_norm_big"
    ),
    tasks_max = Inf
)


#############################      CALCULATE FIT    #############################

controller_calc_fit_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_fit_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "geo",
        n_tasks = 1,
        memory_gigabytes_required = 12,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=calc_fit_geo"
    ),
    tasks_max = Inf
)

controller_calc_fit_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_fit_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=calc_fit_norm",
        memory_gigabytes_required = 25
    ),
    tasks_max = Inf
)

#############################      CALCULATE PREDICT    #############################

controller_calc_pred_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_pred_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "geo",
        n_tasks = 1,
        memory_gigabytes_required = 12,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=calc_pred_geo"
    ),
    tasks_max = Inf
)

controller_calc_pred_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_pred_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=calc_pred_norm",
        memory_gigabytes_required = 25
    ),
    tasks_max = Inf
)

controller_calc_pred_norm_big <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_pred_norm_big",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=calc_pred_norm_big",
        memory_gigabytes_required = 50
    ),
    tasks_max = Inf
)

controller_calc_pred_norm_huge <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_pred_norm_huge",
    workers = 100,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=calc_pred_norm_huge",
        memory_gigabytes_required = 100
    ),
    tasks_max = Inf
)

#############################      baselearners   #############################

controller_baselearners_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_baselearners_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "geo",
        n_tasks = 1,
        memory_gigabytes_required = 12,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=baselearners_geo"
    ),
    tasks_max = Inf
)

controller_baselearners_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_baselearners_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=baselearners_norm",
        memory_gigabytes_required = 25
    ),
    tasks_max = Inf
)

controller_baselearners_norm_big <- crew.cluster::crew_controller_slurm(
    name = "controller_baselearners_norm_big",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=baselearners_norm_big",
        memory_gigabytes_required = 50
    ),
    tasks_max = Inf
)

controller_baselearners_norm_huge <- crew.cluster::crew_controller_slurm(
    name = "controller_baselearners_norm_huge",
    workers = 100,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=baselearners_norm_huge",
        memory_gigabytes_required = 100
    ),
    tasks_max = Inf
)


#############################      metalearners   #############################

controller_metalearners_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_metalearners_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "geo",
        n_tasks = 1,
        memory_gigabytes_required = 12,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=metalearners_geo"
    ),
    tasks_max = Inf
)

controller_metalearners_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_metalearners_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=metalearners_norm",
        memory_gigabytes_required = 25
    ),
    tasks_max = Inf
)

controller_metalearners_norm_big <- crew.cluster::crew_controller_slurm(
    name = "controller_metalearners_norm_big",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=metalearners_norm_big",
        memory_gigabytes_required = 50
    ),
    tasks_max = Inf
)

controller_metalearners_norm_huge <- crew.cluster::crew_controller_slurm(
    name = "controller_metalearners_norm_huge",
    workers = 100,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=metalearners_norm_huge",
        memory_gigabytes_required = 100
    ),
    tasks_max = Inf
)


#############################      predict   #############################

controller_predict_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_predict_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "geo",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=calc_pred_geo"
    ),
    tasks_max = Inf
)

controller_predict_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_predict_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=calc_pred_norm",
        memory_gigabytes_required = 25
    ),
    tasks_max = Inf
)

controller_predict_norm_big <- crew.cluster::crew_controller_slurm(
    name = "controller_predict_norm_big",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=calc_pred_norm_big",
        memory_gigabytes_required = 50
    ),
    tasks_max = Inf
)

controller_predict_norm_huge <- crew.cluster::crew_controller_slurm(
    name = "controller_predict_norm_huge",
    workers = 100,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/%x_%j.out",
        log_error = "slurm/%x_%j.err",
        script_lines = "#SBATCH --job-name=calc_pred_norm_huge",
        memory_gigabytes_required = 100
    ),
    tasks_max = Inf
)


#############################      AQS   #############################

controller_aqs <- crew.cluster::crew_controller_slurm(
    name = "controller_aqs",
    workers = 100,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        partition = "normal",
        n_tasks = 1,
        log_output = "slurm/aqs_%j.out",
        log_error = "slurm/aqs_%j.err",
        script_lines = "#SBATCH --job-name=aqs"
    ),
    tasks_max = Inf
)
