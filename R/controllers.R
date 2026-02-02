#############################      SCRIPTLINES      #############################
#####################################################################################

scriptlines_general <- glue::glue(
    "#SBATCH --job-name=general \
  #SBATCH --partition=normal \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=12G \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_download_geo <- glue::glue(
    "#SBATCH --job-name=download \
  #SBATCH --partition=geo \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=12G \
  #SBATCH --gres=gpu:1 \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_initiate <- glue::glue(
    "#SBATCH --job-name=initiate \
  #SBATCH --partition=normal \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=12G \
  #SBATCH --output=slurm/initiate_%j.out \
  #SBATCH --error=slurm/initiate_%j.err \
  apptainer exec --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_download_normal <- glue::glue(
    "#SBATCH --job-name=download_normal \
  #SBATCH --partition=normal \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=25G \
  #SBATCH --output=slurm/download_normal_%j.out \
  #SBATCH --error=slurm/download_normal_%j.err \
  apptainer exec --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_download_big <- glue::glue(
    "#SBATCH --job-name=download_big \
  #SBATCH --partition=normal \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=50G \
  #SBATCH --output=slurm/download_big_%j.out \
  #SBATCH --error=slurm/download_big_%j.err \
  apptainer exec --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_calc_fit_geo <- glue::glue(
    "#SBATCH --job-name=calc_fit \
  #SBATCH --partition=geo \
  #SBATCH --gres=gpu:1 \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=12G \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --nv --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_calc_fit_norm <- glue::glue(
    "#SBATCH --job-name=calc_fit \
  #SBATCH --partition=normal \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=25G \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_calc_pred_geo <- glue::glue(
    "#SBATCH --job-name=calc_pred \
  #SBATCH --partition=geo \
  #SBATCH --gres=gpu:1 \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=12G \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --nv --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_calc_pred_norm <- glue::glue(
    "#SBATCH --job-name=calc_pred \
  #SBATCH --partition=normal \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=25G \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_baselearners_geo <- glue::glue(
    "#SBATCH --job-name=baselearners \
  #SBATCH --partition=geo \
  #SBATCH --gres=gpu:1 \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=12G \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --nv --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_baselearners_norm <- glue::glue(
    "#SBATCH --job-name=baselearners \
  #SBATCH --partition=normal \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=25G \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_metalearners_geo <- glue::glue(
    "#SBATCH --job-name=metalearners \
  #SBATCH --partition=geo \
  #SBATCH --gres=gpu:1 \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=12G \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --nv --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_metalearners_norm <- glue::glue(
    "#SBATCH --job-name=metalearners \
  #SBATCH --partition=normal \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=25G \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_predict_geo <- glue::glue(
    "#SBATCH --job-name=predict \
  #SBATCH --partition=geo \
  #SBATCH --gres=gpu:1 \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=12G \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --nv --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_predict_norm <- glue::glue(
    "#SBATCH --job-name=predict \
  #SBATCH --partition=normal \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=25G \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)

scriptlines_aqs <- glue::glue(
    "#SBATCH --job-name=aqs \
  #SBATCH --partition=normal \
  #SBATCH --ntasks=1 \
  #SBATCH --mem=50G \
  #SBATCH --output=slurm/%x_%j.out \
  #SBATCH --error=slurm/%x_%j.err \
  apptainer exec --bind $PWD:/mnt --bind $PWD/inst:/inst --bind /ddn/gs1/group/set/beethoven_store/input:/input --bind /ddn/gs1/group/set/beethoven_store/_targets:/opt/_targets --bind /run/munge:/run/munge --bind /ddn/gs1/tools/slurm/etc/slurm:/ddn/gs1/tools/slurm/etc/slurm --env-file beethoven_env.txt container_mlverse.sif \\"
)


#############################      CONTROLLER  SETUP    #############################
#####################################################################################

controller_general_beethoven <- crew.cluster::crew_controller_slurm(
    name = "controller_general_beethoven",
    workers = 1000,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_general,
    ),
    tasks_max = Inf
)


#############################      DOWNLOAD    #############################

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
        script_lines = scriptlines_initiate
    ),
    tasks_max = Inf
)

controller_download_norm <- crew.cluster::crew_controller_slurm(
    name = "controller_download_norm",
    workers = 500,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_download_normal
    ),
    tasks_max = Inf
)

controller_download_norm_big <- crew.cluster::crew_controller_slurm(
    name = "controller_download_norm_big",
    workers = 250,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_download_big
    ),
    tasks_max = Inf
)


#############################      CALCULATE FIT    #############################

controller_calc_fit_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_fit_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_calc_fit_geo
    ),
    tasks_max = 1
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

#############################      CALCULATE PREDICT    #############################

controller_calc_pred_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_calc_pred_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_calc_pred_geo
    ),
    tasks_max = 1
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

controller_baselearners_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_baselearners_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_baselearners_geo
    ),
    tasks_max = 1
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

controller_metalearners_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_metalearners_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_metalearners_geo
    ),
    tasks_max = 1
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

controller_predict_geo <- crew.cluster::crew_controller_slurm(
    name = "controller_predict_geo",
    workers = 4,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_predict_geo
    ),
    tasks_max = 1
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

controller_aqs <- crew.cluster::crew_controller_slurm(
    name = "controller_aqs",
    workers = 100,
    options_cluster = crew.cluster::crew_options_slurm(
        verbose = TRUE,
        script_lines = scriptlines_aqs
    ),
    tasks_max = Inf
)
