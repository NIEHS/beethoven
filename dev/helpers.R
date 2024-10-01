# srun --partition=geo --cpus-per-task=1 --pty top
# scontrol show node gn040815 # geo cluster
# scontrol show node cn040827 # hihgmem cluster
# sacct -j 303493 --format=JobID,Elapsed,TotalCPU,MaxRSS
job <- function(job_id) {
  system(
    paste0("sacct -j ", job_id, " --format=JobID,Elapsed,TotalCPU,MaxRSS")
  )
}

kb_to_gb <- function(kb) {
  gb <- kb / (1024^2)
  return(gb)
}

geo <- function() {
  system("srun --partition=geo --cpus-per-task=1 --pty top")
}

node <- function(node = "gn040815") {
  system(paste0("scontrol show node ", node))
}

queue <- function() {
  system("squeue -u $USER")
}

cancel <- function() {
  system("scancel -u $USER")
}

batch <- function(file = "run.sh") {
  system(paste0("sbatch ", file))
}