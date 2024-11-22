#!/bin/bash
#SBATCH --job-name=lcmmtp_sim
#SBATCH --output=output_%A_%a.txt
#SBATCH --error=error_%A_%a.txt
#SBATCH --array=2400
#SBATCH --time=05:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G

module load r/4.3.2

# Parameter combinations (8 total, each repeated 300 times)
declare -a N_VALUES=(1000 5000)
declare -a U_VALUES=(-1 1)
declare -a V_VALUES=(-1 1)

# Determine the parameter set and replicate number for this task
param_index=$(( (SLURM_ARRAY_TASK_ID - 1) / 300 ))  # Each combination is repeated 300 times
replicate_num=$(( (SLURM_ARRAY_TASK_ID - 1) % 300 + 1 ))

# Compute specific `n`, `U`, and `V` values based on `param_index`
n=${N_VALUES[$((param_index / 4))]}
U=${U_VALUES[$(( (param_index / 2) % 2 ))]}
V=${V_VALUES[$(( param_index % 2 ))]}

# Run the R script with the task-specific seed index
Rscript sim.R "$n" "$U" "$V" "$SLURM_ARRAY_TASK_ID"

