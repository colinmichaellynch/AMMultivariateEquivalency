# --- Clean environment & set working directory -------------------------------
rm(list = ls())

# Set to your project folder OR comment this out if you use a project file.
# setwd("~/your/directory")

# --- Source only the functions actually used ---------------------------------
source("multivariateProcessStability.R")
source("binLimits.R")
source("multivariateEquivalency.R")

# --- User-tunable parameter --------------------------------------------------
b <- 8  # number of bins for discretization in equivalency test

# --- 1) Import reference data ------------------------------------------------
ref_path <- "referenceProcess.csv"
referenceProcess <- read.csv(ref_path, header = TRUE, check.names = FALSE)

# --- 2) Verify reference process stability -----------------------------------
outOfControlCount <- multivariateProcessStability(referenceProcess)
print(outOfControlCount)

if (is.null(outOfControlCount$num_out_of_control)) {
  stop("multivariateProcessStability() must return a list with num_out_of_control.", call. = FALSE)
}

if (outOfControlCount$num_out_of_control > 0) {
  stop("Reference process is NOT stable. Do not continue; fix the reference process first.", call. = FALSE)
} else {
  message("Reference process appears stable. Proceeding…")
}

# --- 3) Compute reference bin limits -----------------------------------------
referenceLimits <- binLimits(referenceProcess, b)
if (is.null(referenceLimits)) stop("binLimits() returned NULL.", call. = FALSE)

# --- 4) Test candidate datasets ----------------------------------------------
# Example candidates (rename/replace these paths as needed)
cand_eq_path  <- "candidateProcessEquivalent.csv"
cand_neq_path <- "candidateProcessNotEquivalent.csv"

for (cand_path in c(cand_eq_path, cand_neq_path)) {
  if (!file.exists(cand_path)) {
    message("Skipping missing candidate file: ", cand_path)
    next
  }
  
  candidateProcess <- read.csv(cand_path, header = TRUE, check.names = FALSE)
  
  # Sanity checks
  if (!all(vapply(candidateProcess, is.numeric, logical(1)))) {
    stop(paste0("All analyzed columns in ", cand_path, " must be numeric."), call. = FALSE)
  }
  if (ncol(candidateProcess) != ncol(referenceProcess)) {
    stop(paste0("Column mismatch: ", cand_path,
                " has ", ncol(candidateProcess), " columns vs ",
                ncol(referenceProcess), " in reference."), call. = FALSE)
  }
  
  # Run test
  eq_res <- multivariateEquivalency(referenceLimits, candidateProcess, b)
  
  # Expect a list/data.frame with a field named 'equivalent'
  if (is.null(eq_res$equivalent)) {
    stop("multivariateEquivalency() should return an object with $equivalent.", call. = FALSE)
  }
  
  # Print concise verdict
  verdict <- as.character(eq_res$equivalent[1])
  message(sprintf("Candidate file: %s --> Equivalent? %s", cand_path, verdict))
}

message("Done.")
