
library(gtools)

# Function to generate and save multiple landscapes
generate_and_save_multiple_landscapes <- function(N, K, num_landscapes = 100, folder = "NK_LandscapesN10K8") {
  # Ensure the folder exists, if not, create it
  if (!dir.exists(folder)) {
    dir.create(folder)
  }
  
  for (i in 1:num_landscapes) {
    # Generate Landscape
    LS <- permutations(2, N, v = c(0, 1), repeats.allowed = TRUE)
    LS <- as.data.frame(LS)
    
    if (K == 0) {
      depends <- as.vector(1:N)
      values <- replicate(N, round(runif(2, 0, 1), 1))
      fitness <- values
    } else {
      depends <- matrix(1:N, nrow = K + 1, ncol = N)
      for (j in 1:N) {
        for (k in 2:(K + 1)) {
          repeat {
            available_values <- setdiff(1:N, c(j, depends[1:(k-1), j]))
            sampled_value <- sample(available_values, 1)
            if (!sampled_value %in% depends[1:(k-1), j]) {
              depends[k, j] <- sampled_value
              break
            }
          }
        }
      }
      combinations <- permutations(2, K + 1, v = c(0, 1), repeats.allowed = TRUE)
      values <- replicate(N, round(runif(nrow(combinations), 0, 1), 1))
      fitness <- cbind(combinations, values)
    }
    
    landscape <- generate_landscape(N, K, LS, fitness, depends)
    
    # Construct unique filenames for each landscape, fitness, and depends data
    landscape_filename <- sprintf("%s/landscape_N%d_K%d_%d.csv", folder, N, K, i)
    fitness_filename <- sprintf("%s/fitness_N%d_K%d_%d.csv", folder, N, K, i)
    depends_filename <- sprintf("%s/depends_N%d_K%d_%d.csv", folder, N, K, i)
    
    # Save data to CSV files
    write.csv(landscape, landscape_filename, row.names = FALSE)
  }
}

# Helper function to generate the landscape
generate_landscape <- function(N, K, LS, fitness, depends) {
  fitness_scores <- vector("numeric", length = nrow(LS))
  
  if (K == 0) {
    for (ag in 1:nrow(LS)) {
      rows <- as.numeric(LS[ag, ]) + 1
      values <- sapply(1:N, function(y) fitness[rows[y], y])
      fitness_scores[ag] <- mean(values)
    }
  } else {
    indx1 <- do.call(paste0, as.data.frame(fitness[, 1:(K + 1)]))
    indx2 <- sapply(1:N, function(y) do.call(paste0, as.data.frame(LS[, depends[, y]])))
    fitness_scores <- sapply(1:nrow(LS), function(o) mean(diag(sapply(indx2[o, ], function(x) fitness[which(indx1 == x), (K + 2):ncol(fitness)]))))
  }
  
  landscape <- cbind(LS[, 1:N], scale_fitness = (fitness_scores / max(fitness_scores))^8)
  return(landscape)
}

generate_and_save_multiple_landscapes(10,8)

generate_and_save_landscapes(4, 2, 2)



