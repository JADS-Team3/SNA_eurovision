library(ergm)
library(igraph)

# Load the node list
nodelist <- read.csv(file.path(getwd(), "data", "nodelist.csv"), stringsAsFactors = FALSE)
nodelist$language <- as.factor(nodelist$language)

politics_matrix <- as.matrix(read.csv(file.path(getwd(), "data", "politics_matrix.csv"), stringsAsFactors = FALSE))

# Load the edge lists
edgelist_tele <- read.csv(file.path(getwd(), "data", "edgelist_tele.csv"), stringsAsFactors = FALSE)
edgelist_jury <- read.csv(file.path(getwd(), "data", "edgelist_jury.csv"), stringsAsFactors = FALSE)


png(filename = file.path(getwd(), "pics", "ratios_histogram.png"), width = 800, height = 600)

# Create the histogram
hist(edgelist_tele$voting_ratio, breaks = 10, main = "Histogram of Voting Ratios", 
     xlab = "Voting ratio", ylab = "Frequency", col = "skyblue", border = "black")
abline(v = 0.4, col = "red", lwd = 4)

dev.off()


# Parameters
p <- 3 / 25        # Probability of top-3 vote
lambda_values <- c(0.3, 0.4, 0.5, 0.6)  # Thresholds
participations <- 2:8  # Range of participations

# Function to calculate probabilities
calculate_probability <- function(n_B, p, lambda) {
  k_min <- ceiling(lambda * n_B)  # Minimum number of votes
  # Compute P(X > k_min - 1)
  1 - pbinom(k_min - 1, n_B, p)
}

# Compute probabilities for each lambda
results <- lapply(lambda_values, function(lambda) {
  sapply(participations, calculate_probability, p = p, lambda = lambda)
})


png(filename = file.path(getwd(), "pics", "lambda_probs.png"), width = 800, height = 600)

# Set up a 2x2 grid for plotting
par(mfrow = c(2, 2))  # 2 rows, 2 columns

# Plot each lambda on a separate subplot
for (i in seq_along(lambda_values)) {
  plot(participations, results[[i]], type = "b", pch = 19, col = "blue",
       ylim = c(0, 0.4), xlab = "Number of Participations", 
       ylab = "Probability", 
       main = paste("Probability of exceeding threshold λ =", lambda_values[i]))
  grid()
}
dev.off()
par(mfrow = c(1, 1))

# Filter based on lambda
lambda = 0.4
edgelist_tele_0.4 <- edgelist_tele[edgelist_tele['voting_ratio'] >= lambda, c("from", "to")]
edgelist_jury_0.4 <- edgelist_jury[edgelist_jury['voting_ratio'] >= lambda, c("from", "to")]

lambda = 0.6
edgelist_tele_0.6 <- edgelist_tele[edgelist_tele['voting_ratio'] > lambda, c("from", "to")]
edgelist_jury_0.6 <- edgelist_jury[edgelist_jury['voting_ratio'] > lambda, c("from", "to")]

# Create the graph
igraph_network_tele_0.4 <- igraph::graph_from_data_frame(d = edgelist_tele_0.4, vertices = nodelist, directed = TRUE)
igraph_network_jury_0.4 <- igraph::graph_from_data_frame(d = edgelist_jury_0.4, vertices = nodelist, directed = TRUE)

# Convert igraph object to a network object
network_tele_0.4 <- snafun::to_network(igraph_network_tele_0.4)
network_jury_0.4 <- snafun::to_network(igraph_network_jury_0.4)

png(filename = file.path(getwd(), "pics", "network_tele_plot.png"), width = 800, height = 600)
plot(network_tele_0.4)
dev.off()
png(filename = file.path(getwd(), "pics", "network_jury_plot.png"), width = 800, height = 600)
plot(network_jury_0.4)
dev.off()

# Create a table summarizing the 'language' attribute
language_summary <- table(nodelist$language)
language_summary <- data.frame(Language = names(language_summary),
                               Count = as.numeric(language_summary),
                               Proportion = prop.table(language_summary))

png(filename = file.path(getwd(), "pics", "languages.png"), width = 1000, height = 400)
barplot(language_summary$Count ~ language_summary$Language,
        main = "Counts of Languages in the Network",
        xlab = "Language",
        ylab = "Count",
        border = "black",
        las = 1)
dev.off()

# Select the cultural dimensions columns (idv, pdi, mas, uai, ltowvs, ivr) for distance computation
cultural_data <- nodelist[, c("idv", "pdi", "mas", "uai", "ltowvs", "ivr")]

# Compute the pairwise Euclidean distances
cultural_dist_matrix <- as.matrix(dist(cultural_data))

# Scale the matrix values (optional, for normalization in ERGM)
cultural_dist_matrix_scaled <- cultural_dist_matrix / max(cultural_dist_matrix)

# Add country names as row and column names for interpretation
rownames(cultural_dist_matrix_scaled) <- nodelist$country
colnames(cultural_dist_matrix_scaled) <- nodelist$country

cultural_dist <- cultural_dist_matrix_scaled

# Define the list of countries
countries <- c("al", "am", "at", "au", "az", "be", "bg", "by", "ch", "cy", 
               "cz", "de", "dk", "ee", "es", "fi", "fr", "gb", "ge", "gr", 
               "hr", "hu", "ie", "il", "is", "it", "lt", "lv", "md", "mk", 
               "mt", "nl", "no", "pl", "pt", "ro", "rs", "ru", "se", "si", 
               "sm", "ua")

# Create an empty matrix
n <- length(countries)
neighbour_matrix <- matrix(0, nrow = n, ncol = n)
rownames(neighbour_matrix) <- countries
colnames(neighbour_matrix) <- countries

# Define neighboring relationships
neighbours <- list(
  al = c("mk", "gr"),
  am = c("ge", "tr", "az"),
  at = c("de", "cz", "sk", "hu", "si", "it", "ch"),
  au = NULL,  # Australia has no land neighbors
  az = c("am", "ge", "ru"),
  be = c("nl", "de", "fr"),
  bg = c("ro", "rs", "mk", "gr", "tr"),
  by = c("ru", "ua", "pl", "lt", "lv"),
  ch = c("de", "at", "it", "fr"),
  cy = NULL,  # Cyprus is an island
  cz = c("de", "pl", "sk", "at"),
  de = c("nl", "be", "fr", "ch", "at", "cz", "pl", "dk"),
  dk = c("se", "de"),
  ee = c("ru", "lv"),
  es = c("pt", "fr"),
  fi = c("se", "ru"),
  fr = c("be", "de", "ch", "it", "es", "gb"),
  gb = c("ie", "fr"),
  ge = c("ru", "az", "am", "tr"),
  gr = c("bg", "tr", "al", "mk"),
  hr = c("si", "hu", "rs"),
  hu = c("at", "sk", "ro", "rs", "hr", "si", "ua"),
  ie = c("gb"),
  il = NULL,
  is = NULL,  # Iceland is an island
  it = c("ch", "at", "si", "sm"),
  lt = c("lv", "pl", "by", "ru"),
  lv = c("ee", "lt", "ru", "by"),
  md = c("ro", "ua"),
  mk = c("gr", "al", "bg", "rs"),
  mt = NULL,  # Malta is an island
  nl = c("de", "be"),
  no = c("se", "fi", "ru"),
  pl = c("de", "cz", "sk", "ua", "by", "lt", "ru"),
  pt = c("es"),
  ro = c("hu", "rs", "bg", "ua", "md"),
  rs = c("hu", "hr", "bg", "mk", "ro"),
  ru = c("ua", "pl", "by", "fi", "ee", "lv", "lt", "ge", "az", "no"),
  se = c("no", "fi", "dk"),
  si = c("hr", "at", "hu", "it"),
  sm = c("it"),
  ua = c("ru", "by", "pl", "hu", "ro", "md", "sk")
)

# Fill the matrix based on neighboring relationships
for (country in names(neighbours)) {
  if (!is.null(neighbours[[country]])) {
    for (neighbour in neighbours[[country]]) {
      country_idx <- which(countries == country)
      neighbour_idx <- which(countries == neighbour)
      neighbour_matrix[country_idx, neighbour_idx] <- 1
      neighbour_matrix[neighbour_idx, country_idx] <- 1
    }
  }
}

model_tele_0.4_01 <- ergm::ergm(
  network_tele_0.4 ~ edges + 
    nodematch("language"),
  control = ergm::control.ergm(
    MCMC.burnin = 7000,
    MCMC.samplesize = 20000,
    seed = 1234,
    MCMLE.maxit = 40,
    parallel = 8,
    parallel.type = "PSOCK"
  )
) 

model_tele_0.4_02 <- ergm::ergm(
  network_tele_0.4 ~ edges + 
    nodematch("language") + 
    edgecov(cultural_dist),
  control = ergm::control.ergm(
    MCMC.burnin = 7000,
    MCMC.samplesize = 20000,
    seed = 1234,
    MCMLE.maxit = 40,
    parallel = 8,
    parallel.type = "PSOCK"
  )
)

model_tele_0.4_03 <- ergm::ergm(
  network_tele_0.4 ~ edges + 
    nodematch("language") + 
    edgecov(cultural_dist) +
    edgecov(neighbour_matrix),
  control = ergm::control.ergm(
    MCMC.burnin = 7000,
    MCMC.samplesize = 20000,
    seed = 1234,
    MCMLE.maxit = 40,
    parallel = 8,
    parallel.type = "PSOCK"
  )
)

model_tele_0.4_04 <- ergm::ergm(
  network_tele_0.4 ~ edges + 
    nodematch("language") + 
    edgecov(cultural_dist) +
    edgecov(neighbour_matrix) +
    edgecov(politics_matrix),
  control = ergm::control.ergm(
    MCMC.burnin = 7000,
    MCMC.samplesize = 20000,
    seed = 1234,
    MCMLE.maxit = 40,
    parallel = 8,
    parallel.type = "PSOCK"
  )
)

texreg::screenreg(list(model_tele_0.4_01, model_tele_0.4_02, model_tele_0.4_03, model_tele_0.4_04))


model_tele_0.4 <- ergm::ergm(
  network_tele_0.4 ~ edges + 
    mutual +
    gwesp(0.75, fixed = TRUE, type = "OSP") + 
    gwidegree(0.25, fixed = TRUE) +
    gwodegree(0.5, fixed = TRUE) +
    nodematch("language") +
    edgecov(cultural_dist) +
    edgecov(neighbour_matrix),
  control = ergm::control.ergm(
    MCMC.burnin = 7000,
    MCMC.samplesize = 20000,
    seed = 1234,
    MCMLE.maxit = 40,
    parallel = 8,
    parallel.type = "PSOCK"
  )
)


model_jury_0.4 <- ergm::ergm(
  network_jury_0.4 ~ edges + 
    gwidegree(0.5, fixed = TRUE) + 
    mutual +
    nodematch("language") + 
    edgecov(cultural_dist_matrix_scaled) +
    edgecov(neighbour_matrix),
  control = ergm::control.ergm(
    MCMC.burnin = 7000,
    MCMC.samplesize = 20000,
    seed = 1234,
    MCMLE.maxit = 20,
    parallel = 8,
    parallel.type = "PSOCK"
  )
)

igraph_network_tele_0.6 <- igraph::graph_from_data_frame(d = edgelist_tele_0.6, vertices = nodelist, directed = TRUE)
network_tele_0.6 <- snafun::to_network(igraph_network_tele_0.6)

model_tele_0.6 <- ergm::ergm(
  network_tele_0.6 ~ edges + 
    mutual +
    gwesp(0.75, fixed = TRUE, type = "OSP") + 
    gwidegree(0.25, fixed = TRUE) +
    gwodegree(0.5, fixed = TRUE) +
    nodematch("language") +
    edgecov(cultural_dist) +
    edgecov(neighbour_matrix),
  control = ergm::control.ergm(
    MCMC.burnin = 7000,
    MCMC.samplesize = 20000,
    seed = 1234,
    MCMLE.maxit = 40,
    parallel = 8,
    parallel.type = "PSOCK"
  )
)

export_ergm_diagnostics <- function(ergm_model, output_prefix) {
  # Load required libraries
  if (!requireNamespace("ergm", quietly = TRUE)) stop("The 'ergm' package is required but not installed.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("The 'ggplot2' package is required but not installed.")
  if (!requireNamespace("snafun", quietly = TRUE)) stop("The 'snafun' package is required but not installed.")
  
  # Create output directory
  output_dir <- "ergm_results"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Create MCMC diagnostics plots
  mcmc_diag_file <- file.path(output_dir, paste0(output_prefix, "_mcmc_diagnostics.pdf"))
  pdf(mcmc_diag_file)
  ergm::mcmc.diagnostics(ergm_model)
  dev.off()
  cat("MCMC diagnostics saved to", mcmc_diag_file, "\n")
  
  # Compute GOF and create GOF plots
  gof_results <- ergm::gof(ergm_model)
  gof_plot_file <- file.path(output_dir, paste0(output_prefix, "_gof_plots.pdf"))
  pdf(gof_plot_file)
  snafun::stat_plot_gof(gof_results)
  dev.off()
  cat("GOF plots saved to", gof_plot_file, "\n")
  
  # Save summary of the ERGM model to a text file
  summary_file <- file.path(output_dir, paste0(output_prefix, "_summary.txt"))
  summary_content <- capture.output(summary(ergm_model))
  writeLines(summary_content, summary_file)
  cat("Model summary saved to", summary_file, "\n")
  
  cat("All diagnostics exported successfully!\n")
}

export_ergm_diagnostics(model_tele_0.4, "tele")
export_ergm_diagnostics(model_jury_0.4, "jury")
export_ergm_diagnostics(model_tele_0.6, "tele06")
