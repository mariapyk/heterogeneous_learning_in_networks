



library(data.table)
library(ggplot2)
library(dplyr)

conflicts()
library(tidyr)
library(patchwork)
library(purrr)
library(stringr)  
library(ggtext)


################################################

raw_data <- fread("results.csv") 


raw_data[, strategy_name := case_when(
  strategy == 1 ~ "Hill-climb",
  strategy == 2 ~ "Payoff-bias", 
  strategy == 3 ~ "Frequency-bias",
  TRUE ~ paste0("Strategy ", strategy)
)]


raw_data <- raw_data %>%
  mutate(
    density_formatted = case_when(
      round(actual_density, 1) == 0.2 ~ "~0.2",
      round(actual_density, 1) == 0.9 ~ "~0.9",
      TRUE ~ paste0("~", round(actual_density, 1))
    )
  )

# Rename landscape_type values
raw_data[, landscape_type := case_when(
  landscape_type == "K0" ~ "Simple Landscape",
  landscape_type == "K8" ~ "Complex Landscape",
  TRUE ~ landscape_type  # Keep any other values unchanged
)]


raw_data_mu_lower <-  raw_data %>% filter(mu_value == 0.01)

plot_data_individual <- raw_data_mu_lower[gen %in% plot_generations]

mean_data <- raw_data_mu_lower[, .(
  mean_proportion = mean(mean_proportion),
  sd_proportion = sd(mean_proportion),  # SD across simulations
  n_sims = .N,  # Number of simulations
  se = sd(mean_proportion) / sqrt(.N),  # Standard error
  ci_lower = mean(mean_proportion) - 1.96 * sd(mean_proportion) / sqrt(.N),
  ci_upper = mean(mean_proportion) + 1.96 * sd(mean_proportion) / sqrt(.N)
), by = .(gen, strategy_name, landscape_type, mu_value, n_agents, density_formatted, network_type)]


max_gen <- max(mean_data$gen)

plot_generations <- c(1, seq(100, max_gen, 100), max_gen)

plot_data_mean <- mean_data[gen %in% plot_generations]


plot_data_individual$strategy_name <- factor(plot_data_individual$strategy_name, 
                                             levels = c("Hill-climb", "Payoff-bias", "Frequency-bias"))

plot_data_mean$strategy_name <- factor(plot_data_mean$strategy_name, 
                                       levels = c("Hill-climb", "Payoff-bias", "Frequency-bias"))

max_gen <- max(plot_data_mean$gen)

final_gen_data <- plot_data_mean %>% 
  filter(gen == max_gen) %>%
  # Convert strategy_name to character to avoid factor issues
  mutate(strategy_name = as.character(strategy_name)) %>%
  # Remove any rows with missing values
  filter(!is.na(mean_proportion), !is.na(ci_lower), !is.na(ci_upper))

lattice_final <- final_gen_data %>%
  filter(network_type == "regular") %>%
  mutate(
    landscape_type = factor(landscape_type, levels = c("Simple Landscape", "Complex Landscape")),
    strategy_name = factor(strategy_name, levels = c("Hill-climb", "Payoff-bias", "Frequency-bias")),
    row_label = paste0("*S* = ", n_agents, " & *D* ", density_formatted)
  )


p_bar_lattice <- ggplot(lattice_final, aes(x = strategy_name, y = mean_proportion, fill = strategy_name)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25, linewidth = 0.8) +
  
  facet_grid(row_label ~ landscape_type,              
             labeller = labeller(landscape_type = c("Simple Landscape" = "Simple Landscape", "Complex Landscape" = "Complex Landscape"))) +
  
  scale_fill_manual(
    values = c("Hill-climb" = "#FFD700", 
               "Payoff-bias" = "#FF6B6B", 
               "Frequency-bias" = "#4ECDC4"),
    breaks = c("Hill-climb", "Payoff-bias", "Frequency-bias"), 
    name = "Strategy"
  ) +
  
  scale_x_discrete(limits = c("Hill-climb", "Payoff-bias", "Frequency-bias"), 
                   labels = c("Simple" = "Simple Landscape", 
                              "Complex" = "Complex Landscape")) +
  
  labs(
    title = "",
    x = "",  # Remove x-axis title
    y = "Mean Proportion"   # Remove y-axis title from right plot
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),    # Remove x-axis text
    axis.ticks.x = element_blank(),   # Remove x-axis ticks
    axis.title.y = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    strip.text = element_markdown(size = 12),  # Changed to element_markdown
    strip.text.y = element_markdown(angle = 0),  # Changed to element_markdown
    legend.position = "bottom", 
    legend.text = element_text(size = 12),   # ADD THIS - legend labels (Hill-climb, etc.)
    legend.title = element_text(size = 12) 
  ) +
  ylim(0, 1)

p_bar_lattice

ggsave("final_bar_plot_clean_lattice_only_revision.png", 
       plot = p_bar_lattice,
       width = 6, height = 6, 
       dpi = 300, 
       units = "in")



random_final <- final_gen_data %>%
  filter(network_type == "irregular") %>%
  mutate(
    landscape_type = factor(landscape_type, levels = c("Simple Landscape", "Complex Landscape")),
    strategy_name = factor(strategy_name, levels = c("Hill-climb", "Payoff-bias", "Frequency-bias")),
    row_label = paste0("*S* = ", n_agents, " & *D* ", density_formatted)
  )


p_bar_random_final <- ggplot(random_final, aes(x = strategy_name, y = mean_proportion, fill = strategy_name)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.25, linewidth = 0.8) +
  
  facet_grid(row_label ~ landscape_type,              
             labeller = labeller(landscape_type = c("Simple Landscape" = "Simple Landscape", "Complex Landscape" = "Complex Landscape"))) +
  
  scale_fill_manual(
    values = c("Hill-climb" = "#FFD700", 
               "Payoff-bias" = "#FF6B6B", 
               "Frequency-bias" = "#4ECDC4"),
    breaks = c("Hill-climb", "Payoff-bias", "Frequency-bias"), 
    name = "Strategy"
  ) +
  
  scale_x_discrete(limits = c("Hill-climb", "Payoff-bias", "Frequency-bias"), 
                   labels = c("Simple" = "Simple Landscape", 
                              "Complex" = "Complex Landscape")) +
  
  labs(
    title = "",
    x = "",  # Remove x-axis title
    y = "Mean Proportion"   # Remove y-axis title from right plot
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),    # Remove x-axis text
    axis.ticks.x = element_blank(),   # Remove x-axis ticks
    axis.title.y = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    strip.text = element_markdown(size = 12),  # Changed to element_markdown
    strip.text.y = element_markdown(angle = 0),  # Changed to element_markdown
    legend.position = "bottom", 
    legend.text = element_text(size = 12),   # ADD THIS - legend labels (Hill-climb, etc.)
    legend.title = element_text(size = 12) 
  ) +
  ylim(0, 1)

p_bar_random_final

ggsave("final_bar_plot_clean_random_only_revision.png", 
       plot = p_bar_lattice,
       width = 6, height = 6, 
       dpi = 300, 
       units = "in")


######### Robustness figures

plot_data_mean <- raw_data[, .(
  mean_proportion = mean(mean_proportion),
  sd_proportion = sd(mean_proportion),
  n_sims = .N,
  se = sd(mean_proportion) / sqrt(.N),
  ci_lower = mean(mean_proportion) - 1.96 * sd(mean_proportion) / sqrt(.N),
  ci_upper = mean(mean_proportion) + 1.96 * sd(mean_proportion) / sqrt(.N)
), by = .(gen, strategy_name, landscape_type, mu_value, n_agents, density_formatted, network_type)]

# IRREGULAR NETWORKS with ribbon
random_individual <- plot_data_individual %>%
  filter(network_type == "Irregular network") %>%
  mutate(
    landscape_type = factor(landscape_type,
                            levels = c("Simple", "Complex"),
                            labels = c("Simple", "Complex")),
    row_label = paste0("*S*=", n_agents, " & *D*=", density_formatted),
    mu_label = paste0("μ = ", mu_value)
  )

random_mean <- plot_data_mean %>%
  filter(network_type == "Irregular network") %>%
  mutate(
    landscape_type = factor(landscape_type,
                            levels = c("Simple", "Complex"),
                            labels = c("Simple", "Complex")),
    row_label = paste0("*S*=", n_agents, " & *D*=", density_formatted),
    mu_label = paste0("μ = ", mu_value)
  )

p_random <- ggplot() +
  geom_line(data = random_individual,
            aes(x = gen, y = mean_proportion,
                color = strategy_name,
                group = interaction(strategy_name, network_instance, landscape_code)),
            alpha = 0.1, linewidth = 0.2) +
  geom_ribbon(data = random_mean,
              aes(x = gen, ymin = ci_lower, ymax = ci_upper,
                  fill = strategy_name,
                  group = interaction(strategy_name, density_formatted)),
              alpha = 0.2) +
  geom_line(data = random_mean,
            aes(x = gen, y = mean_proportion, color = strategy_name,
                group = interaction(strategy_name, density_formatted)),
            linewidth = 1, alpha = 0.9) +
  facet_grid(row_label ~ landscape_type + mu_label) +
  scale_x_continuous(breaks = c(0, 1000, 2000)) +
  scale_color_manual(values = c("Hill-climb" = "#FFD700", 
                                "Payoff-bias" = "#FF6B6B", 
                                "Frequency-bias" = "#4ECDC4"),
                     name = "Strategy") +
  scale_fill_manual(values = c("Hill-climb" = "#FFD700", 
                               "Payoff-bias" = "#FF6B6B", 
                               "Frequency-bias" = "#4ECDC4"),
                    name = "Strategy") +
  guides(fill = "none") +
  labs(title = "Irregular Networks",
       x = "Generation",
       y = "Mean Proportion") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28, hjust = 0.5),
    strip.text.y = element_markdown(angle = 0, size = 28),
    strip.text.x = element_text(size = 28),
    legend.position = "bottom",
    legend.text = element_text(size = 28),
    legend.title = element_text(size = 28),
    axis.text = element_text(size = 28),
    axis.title = element_text(size = 28),
    panel.spacing.x = unit(0.3, "cm"),
    panel.spacing.y = unit(0.3, "cm")
  ) +
  ylim(0, 1)

# REGULAR LATTICES
lattice_individual <- plot_data_individual %>%
  filter(network_type == "Regular Lattice") %>%
  mutate(
    landscape_type = factor(landscape_type,
                            levels = c("Simple", "Complex"),
                            labels = c("Simple", "Complex")),
    row_label = paste0("*S*=", n_agents, " & *D*=", density_formatted),
    mu_label = paste0("μ = ", mu_value)
  )

lattice_mean <- plot_data_mean %>%
  filter(network_type == "Regular Lattice") %>%
  mutate(
    landscape_type = factor(landscape_type,
                            levels = c("Simple", "Complex"),
                            labels = c("Simple", "Complex")),
    row_label = paste0("*S*=", n_agents, " & *D*=", density_formatted),
    mu_label = paste0("μ = ", mu_value)
  )

p_lattice <- ggplot() +
  geom_line(data = lattice_individual,
            aes(x = gen, y = mean_proportion,
                color = strategy_name,
                group = interaction(strategy_name, network_instance, landscape_code)),
            alpha = 0.1, linewidth = 0.2) +
  geom_ribbon(data = lattice_mean,
              aes(x = gen, ymin = ci_lower, ymax = ci_upper,
                  fill = strategy_name,
                  group = interaction(strategy_name, density_formatted)),
              alpha = 0.2) +
  geom_line(data = lattice_mean,
            aes(x = gen, y = mean_proportion, color = strategy_name,
                group = interaction(strategy_name, density_formatted)),
            linewidth = 1, alpha = 0.9) +
  facet_grid(row_label ~ landscape_type + mu_label) +
  scale_x_continuous(breaks = c(0, 1000, 2000)) +
  scale_color_manual(values = c("Hill-climb" = "#FFD700", 
                                "Payoff-bias" = "#FF6B6B", 
                                "Frequency-bias" = "#4ECDC4"),
                     name = "Strategy") +
  scale_fill_manual(values = c("Hill-climb" = "#FFD700", 
                               "Payoff-bias" = "#FF6B6B", 
                               "Frequency-bias" = "#4ECDC4"),
                    name = "Strategy") +
  guides(fill = "none") +
  labs(title = "Regular Lattices",
       x = "Generation",
       y = "Mean Proportion") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28, hjust = 0.5),
    legend.position = "none",
    strip.text.y = element_markdown(angle = 0, size = 28),
    strip.text.x = element_text(size = 28),
    axis.text = element_text(size = 28),
    axis.title = element_text(size = 28),
    legend.text = element_text(size = 28),
    legend.title = element_text(size = 28),
    panel.spacing.x = unit(0.3, "cm"),
    panel.spacing.y = unit(0.3, "cm")
  ) +
  ylim(0, 1)

# COMBINE PLOTS
combined_plot <- p_random / p_lattice +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


combined_plot


ggsave("line_plot_revision_robustness.png",
       plot = combined_plot,
       width = 18, height = 16,
       dpi = 300,
       units = "in")


# Calculate mean data with CI across simulations for all mu values
mean_data <- raw_data[, .(
  mean_proportion = mean(mean_proportion),
  sd_proportion = sd(mean_proportion),  # SD across simulations
  n_sims = .N,  # Number of simulations
  se = sd(mean_proportion) / sqrt(.N),  # Standard error
  ci_lower = mean(mean_proportion) - 1.96 * sd(mean_proportion) / sqrt(.N),
  ci_upper = mean(mean_proportion) + 1.96 * sd(mean_proportion) / sqrt(.N)
), by = .(gen, strategy_name, landscape_type, mu_value, n_agents, density_formatted, network_type)]

# Get final generation and filter for mu 0.01 and 0.1
max_gen <- max(mean_data$gen)

final_gen_data <- mean_data %>% 
  filter(gen == max_gen, mu_value %in% c(0.01, 0.1, 0.001)) %>%
  mutate(strategy_name = as.character(strategy_name)) %>%
  filter(!is.na(mean_proportion), !is.na(ci_lower), !is.na(ci_upper))

# Set factor levels
final_gen_data$strategy_name <- factor(
  final_gen_data$strategy_name,
  levels = c("Hill-climb", "Payoff-bias", "Frequency-bias")
)

# Create labels
final_gen_data <- final_gen_data %>%
  mutate(
    landscape_type = factor(landscape_type,
                            levels = c("Simple", "Complex"),
                            labels = c("Simple", "Complex")),
    row_label = paste0("*S*=", n_agents, " & *D*=", density_formatted),
    mu_label = paste0("μ = ", mu_value)
  )


# Create the bar plot
p_bar <- ggplot(final_gen_data, 
                aes(x = strategy_name, y = mean_proportion, fill = strategy_name)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.3, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.25, 
                linewidth = 0.8,
                color = "black") +
  facet_grid(network_type + row_label ~ landscape_type + mu_label) +
  scale_fill_manual(values = c("Hill-climb" = "#FFD700", 
                               "Payoff-bias" = "#FF6B6B", 
                               "Frequency-bias" = "#4ECDC4"),
                    name = "Strategy") +
  labs(
    title = paste0("Final Strategy Proportions at Generation ", max_gen),
    subtitle = "Mean ± 95% CI across simulations",
    x = "",
    y = "Mean Proportion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28, hjust = 0.5),
    plot.subtitle = element_text(size = 28, hjust = 0.5, color = "gray60"),
    strip.text.y = element_markdown(angle = 0, size = 28),
    strip.text.x = element_text(size = 28),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 28),
    axis.title = element_text(size = 28),
    legend.text = element_text(size = 28),
    legend.title = element_text(size = 28),
    panel.spacing.x = unit(0.5, "cm"),
    panel.spacing.y = unit(0.3, "cm")
  ) +
  ylim(0, 1)

print(p_bar)

ggsave("robustness_bar_plot_revision.png",
       plot = p_bar,
       width = 20, height = 18,
       dpi = 300,
       units = "in")


### ANALYSIS 

library(dplyr)
library(lm.beta)  # For standardized coefficients
library(broom)

# Get final generation data with mu = 0.01
max_gen <- max(raw_data$gen)
final_gen <- raw_data %>% 
  filter(gen == max_gen, mu_value == 0.01)

# Function to run standardized regression
run_standardized_regression <- function(data, strat_num, net_type) {
  
  # Filter data
  analysis_data <- data %>%
    filter(network_type == net_type, strategy == strat_num) %>%
    mutate(
      complex_landscape = ifelse(landscape_type == "Complex", 1, 0),
      high_density = ifelse(actual_density > 0.5, 1, 0),
      large_population = ifelse(n_agents == 19, 1, 0)
    )
  
  cat(paste("Processing: Strategy", strat_num, "-", net_type, "- Rows:", nrow(analysis_data), "\n"))
  
  # Run full model
  model <- lm(mean_proportion ~ complex_landscape + high_density + large_population, 
              data = analysis_data)
  
  # Standardize for standardized coefficients
  std_data <- analysis_data %>%
    mutate(
      y = scale(mean_proportion)[,1],
      complexity = scale(complex_landscape)[,1],
      density = scale(high_density)[,1],
      size = scale(large_population)[,1]
    )
  
  std_model <- lm(y ~ complexity + density + size, data = std_data)
  
  # Get coefficients and CIs
  ci_table <- confint(std_model)
  coefs <- coef(std_model)
  r_squared <- summary(std_model)$r.squared
  
  # Map strategy number to name
  strategy_name <- case_when(
    strat_num == 1 ~ "Hill-climb",
    strat_num == 2 ~ "Payoff-bias",
    strat_num == 3 ~ "Frequency-bias"
  )
  
  # Return results
  data.frame(
    Strategy = strategy_name,
    Network_Type = net_type,
    Complexity_beta = coefs[2],
    Complexity_CI_lower = ci_table[2, 1],
    Complexity_CI_upper = ci_table[2, 2],
    Density_beta = coefs[3],
    Density_CI_lower = ci_table[3, 1],
    Density_CI_upper = ci_table[3, 2],
    Size_beta = coefs[4],
    Size_CI_lower = ci_table[4, 1],
    Size_CI_upper = ci_table[4, 2],
    R_squared = r_squared
  )
}

# Run for all combinations
strategies <- c(1, 2, 3)  # Use numeric strategy codes
networks <- c("Irregular network", "Regular Lattice")

all_results <- list()
for (strat in strategies) {
  for (net in networks) {
    result <- run_standardized_regression(final_gen, strat, net)
    all_results[[paste(strat, net)]] <- result
  }
}

# Combine and format results
combined_results <- bind_rows(all_results)

final_table <- combined_results %>%
  mutate(
    Network_Type = ifelse(Network_Type == "Irregular network", "Random", "Lattice"),
    Complexity = sprintf("%.3f [%.3f, %.3f]", 
                         Complexity_beta, Complexity_CI_lower, Complexity_CI_upper),
    Density = sprintf("%.3f [%.3f, %.3f]", 
                      Density_beta, Density_CI_lower, Density_CI_upper),
    `Group Size` = sprintf("%.3f [%.3f, %.3f]", 
                           Size_beta, Size_CI_lower, Size_CI_upper),
    `R squared` = sprintf("%.2e", R_squared)
  ) %>%
  select(Strategy, `Network Type` = Network_Type, Complexity, Density, `Group Size`, `R squared`) %>%
  as.data.frame() 

# Remove row names
row.names(final_table) <- NULL

# create the LaTeX table
library(kableExtra)

latex_table <- final_table %>%
  kable(format = "latex", 
        booktabs = TRUE,
        escape = FALSE,
        row.names = FALSE,
        caption = "Standardized Regression Coefficients ($\\beta$) with 95\\% CI. Each row shows results from a multiple linear regression where problem complexity (Simple vs Complex), network density D (Low vs High), and group size S (10 vs 19 agents) simultaneously predict the proportion of agents using the strategy at the final generation. Coefficients are standardized ($\\beta$) showing change in standard deviation units. Values shown as $\\beta$ [95\\% CI]. $R^2$ indicates variance explained.",
        label = "tab:regression_results")

latex_table <- final_table %>%
  kable(format = "latex", 
        booktabs = TRUE,
        escape = FALSE,
        row.names = FALSE,
        caption = "Standardized Regression Coefficients ($\\beta$) with 95\\% CI. Each row shows results from a multiple linear regression where problem complexity (Simple vs Complex), network density D (Low vs High), and group size S (10 vs 19 agents) simultaneously predict the proportion of agents using the strategy at the final generation. Coefficients are standardized ($\\beta$) showing change in standard deviation units. Values shown as $\\beta$ [95\\% CI]. $R^2$ indicates variance explained.",
        label = "tab:regression_results") %>%
  kable_styling(latex_options = c("scale_down")) 


# Save to file
writeLines(latex_table, "table2_regression_revision.tex")
cat(latex_table)


### Interactions table: 

# Get final generation data with mu = 0.01
max_gen <- max(raw_data$gen)
final_gen <- raw_data %>% 
  filter(gen == max_gen, mu_value == 0.01)

# Filter for lattice networks only
lattice_data <- final_gen %>%
  filter(network_type == "Regular Lattice") %>%
  mutate(
    complex_landscape = ifelse(landscape_type == "Complex", 1, 0),
    high_density = ifelse(actual_density > 0.5, 1, 0),
    large_population = ifelse(n_agents == 19, 1, 0)
  )

# Function to run interaction models and extract standardized coefficients
run_interaction_model <- function(data, strat_num) {
  
  # Filter for this strategy
  strat_data <- data %>% filter(strategy == strat_num)
  
  # Map strategy number to name
  strategy_name <- case_when(
    strat_num == 1 ~ "Hill-climb",
    strat_num == 2 ~ "Payoff-bias",
    strat_num == 3 ~ "Frequency-bias"
  )
  
  cat(paste("\n=== Processing:", strategy_name, "===\n"))
  cat(paste("Sample size:", nrow(strat_data), "\n"))
  
  # Run the full interaction model
  full_model <- lm(mean_proportion ~ complex_landscape * high_density * large_population,
                   data = strat_data)
  
  # Standardize all variables for standardized coefficients
  std_data <- strat_data %>%
    mutate(
      y = scale(mean_proportion)[,1],
      complexity = scale(complex_landscape)[,1],
      density = scale(high_density)[,1],
      size = scale(large_population)[,1]
    )
  
  # standardized model with interactions
  std_model <- lm(y ~ complexity * density * size, data = std_data)
  
  # coefficients and confidence intervals
  coefs <- coef(std_model)
  ci_table <- confint(std_model)
  
  # R-squared
  r_squared <- summary(std_model)$r.squared
  
  # Create results data frame
  results <- data.frame(
    Strategy = strategy_name,
    Term = names(coefs),
    Beta = coefs,
    CI_lower = ci_table[, 1],
    CI_upper = ci_table[, 2],
    R_squared = r_squared
  )
  
  results <- results %>%
    filter(Term != "(Intercept)") %>%
    mutate(
      Term = recode(Term,
                    "complexity" = "Complexity",
                    "density" = "Density",
                    "size" = "Group Size",
                    "complexity:density" = "Complexity × Density",
                    "complexity:size" = "Complexity × Group Size",
                    "density:size" = "Density × Group Size",
                    "complexity:density:size" = "Complexity × Density × Group Size"),
      `β [95% CI]` = sprintf("%.3f [%.3f, %.3f]", Beta, CI_lower, CI_upper)
    ) %>%
    select(Strategy, Term, `β [95% CI]`, R_squared)
  
  return(results)
}

# Run for all three strategies
all_interaction_results <- list()
for (strat in 1:3) {
  all_interaction_results[[strat]] <- run_interaction_model(lattice_data, strat)
}

# Combine results
combined_interactions <- bind_rows(all_interaction_results)

# Create a wide-format table for display
interaction_table <- combined_interactions %>%
  group_by(Strategy) %>%
  mutate(
    R_squared = sprintf("%.2e", first(R_squared))
  ) %>%
  ungroup() %>%
  select(Strategy, Term, `β [95% CI]`, R_squared)


# Create LaTeX table
latex_interaction <- interaction_table %>%
  kable(format = "latex", 
        booktabs = TRUE,
        escape = FALSE,
        row.names = FALSE,
        label = "tab:interaction_results") %>%
  kable_styling(font_size = 8,
                latex_options = c("hold_position", "scale_down"))

# Save to file
writeLines(latex_interaction, "table_interaction_results_revision.tex")


## bar plot showing simple vs complex comparisons: 

mean_data <- raw_data_mu_lower[, .(
  mean_proportion = mean(mean_proportion),
  sd_proportion = sd(mean_proportion),  # SD across simulations
  n_sims = .N,  # Number of simulations
  se = sd(mean_proportion) / sqrt(.N),  # Standard error
  ci_lower = mean(mean_proportion) - 1.96 * sd(mean_proportion) / sqrt(.N),
  ci_upper = mean(mean_proportion) + 1.96 * sd(mean_proportion) / sqrt(.N)
), by = .(gen, strategy_name, landscape_type, mu_value, n_agents, density_formatted, network_type)]

# Filter for plotting (both mean data and individual runs)
max_gen <- max(mean_data$gen)

plot_generations <- c(1, seq(100, max_gen, 100), max_gen)

# Filter mean data
plot_data_mean <- mean_data[gen %in% plot_generations]

max_gen <- max(plot_data_mean$gen)

final_gen_data <- plot_data_mean %>% 
  filter(gen == max_gen) %>%
  mutate(strategy_name = as.character(strategy_name)) %>%
  filter(!is.na(mean_proportion), !is.na(ci_lower), !is.na(ci_upper))

all_strategies_data <- final_gen_data %>%
  filter(network_type == "Regular Lattice") %>%
  mutate(
    landscape_type = factor(landscape_type, levels = c("Simple", "Complex")),
    strategy_name = factor(strategy_name, levels = c("Hill-climb", "Payoff-bias", "Frequency-bias")),
    row_label = paste0("*S* = ", n_agents, " & *D* ", density_formatted),
    condition = paste0("S=", n_agents, "\nD", density_formatted)
  )

# Create separate facets for each strategy with landscape comparisons
strategies_separate_facets <- ggplot(all_strategies_data, aes(x = condition, y = mean_proportion, fill = landscape_type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(width = 0.7), width = 0.25, linewidth = 0.8) +
  
  # Each strategy gets its own facet
  facet_wrap(~ strategy_name, scales = "free_y", ncol = 3) +
  
  scale_fill_manual(
    values = c("Simple" = "#BBDEFB", "Complex" = "#0D47A1"),
    name = "Problem Type"
  ) +
  
  labs(
    title = "Strategy Proportions by Problem Complexity, Network Size and Density",
    x = "Group Size & Density Conditions", 
    y = "Mean Proportion"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(size = 20, face = "italic"),  # Added face = "italic"
    axis.title.y = element_text(size = 20), 
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    strip.text = element_text(size = 20),
    legend.position = "bottom", 
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Create separate facets for each strategy with landscape comparisons
strategies_separate_facets <- ggplot(all_strategies_data, aes(x = condition, y = mean_proportion, fill = landscape_type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(width = 0.7), width = 0.25, linewidth = 0.8) +
  
  # Each strategy gets its own facet with fixed y-axis
  facet_wrap(~ strategy_name, scales = "fixed", ncol = 3) +
  
  scale_fill_manual(
    values = c("Simple" = "#BBDEFB", "Complex" = "#0D47A1"),
    name = "Problem Type"
  ) +
  
  labs(
    title = "Strategy Proportions by Problem Complexity, Network Size and Density",
    x = "Group Size & Density Conditions", 
    y = "Mean Proportion"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(size = 20, face = "italic"),
    axis.title.y = element_text(size = 20), 
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    strip.text = element_text(size = 20),
    legend.position = "bottom", 
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  ) +
  scale_y_continuous(limits = c(0, 1.0), expand = expansion(mult = c(0, 0.05)))

print(strategies_separate_facets)


ggsave("strategies_separate_facets_revision.png", 
       plot = strategies_separate_facets,
       width = 12, height = 8, 
       dpi = 300, 
       units = "in")





