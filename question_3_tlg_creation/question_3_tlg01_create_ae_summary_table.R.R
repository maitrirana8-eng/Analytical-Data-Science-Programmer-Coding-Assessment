########################################################
# Question : question_3_tlg/01_create_ae_summary_table.R
# Plot 1   : Bar Chart or heatmap for AE Severity 
#            distribution by treatment
# Plot 2   : Top 10 most frequent AEs (with 95% Cl for 
#            incidence rates)
#Input 
# datasets : pharmaverseadam::adae
#########################################################

#Load required libraries
library(ggplot2)
library(tidyverse)

#Load input datasets

adae <- pharmaverseadam::adae

#Plot1 - AE severity distribution by treatment (bar chart or heatmap).

# AE Severity distribution by treatment
ae_sev_dist <- adae %>%
  filter(TRTEMFL == "Y") %>%  # treatment-emergent AEs
  filter(!is.na(TRT01A), !is.na(AESEV), !is.na(AEDECOD)) %>%
  mutate(
    AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
  ) %>%
  count(TRT01A, AESEV, name = "Count") %>%
  group_by(TRT01A) %>%
  mutate(Percent = Count / sum(Count)) %>%
  ungroup()

#ae_sev_dist

#Create plot stacked bar chart of AE severity counts by treatment group
p <- ggplot(ae_sev_dist, aes(x = TRT01A, y = Count, fill = AESEV)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c(
    "MILD" = "#F8766D",
    "MODERATE" = "#00BA38",
    "SEVERE" = "#619CFF"
  )) +
  labs(
    title = "AE Severity Distribution by Treatment",
    x = "Treatment",
    y = "Count of AEs",
    fill = "Severity/Intensity"
  ) +
  theme_minimal()

# Save as PNG
ggsave(
  filename = "AE_Severity_Distribution.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

#Plot2 - Top 10 most frequent AEs (with 95% Cl for incidence rates.

# Keep one record per subject / treatment / AE
ae_data <- adae %>%
  filter(TRTEMFL == "Y") %>%                    # treatment-emergent AEs
  filter(!is.na(USUBJID), !is.na(TRT01A), !is.na(AEDECOD)) %>%
  distinct(USUBJID, TRT01A, AEDECOD)

#Count subjects with each AE per treatment
ae_counts <- ae_data %>%
  group_by(TRT01A,AEDECOD) %>%
  summarise(n = n(), .groups = "drop")

#Total number of subjects
total_n <- adae %>%
  filter(!is.na(USUBJID)) %>%
  distinct(USUBJID) %>%
  nrow()

# Get top 10 most frequent AEs
top10_ae <- ae_counts %>%
  group_by(AEDECOD) %>%
  summarise(total = sum(n), .groups = "drop")%>%
  arrange(desc(total)) %>%
  slice_head(n=10) %>%
  pull(AEDECOD)

#Filter AETERM includes top10 AEs
ae_top10 <- ae_counts %>%
  filter(AEDECOD %in% top10_ae)

#Total subjects per treatment group
total_per_group <- adae %>%
  filter(!is.na(USUBJID), !is.na(TRT01A)) %>%
  distinct(USUBJID, TRT01A) %>%
  count(TRT01A, name = "total_n")

#Calculate proportion and 95% Cl for incidence rates

ae_plot_data <- ae_top10 %>%
  left_join(total_per_group, by = "TRT01A") %>%
  mutate(
    proportion = n / total_n,
    se = sqrt(proportion * (1 - proportion) / total_n),
    lower = pmax(0, proportion - 1.96 * se),
    upper = pmin(1, proportion + 1.96 * se)
  )

# Reorder AE terms by total frequency
ae_order <- ae_plot_data %>%
  group_by(AEDECOD) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(total) %>%
  pull(AEDECOD)

ae_plot_data <- ae_plot_data %>%
  mutate(AEDECOD = factor(AEDECOD, levels = ae_order))

#Create a forest chart for top 10 frequent AEs

p <- ggplot(
  ae_plot_data,
  aes(x = proportion, y = AEDECOD)
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    width = 0.2
  ) +
  scale_x_continuous(
    breaks = c(0.10, 0.20, 0.30),
    labels = function(x) paste0(round(x * 100), "%")) +
  coord_cartesian(xlim = c(0.10, 0.30)) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", total_n, "subjects; 95% Clopper-Pearson CIs incidence"),
    x = "Percentage of Patients (%)",
    y = "Adverse Event",
  ) +
  theme_minimal(base_size = 14)

#p

# Save the graph in png format
ggsave(
  filename = "Top10_AE_ForestPlot.png",
  plot = p,
  width = 10,
  height = 7,
  dpi = 300
)
  