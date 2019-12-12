# ====================================================================================
# Functions for the Risk By Cause plot
# ====================================================================================

# Data ===============================================================================
riskByCauseData <- readRDS(path(myPlace, "/myData/risk-by-cause.RDS"))

# Constants ==========================================================================
SHOW_TOP = 25  # Could be a user input if we want

MEASURES <- list(
  "1" = list(name = "Deaths", short_name = "Deaths"),
  "2" = list(name = "Disability-Adjusted Life Years", short_name = "DALYs"),
  "3" = list(name = "Years Lived with Disability",  short_name = "YLDs"),
  "4" = list(name = "Years of Life Lost", short_name = "YLLs")
)
METRICS <- list(
  "1" = list(name = "Number", symbol = "#"),
  "2" = list(name = "Percent", symbol = "%"),
  "3" = list(name = "Rate (per 100,000)", symbol = "Rate")
)
SEXES <- list(
  "1" = list(name = "Males"),
  "2" = list(name = "Females"),
  "3" = list(name = "Both sexes")
)

# Color palette for the plot -----------------------------
red <- colorRampPalette(c("#FBE1D4", "#8F1919"))(7)
blue <- colorRampPalette(c("#CAD9EC", "#1A468F"))(6)
purple <- colorRampPalette(c("#BCBDDB", "#6A51A3"))(6)
green <- colorRampPalette(c("#C1E1B5", "#43884E"))(3)

BAR_PALETTE = c("HIV/AIDS & STIs"=red[1],
                "Respiratory infections &TB"=red[2],
                "Enteric infections"=red[3],
                "NTDs & malaria"=red[4],
                "Other infectious"=red[5],
                "Maternal & neonatal"=red[6],
                "Nutritional deficiencies"=red[7],
                "Neoplasms"=blue[1],
                "Cardiovascular diseases"=blue[2],
                "Chronic respiratory"=blue[3],
                "Digestive diseases"=blue[4],
                "Neurological disorders"=blue[5],
                "Mental disorders"=blue[6],
                "Substance use"=purple[1],
                "Diabetes & CKD"=purple[2],
                "Skin diseases"=purple[3],
                "Sense organ diseases"=purple[4],
                "Musculoskeletal disorders"=purple[5],
                "Other non-communicable"=purple[6],
                "Transport injuries"=green[1],
                "Unintentional injuries"=green[2],
                "Self-harm & violence"=green[3])

# Filter Data Function ==========================================================================
FilterRiskByCause <- function(level_id_in, year_id_in, sex_id_in, metric_id_in, measure_id_in) {
  filtered_data <- riskByCauseData %>%
    filter(grepl(level_id_in, level),
           year_id == year_id_in,
           sex_id == sex_id_in,
           metric_id == metric_id_in,
           measure_id == measure_id_in)
  
  return(list(data = filtered_data,
              xlabel = paste(METRICS[[metric_id_in]]$name, MEASURES[[measure_id_in]]$short_name, sep=" of "),
              title = paste0("California, ", SEXES[[sex_id_in]]$name, ", ", year_id_in),
              measure = MEASURES[[measure_id_in]]$short_name,
              metric = METRICS[[metric_id_in]]$name)
  )
}

# Plot Function ==========================================================================
RiskByCausePlot <- function(data_in) {
  
  # Uncomment next line to filter negative values (only exist for alcohol - this is weird) 
  # data <- filter(data, val > 0)
  
  ### Add Total.  Arrange by sort order for ordering bars?
  data <- data_in$data
  data <- data %>%
    mutate(total = mapply(function(r) sum(data$val[data$risk_id == r]), risk_id))
  
  ### Remove non-top items
  totals = unique(data$total)
  n <- length(totals)
  if (n > SHOW_TOP) {
    min_total = sort(totals,partial=n-SHOW_TOP)[n-SHOW_TOP]
    data <- data %>% 
      filter(total > min_total) %>%
      arrange(total)
  }
  
  p <- ggplot(data, aes(x = reorder(risk_short_name, total), y = val, fill = cause_short_name,
                        text=paste0(cause_name,
                                    '<br><b>Attributable Risk: </b>', risk_name,
                                    '<br><b>', data_in$metric, ': </b>', val, " ", data_in$measure)
                        )
              ) +
    geom_bar(stat = "identity", lwd=0.2, color="white") +
    labs(title=data_in$title, x="", y=data_in$xlabel) +
    scale_fill_manual(name="Cause", values=BAR_PALETTE) +
    theme_classic() +
    #theme(legend.position = "right",legend.text = element_text(margin = margin(r = 10, unit = "pt")), legend.key.size = unit(4.0, 'cm')) +
    coord_flip(ylim = c(0, max(data$total))) # keep as last ;)
  
 # dev.off()
  ggplotly(p, tooltip="text") # %>% layout(legend = list(orientation = "v", x = 100000, size = 0.1))
}
