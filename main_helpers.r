library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(openair)
library(leaflet)

# Mapping

#Make mapping 
mapping <- list(
  "MOD-00024" = "ACE Parking lot",
  "MOD-00025" = "6 Kennilworth St",
  "MOD-00026" = "The Base",
  "MOD-00027" = "Indigo Block",
  "MOD-00028" = "Hibernian Hall Playgr.",
  "MOD-PM-00141" = "Tobin Comm Ctr",
  "MOD-PM-00211" = "Dudley Vil Playgr",
  "MOD-PM-00212" = "Dearborn Academy",
  "MOD-PM-00213" = "Rafael Hernandez School",
  "MOD-PM-00214" = "Trotter Elem",
  "MOD-PM-00216" = "Orchard Gardens",
  "MOD-PM-00217" = "Residence Obierne Pl.",
  "MOD-PM-00221" = "Residence Baker Ave.",
  "MOD-PM-00222" = "Beryl Gardens",
  "MOD-PM-00223" = "Hale Elementary", #Originally Called Dillaway School idk why
  "MOD-PM-00224" = "Indigo Block (2)",
  "MOD-PM-00226" = "Dudley Greenhouse",
  "MOD-PM-00230" = "Trina Persad Playgr",
  "MOD-PM-00231" = "Cardinal Medeiros"
)

# Groups
gas_sensors <- c("MOD-00024", "MOD-00025", "MOD-00026", "MOD-00027", "MOD-00028")
construction_sensors <- c("MOD-00025", "MOD-PM-00217", "MOD-PM-00226", "MOD-PM-00231")
schoolbus_sensors <- c("MOD-PM-00212", "MOD-PM-00213", "MOD-PM-00214", "MOD-PM-00223")
publicbus_sensors <- c("MOD-00024", "MOD-00028", "MOD-PM-00141", "MOD-PM-00222")
commuter_sensors <- c("MOD-00026", "MOD-00027", "MOD-PM-00221", "MOD-PM-00224")

gas_pollutants <- c("co", "o3", "no", "no2")
particulate_pollutants <- c( "pm1", "pm25", "pm10")


#Function to Generate TimeVariation Plots based on Group
averageAndIndividualTimeVariations <- function(sensor_group, group_title="Sensors", overall=FALSE) {
data_for_sensor <- my_df_filtered %>% filter(sn == sensor_group)
  
  
for (sensor in sensor_group) {
  data_for_sensor <- my_df_filtered %>% filter(sn == sensor)

  #Normalized
  title <- paste(sensor, mapping[[sensor]], sep= " - ") 
  temp_plot = openair::timeVariation(data_for_sensor, pollutant = c("pm1", "pm10", "pm25"), limits = c(0,10), main =title, normalise = TRUE)
  plot(temp_plot, subset = "hour", main = paste(mapping$sensor, "TimeVariation Plot"))

  #Non-Normalized
  temp_plot = openair::timeVariation(data_for_sensor, pollutant = c("pm1", "pm10", "pm25"), limits = c(0,10), main =title, normalise = FALSE)
  plot(temp_plot, subset = "hour", main = paste(mapping$sensor, "TimeVariation Plot"))

  #Non-Normalized Seasonal
  temp_plot = openair::timeVariation(data_for_sensor, pollutant = c("pm1", "pm10", "pm25"), type ="season", limits = c(0,10), main =title, normalise = FALSE)
  plot(temp_plot, subset = "hour", main = paste(mapping$sensor, "TimeVariation Plot"))

  }

if (overall) {
  data_for_sensor <- my_df_filtered %>% filter(sn %in% sensor_group)
  title <- as.character(group_title) # Title the figure by sensor number
  temp_plot = openair::timeVariation(data_for_sensor, pollutant = c("pm1", "pm10", "pm25"), limits = c(0,10), main =title, normalise = TRUE)
  plot(temp_plot, subset = "hour", main = paste(mapping$sensor, "TimeVariation Plot"))
}

}

#Generate TimeVariation Plots for a Single Sensor (Entire Week, Weekday only, and Weekend Only)
timeVariationByDay <- function(sensor, pollutants, data, normalized=FALSE, entireweek=FALSE) {
  
  # Filter the dataset for the given sensor
  data_for_sensor <- data %>% filter(sn == sensor)
  
  # Split into weekdays and weekends
  df_weekdays <- data_for_sensor[!weekdays(as.Date(data_for_sensor$date)) %in% c("Saturday", "Sunday"), ]
  df_weekends <- data_for_sensor[weekdays(as.Date(data_for_sensor$date)) %in% c("Saturday", "Sunday"), ]
  
  # Generate the time variation plot for weekdays only
  title_weekday <- paste(sensor, "-", mapping[[sensor]], "(WEEKDAY ONLY)")
  temp_plot_weekday <- openair::timeVariation(df_weekdays, pollutant = pollutants, 
                                              limits = c(0,10), main = title_weekday, normalise = normalized)
  plot(temp_plot_weekday, subset = "hour", main = paste(mapping[[sensor]], "TimeVariation Plot (Weekdays)"))
  
  # Generate the time variation plot for weekends only
  title_weekend <- paste(sensor, "-", mapping[[sensor]], "(WEEKEND ONLY)")
  temp_plot_weekend <- openair::timeVariation(df_weekends, pollutant = pollutants, 
                                              limits = c(0,10), main = title_weekend, normalise = normalized)
  plot(temp_plot_weekend, subset = "hour", main = paste(mapping[[sensor]], "TimeVariation Plot (Weekends)"))
  
    
  # Generate the time variation plot for entire week 
  if (entireweek) {
      title_weekend <- paste(sensor, "-", mapping[[sensor]], "(ENTIRE WEEK)")
      temp_plot_weekend <- openair::timeVariation(data_for_sensor, pollutant = pollutants, 
                                                  limits = c(0,10), main = title_weekend, normalise = normalized)
      plot(temp_plot_weekend, subset = "hour", main = paste(mapping[[sensor]], "TimeVariation Plot (Entire Week)"))

  }

}


#Time Variation With Spliced Data & Median Heatmap for spliced data
filterBySensorData <- function(data, sensor_to_filter, start_date, end_date) {
  data %>%
    filter(
      # If the sensor is not the one we want to filter, keep the row.
      # Otherwise, if it is, then keep it only if the date is outside the range.
      sn != sensor_to_filter | (sn == sensor_to_filter & (date <= start_date | date >= end_date))
    )
}


plotAnnulusMap <- function(sensor, pollutant, data, max =0, statistic="mean") {
  # Filter dataset for the given sensor
  data_for_sensor <- data %>% filter(sn == sensor)
  
  if (max == 0) {
    max_limit <- quantile(data_for_sensor %>% pull(all_of(pollutant)), 0.95, na.rm = TRUE)
  } else {
    max_limit = max
  }

  # Plot the Annulus Map
  annulusMap(
    data_for_sensor,
    pollutant = pollutant,
    key.position = "bottom",
    key.header = paste(statistic, "of", sensor, "at", toupper(pollutant), "(ppb)"),
    latitude = "lat",
    longitude = "lon",
    provider = "OpenStreetMap",
    limits = c(0, max_limit),
    cols = "jet",
    key = TRUE,
    statistic= statistic
  )
}

plotPolarMap <- function(sensor, pollutant, data, max =0, statistic="mean") {
  # Filter dataset for the given sensor
  data_for_sensor <- data %>% filter(sn == sensor)
  
  if (max == 0) {
    max_limit <- quantile(data_for_sensor %>% pull(all_of(pollutant)), 0.95, na.rm = TRUE)
  } else {
    max_limit = max
  }


  # Plot the Annulus Map
  polarPlot(
    data_for_sensor,
    pollutant = pollutant,
    key.position = "bottom",
    key.header = paste(statistic, "of", sensor, "at", toupper(pollutant), "(ppb)"),
    limits = c(0, max_limit),
    cols = "jet",
    key = TRUE,
    statistic = statistic
  )
}



 computeFullSummary <- function(my_df, sensors, pollutants) {
  results <- data.frame(Sensor = character(), 
                        Pollutant = character(), 
                        Mean = numeric(), 
                        Median = numeric(), 
                        SD = numeric(), 
                        P25 = numeric(), 
                        P75 = numeric(), 
                        P95 = numeric(), 
                        Lat = numeric(), 
                        Lon = numeric(), 
                        stringsAsFactors = FALSE)

  for (pollutant in pollutants) {
    for (sensor in sensors) {
      data_for_sensor <- my_df %>% filter(sn == sensor)

      if (nrow(data_for_sensor) == 0 || !(pollutant %in% colnames(data_for_sensor))) next

      # Compute summary stats
      mean_value <- mean(data_for_sensor[[pollutant]], na.rm = TRUE)
      median_value <- median(data_for_sensor[[pollutant]], na.rm = TRUE)
      sd_value <- sd(data_for_sensor[[pollutant]], na.rm = TRUE)
      p25_value <- quantile(data_for_sensor[[pollutant]], 0.25, na.rm = TRUE)
      p75_value <- quantile(data_for_sensor[[pollutant]], 0.75, na.rm = TRUE)
      p95_value <- quantile(data_for_sensor[[pollutant]], 0.95, na.rm = TRUE)

      # Get coordinates (assume unique per sensor)
      sensor_lat <- unique(data_for_sensor$lat)
      sensor_lon <- unique(data_for_sensor$lon)

      results <- rbind(results, data.frame(Sensor = sensor, 
                                           Pollutant = pollutant, 
                                           Mean = mean_value, 
                                           Median = median_value, 
                                           SD = sd_value, 
                                           P25 = p25_value, 
                                           P75 = p75_value, 
                                           P95 = p95_value, 
                                           Lat = sensor_lat, 
                                           Lon = sensor_lon))
    }
  }

  return(results)
}

barCharts <- function(summary_df, pollutant_to_plot, stats, include_sd = TRUE, order_by = c("group", "overall")) {
  
  order_by <- match.arg(order_by)
  
  # Create Sensor_Label
  summary_df <- summary_df %>%
    mutate(Sensor_Label = paste(Sensor, "\n", sapply(Sensor, function(x) mapping[[x]])))

  # Filter by pollutant
  filtered_df <- summary_df %>%
    filter(Pollutant == pollutant_to_plot)

  # Drop rows with NA in required fields
  required_fields <- c("Sensor_Label", "Group", stats)
  if (include_sd) required_fields <- c(required_fields, "SD")
  filtered_df <- filtered_df %>% filter(if_all(all_of(required_fields), ~ !is.na(.)))

  # Skip if nothing left to plot
  if (nrow(filtered_df) == 0) {
    stop("No valid data to plot after removing rows with missing values.")
  }

  # Order logic
  if (order_by == "overall") {
    filtered_df <- filtered_df %>%
      arrange(desc(.data[[stats]])) %>%
      mutate(Sensor_Label = factor(Sensor_Label, levels = unique(Sensor_Label)))
  } else {
    filtered_df <- filtered_df %>%
      group_by(Group) %>%
      arrange(desc(.data[[stats]]), .by_group = TRUE) %>%
      ungroup() %>%
      mutate(Sensor_Label = factor(Sensor_Label, levels = unique(Sensor_Label)))
  }

  options(repr.plot.width = 25, repr.plot.height = 10)

  # Base plot
  p <- ggplot(filtered_df, aes(x = Sensor_Label, y = .data[[stats]], fill = Group)) +
    geom_bar(stat = "identity", width = 0.5)

  # Optional error bars
  if (include_sd) {
    p <- p + geom_errorbar(
      aes(ymin = pmax(0, .data[[stats]] - SD), ymax = .data[[stats]] + SD),
      width = 0.2, color = "black", size = 1
    )
  }

  # Add facet if grouped
  if (order_by == "group") {
    p <- p + facet_wrap(~ Group, scales = "free_x", nrow = 1)
  }

  # Final polish
  p <- p +
    labs(
      title = paste(stats, "Values of", pollutant_to_plot, ifelse(order_by == "group", "by Sensor Group", "by Sensor")),
      x = "Sensor",
      y = paste(stats, "Value (log scale)")
    ) +
    scale_y_continuous(trans = "pseudo_log") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, margin = margin(t = 10)),
      axis.title.x = element_text(size = 12),
      strip.text = element_text(size = 12)
    )

  # Save output
  output_file <- paste(stats, "of", pollutant_to_plot, "with_SD", include_sd, "ordered_by", order_by, sep = "_")
  ggsave(paste0(output_file, ".png"), plot = p, width = 25, height = 10, dpi = 300, bg = "white")

  return(p)
}

# Internal Function for heatmap
percentile_summary <- function(my_df, sensors, pollutants) {
  results <- data.frame(Sensor = character(), 
                        Pollutant = character(), 
                        P25 = numeric(),    # 25th percentile
                        Median = numeric(), 
                        P75 = numeric(),    # 75th percentile
                        P95 = numeric(),    # 95th percentile
                        stringsAsFactors = FALSE)

  for (pollutant in pollutants) {
    for (sensor in sensors) {
      data_for_sensor <- my_df %>% filter(sn == sensor, pollutant == pollutant)

      if (nrow(data_for_sensor) == 0) next  # Skip if no data

      # Calculate mean, median, and standard deviation
      median_value <- median(data_for_sensor[[pollutant]], na.rm = TRUE)

      # Calculate the percentiles (25th, 75th, 95th)
      p25_value <- quantile(data_for_sensor[[pollutant]], 0.25, na.rm = TRUE)
      p75_value <- quantile(data_for_sensor[[pollutant]], 0.75, na.rm = TRUE)
      p95_value <- quantile(data_for_sensor[[pollutant]], 0.95, na.rm = TRUE)

      # Add to the results (including Pollutant)
      results <- rbind(results, data.frame(Sensor = sensor, 
                                           Pollutant = pollutant, 
                                           P25 = p25_value,   # Add 25th percentile
                                           Median = median_value, 
                                           P75 = p75_value,   # Add 75th percentile
                                           P95 = p95_value))
    }
  }
  return(results)
}



# percentile heatmap with transposed axes for each pollutant
percentileHeatmap <- function(my_df, sensors, pollutants) {
  
  # Loop through each pollutant
  for (pollutant in pollutants) {
    
    # Get the summary table for the specific pollutant
    results <- percentile_summary(my_df, sensors, pollutants = pollutant)
    
    # Reshape the data to long format for the heatmap
    results_long <- results %>%
      pivot_longer(cols = c(P25, Median, P75, P95), names_to = "Percentile", values_to = "Value") %>%
      # Recode pollutant names for more readable labels (optional)
      mutate(Pollutant = recode(Pollutant, "pm25" = "PM 2.5")) %>%

      # Factor levels to control the order of appearance
      mutate(Sensor = factor(Sensor, levels = unique(results$Sensor)),
             Percentile = factor(Percentile, levels = c("P25", "Median", "P75", "P95")))
    
    # Create the transposed heatmap for the current pollutant
p <- ggplot(results_long, aes(y = Sensor, x = Percentile)) +
  geom_tile(aes(fill = Value)) +  # Tile fill by value
  geom_text(aes(label = round(Value, 2)), color = "black") +  # Add numeric labels inside the tiles
  scale_fill_gradient2( low = "white", high = "red",midpoint =min(results_long$Value, na.rm = TRUE),
                       limits = c(min(results_long$Value, na.rm = TRUE), 
                                  max(results_long$Value, na.rm = TRUE))) +  
  labs(title = paste("Percentile Heatmap for", pollutant),
       x = "Percentile",
       y = "Sensor",
       fill = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_y_discrete(labels = function(x) paste(x, sapply(x, function(sensor) paste0(" - ", mapping[[sensor]]))))  # Modify y-axis labels

    
    # Save the plot for the current pollutant
    ggsave(filename = paste0("percentile_heatmap_transposed_", pollutant, ".png"), plot = p,   width = 12, height = 5, dpi = 300,bg = "white")
    
    # Optionally, print the plot
    print(p)
  }
}


generateNetworkMap <- function(summary_data, summary_stat = "Median", pollutant) {
  required_cols <- c("Lon", "Lat", "Median", "Mean", "SD", "P25", "P75", "P95")
  if (!all(required_cols %in% colnames(summary_data))) {
    stop(paste("Error: summary_data must include", paste(required_cols, collapse = ", ")))
  }

  # Filter data for specified pollutant
  new_summary_data <- summary_data %>% filter(Pollutant == pollutant)

  # Determine the summary values
  summary_values <- switch(summary_stat,
                           "Mean" = new_summary_data$Mean,
                           "Median" = new_summary_data$Median,
                           "Max" = new_summary_data$Mean + new_summary_data$SD,
                           "P25" = new_summary_data$P25,
                           "P75" = new_summary_data$P75,
                           "P95" = new_summary_data$P95,
                           stop("Error: Unsupported summary_stat. Choose from 'Mean', 'Median', 'Max', 'P25', 'P75', or 'P95'."))

  # Filter out rows with NA values in the selected summary stat
  valid_indices <- !is.na(summary_values)
  new_summary_data <- new_summary_data[valid_indices, ]
  summary_values <- summary_values[valid_indices]

  # Skip map creation if no valid data points
  if (nrow(new_summary_data) == 0) {
    stop("No valid data to plot after filtering out NA summary values.")
  }

  # Define color scale
  color_palette <- colorNumeric(palette = "RdYlGn", domain = summary_values, reverse = TRUE)

  # Create leaflet map
  leaflet(new_summary_data) %>%
    addTiles() %>%
    addCircleMarkers(
      ~Lon, ~Lat,
      color = "black",
      weight = 2,
      fillColor = ~color_palette(summary_values),
      fillOpacity = 0.9,
      popup = ~paste(
        "<b>Sensor:</b>", Sensor, "<br>",
        "<b>Pollutant:</b>", Pollutant, "<br>",
        "<b>Summary Stat:</b>", summary_stat, "<br>",
        "<b>Value:</b>", round(summary_values, 2), "<br>",
        "<b>SD:</b>", round(SD, 2)
      )
    ) %>%
    addLegend(
      "bottomright",
      pal = color_palette,
      values = summary_values,
      title = paste(summary_stat, pollutant, "Pollutant Level"),
      opacity = 1
    )
}


dutyCycleSensor <- function(sensor_id, chemical, my_df) {
  # Ensure date column is Date class
  my_df$date <- as.Date(my_df$date)

  # Filter for the selected sensor only
  df <- my_df %>%
    filter(sn == sensor_id) %>%
    arrange(date)

  # Pull values for the specified chemical column
  df <- df %>%
    select(date, sn, value = all_of(chemical))

  # Get full date range
  full_dates <- seq(min(df$date, na.rm = TRUE), max(df$date, na.rm = TRUE), by = "day")

  # Create complete timeline
  timeline <- data.frame(date = full_dates) %>%
    left_join(df, by = "date") %>%
    mutate(
      status = case_when(
        is.na(value) ~ "off",
        value < 0 ~ "off",
        TRUE ~ "on"
      )
    )

  # Group continuous runs of same status
  timeline <- timeline %>%
    mutate(
      group = cumsum(
        lag(status, default = first(status)) != status
      )
    )

segments <- timeline %>%
  group_by(group, status) %>%
  summarise(
    start = min(date),
    end = max(date) + 1,  # <-- extend end date
    .groups = "drop"
  )

  # Plot
  ggplot(segments) +
    geom_segment(aes(x = start, xend = end, y = sensor_id, yend = sensor_id, color = status), size = 5) +
    scale_color_manual(values = c("on" = "green", "off" = "red")) +
    labs(
      title = paste("Sensor Timeline for", mapping[sensor_id], "-", chemical),
      x = "Date",
      y = "Sensor"
    ) +
    theme_minimal()
}

dutyCycleMultipleSensors <- function(chemical, my_df, sensors) {

  # Ensure proper column types
  my_df$date <- as.Date(my_df$date)

  # Get all unique sensors from 'sn'

  # Create a full date sequence from the min to max date
  full_dates <- seq(min(my_df$date, na.rm = TRUE), max(my_df$date, na.rm = TRUE), by = "day")

  # Build status timeline for each sensor
  sensor_timeline <- lapply(seq_along(sensors), function(i) {
    sensor_id <- as.character(sensors[[i]])  # force to single string

    df_sensor <- my_df %>%
      filter(sn == sensor_id) %>%
      select(date, value = all_of(chemical)) %>%
      arrange(date)

    # Create a complete timeline with all days in the range
    timeline <- data.frame(date = full_dates) %>%
      left_join(df_sensor, by = "date") %>%
      mutate(sensor = sensor_id) %>%
      mutate(
        status = case_when(
          is.na(value) ~ "off",   # Mark missing as 'off'
          value < 0 ~ "off",      # Mark negative values as 'off'
          TRUE ~ "on"             # All other values are 'on'
        )
      ) %>%
      arrange(date) %>%
      mutate(
        group = cumsum(lag(status, default = first(status)) != status)
      )

    # Collapse continuous segments into start/end
    timeline %>%
      group_by(sensor, group, status) %>%
      summarise(start = min(date), end = max(date) + 1, .groups = "drop")
  }) %>%
    bind_rows()

  # Double-check that the 'sensor' column is a character type
sensor_timeline$sensor <- paste0( sensor_timeline$sensor,  " - ", mapping[as.character(sensor_timeline$sensor)]
)

  # Plot
  ggplot(sensor_timeline) +
    geom_segment(
      aes(x = start, xend = end, y = sensor, yend = sensor, color = status),
      size = 5
    ) +
    scale_color_manual(values = c("on" = "green", "off" = "red")) +
    labs(
      title = paste("Sensor Timelines for Chemical:", chemical),
      x = "Date",
      y = "Sensor"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      legend.position = "top"
    )
}

individualMaps <- function(sensors, pollutants, stats, function_name) {

  for (sensor in sensors) {
  sensor_filtered <- my_df_filtered %>% filter(sn == sensor)
  
  for (p in pollutants) {
    
    # Dynamically extract the 95th percentile for this pollutant
    max_val <- quantile(sensor_filtered[[p]], 0.95, na.rm = TRUE)
    
    # Set correct unit
    unit <- if (grepl("pm", p)) "µg/m³" else "ppb"
    
    for (stat in stats) {
      fun <- match.fun(function_name)
              # Create polar map
          map <- fun(sensor_filtered,
                      pollutant = p, 
                      latitude = "lat",
                      longitude = "lon", 
                      provider = "OpenStreetMap",
                      cols = "jet",
                      alpha = 0.8,
                      key = FALSE,
                      statistic = stat,      
                      limits = c(0, max_val),
                      cex = 2)

      

      


      
      # Save map
      html_name =  paste(function_name,"map.html", sep="_")
      saveWidget(map,html_name, selfcontained = TRUE)
      filename_png <- paste(mapping[[as.character(sensor)]], p, stat, function_name, "map.png", sep = "_")
      webshot(html_name, file = filename_png, vwidth = 1000, vheight = 800)
      
      
       
    }
  }
}

}



pieCharts <- function(df, sensor, pollutant, low_threshold, high_threshold, save_path = NULL) {
  
  sensor_filtered <- df %>% filter(sn == sensor)
  
  pol_low <- sum(sensor_filtered[[pollutant]] < low_threshold, na.rm=TRUE)
  pol_high <- sum(sensor_filtered[[pollutant]] > high_threshold, na.rm=TRUE)
  pol_mid <- sum(sensor_filtered[[pollutant]] > 0, na.rm=TRUE) - pol_low - pol_high
  
  pol_pie <- data.frame(values = c(pol_low, pol_mid, pol_high),
                        labels = c("low", "medium", "high"))
  
  pol_pie <- mutate(pol_pie, percent = round(values / sum(values) * 100, 1))
  
  # If a save_path is provided, open png device
  if (!is.null(save_path)) {
    png(filename = paste0(save_path,mapping[[sensor]],"_", pollutant, "_pie_chart.png"), width = 800, height = 600)
  }
  
  pie(pol_pie$values,
      labels = round(pol_pie$percent, 1),
      main = paste(mapping[[sensor]],"-", sensor, ":" , "Fraction of time at different", pollutant, "thresholds"),

      col = c(3, 7, 2))
  
  legend("topright", pol_pie$labels,
         cex = 0.8, fill = c(3, 7, 2))
  
  # If a save_path is provided, close png device
  if (!is.null(save_path)) {
    dev.off()
  }
  
  print(paste("Chart created for sensor:", sensor))
}

