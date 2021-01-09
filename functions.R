#---------------------------------------#
# Functions for Microdrop Assay Tool
# Auralee Walmer
# 2019-2020
#---------------------------------------#

library(purrr)
library(magrittr)
library(tidyr)
library(reshape2)
library(stringr)
library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(drc)
library(pracma)
library(ggrepel)
library(readxl)
library(RPMG)
library(Metrics)
library(staplr)
library(berryFunctions)

#---------------------------------------#
# PARSE DATA
#---------------------------------------#

capture_third_input_set <- function(input_data_tab1) { # excel file first tab; insert contents1
  third_plate_index <- last(which(input_data_tab1[,1]=='A'))
  plate <- input_data_tab1[third_plate_index:(third_plate_index+15), 2:25]
  return(plate)
}

### Quadrants for Full Plate ###

extract_Q1_fplate <- function(df_384) { # Q1 = odd columns & odd rows
  odd_col <- seq(from = 1, to = 24, by = 2)
  odd_row <- seq(from = 1, to = 16, by = 2)
  return(df_384[odd_row, odd_col])
}

extract_Q2_fplate <- function(df_384) { # Q2 = even columns & odd rows
  even_col <- seq(from = 2, to = 24, by = 2)
  odd_row <- seq(from = 1, to = 16, by = 2)
  return(df_384[odd_row, even_col])
}

extract_Q3_fplate <- function(df_384) { # Q3 = odd columns & even rows
  odd_col <- seq(from = 1, to = 24, by = 2)
  even_row <- seq(from = 2, to = 16, by = 2)
  return(df_384[even_row, odd_col])
}

extract_Q4_fplate <- function(df_384) { # Q4 = even columns & even rows
  even_col <- seq(from = 2, to = 24, by = 2)
  even_row <- seq(from = 2, to = 16, by = 2)
  return(df_384[even_row, even_col])
}

### Quadrants for Half Plate ###

extract_Q1_hplate <- function(df_384) {
  return(extract_Q1_fplate(df_384)[1:4,])
}

extract_Q2_hplate <- function(df_384) {
  return(extract_Q2_fplate(df_384)[1:4,])
}

extract_Q3_hplate <- function(df_384) {
  return(extract_Q3_fplate(df_384)[1:4,])
}

extract_Q4_hplate <- function(df_384) {
  return(extract_Q4_fplate(df_384)[1:4,])
}

extract_Q5_hplate <- function(df_384) {
  return(extract_Q1_fplate(df_384)[5:8,])
}

extract_Q6_hplate <- function(df_384) {
  return(extract_Q2_fplate(df_384)[5:8,])
}

extract_Q7_hplate <- function(df_384) {
  return(extract_Q3_fplate(df_384)[5:8,])
}

extract_Q8_hplate <- function(df_384) {
  return(extract_Q4_fplate(df_384)[5:8,])
}

### Build table based on quadrant ###

build_quadrant_table_24hr <- function(quadrant, quadrant_number, plate_info, conversion_constant, full=c(TRUE, FALSE)) { # 96-well df, integer(1-4), df, decimal
  colnames(quadrant) <- plate_info$`PEG %`[!is.na(plate_info$`PEG %`)] # 12 values, rename before adding columns
  if (full == TRUE) {
    quadrant$Buffer <- plate_info$`96w Plate Buffer`[!is.na(plate_info$`96w Plate Buffer`)]
  } else if (full == FALSE) {
    quadrant$Buffer <- plate_info$`96w Plate Buffer`[c(1:4)]
  }
  quadrant$Sample <- plate_info$`Sample Name`[quadrant_number] # 1 value
  quadrant$Quadrant <- paste0('Q',quadrant_number)
  quadrant$MAV <- plate_info$`MAV`[quadrant_number]
  if (full == TRUE) {
    quadrant$Order <- as.factor(plate_info$`96w Plate Buffer Column`[!is.na(plate_info$`96w Plate Buffer Column`)])
  }
  else if (full == FALSE) {
    quadrant$Order <- as.factor(plate_info$`96w Plate Buffer`[c(1:4)])
  }
  
  df <- melt(quadrant, id=c("Quadrant","Sample","MAV","Buffer","Order"), value.name = "Raw", variable.name = "PEG %")
  df$Raw <- as.numeric(df$Raw)
  df$`MAB=(Raw/[MAV*0.561])` <- (df$Raw)/(df$MAV*conversion_constant)
  
  return(df)
}

build_quadrant_table_3hr <- function(quadrant, quadrant_number, plate_info, full=c(TRUE, FALSE)) { # 96-well df, integer(1-4), df, decimal
  colnames(quadrant) <- plate_info$`PEG %`[!is.na(plate_info$`PEG %`)] # 12 values, rename before adding columns
  if (full == TRUE) {
    quadrant$Buffer <- plate_info$`96w Plate Buffer`[!is.na(plate_info$`96w Plate Buffer`)]
  } else if (full == FALSE) {
    quadrant$Buffer <- plate_info$`96w Plate Buffer`[c(1:4)]
  }
  quadrant$Sample <- plate_info$`Sample Name`[quadrant_number] # 1 value
  quadrant$Quadrant <- paste0('Q',quadrant_number)
  if (full == TRUE) {
    quadrant$Order <- as.factor(plate_info$`96w Plate Buffer Column`[!is.na(plate_info$`96w Plate Buffer Column`)])
  }
  else if (full == FALSE) {
    quadrant$Order <- as.factor(plate_info$`96w Plate Buffer`[c(1:4)])
  }
  
  df <- melt(quadrant, id=c("Quadrant","Sample","Buffer","Order"), value.name = "Raw", variable.name = "PEG %")
  df$Raw <- as.numeric(df$Raw)
  
  return(df)
}


### Parse Full Plate ###

parse_full_plate <- function(concentration_data, plate_info, onset_type=c("24hr","3hr")) { # concentration_data = capture_third_input_set(input_data_tab1)
  
  q1 <- extract_Q1_fplate(concentration_data) # quadrants
  q2 <- extract_Q2_fplate(concentration_data)
  q3 <- extract_Q3_fplate(concentration_data)
  q4 <- extract_Q4_fplate(concentration_data)
  
  if (onset_type=="24hr") {
    q1_table <- build_quadrant_table_24hr(q1, 1, plate_info, conversion_constant=0.561, full=TRUE) # tables
    q2_table <- build_quadrant_table_24hr(q2, 2, plate_info, conversion_constant=0.561, full=TRUE)
    q3_table <- build_quadrant_table_24hr(q3, 3, plate_info, conversion_constant=0.561, full=TRUE)
    q4_table <- build_quadrant_table_24hr(q4, 4, plate_info, conversion_constant=0.561, full=TRUE)
  } else if (onset_type=="3hr") {
    q1_table <- build_quadrant_table_3hr(q1, 1, plate_info, full=TRUE)
    q2_table <- build_quadrant_table_3hr(q2, 2, plate_info, full=TRUE)
    q3_table <- build_quadrant_table_3hr(q3, 3, plate_info, full=TRUE)
    q4_table <- build_quadrant_table_3hr(q4, 4, plate_info, full=TRUE)
  }
  
  df <- rbind(q1_table, q2_table, q3_table, q4_table)
  df <- df[order(df$Quadrant, df$Order),]
  df$Order <- NULL
  
  return(df) # combined table
}


### Parse Half Plate ###

parse_half_plate <- function(concentration_data, plate_info, onset_type=c("24hr","3hr")) { # concentration_data = capture_third_input_set(input_data_tab1)
  
  q1 <- extract_Q1_hplate(concentration_data) # quadrants
  q2 <- extract_Q2_hplate(concentration_data)
  q3 <- extract_Q3_hplate(concentration_data)
  q4 <- extract_Q4_hplate(concentration_data)
  q5 <- extract_Q5_hplate(concentration_data)
  q6 <- extract_Q6_hplate(concentration_data)
  q7 <- extract_Q7_hplate(concentration_data)
  q8 <- extract_Q8_hplate(concentration_data)
  
  if (onset_type=="24hr") {
    q1_table <- build_quadrant_table_24hr(q1, 1, plate_info, conversion_constant=0.561, full=FALSE) # tables
    q2_table <- build_quadrant_table_24hr(q2, 2, plate_info, conversion_constant=0.561, full=FALSE)
    q3_table <- build_quadrant_table_24hr(q3, 3, plate_info, conversion_constant=0.561, full=FALSE)
    q4_table <- build_quadrant_table_24hr(q4, 4, plate_info, conversion_constant=0.561, full=FALSE)
    q5_table <- build_quadrant_table_24hr(q5, 5, plate_info, conversion_constant=0.561, full=FALSE)
    q6_table <- build_quadrant_table_24hr(q6, 6, plate_info, conversion_constant=0.561, full=FALSE)
    q7_table <- build_quadrant_table_24hr(q7, 7, plate_info, conversion_constant=0.561, full=FALSE)
    q8_table <- build_quadrant_table_24hr(q8, 8, plate_info, conversion_constant=0.561, full=FALSE)
  } else if (onset_type=="3hr") {
    q1_table <- build_quadrant_table_3hr(q1, 1, plate_info, full=FALSE)
    q2_table <- build_quadrant_table_3hr(q2, 2, plate_info, full=FALSE)
    q3_table <- build_quadrant_table_3hr(q3, 3, plate_info, full=FALSE)
    q4_table <- build_quadrant_table_3hr(q4, 4, plate_info, full=FALSE)
    q5_table <- build_quadrant_table_3hr(q5, 5, plate_info, full=FALSE)
    q6_table <- build_quadrant_table_3hr(q6, 6, plate_info, full=FALSE)
    q7_table <- build_quadrant_table_3hr(q7, 7, plate_info, full=FALSE)
    q8_table <- build_quadrant_table_3hr(q8, 8, plate_info, full=FALSE)
  }
  
  df <- rbind(q1_table, q2_table, q3_table, q4_table, q5_table, q6_table, q7_table, q8_table)
  df <- df[order(df$Quadrant, df$Order),]
  df$Order <- NULL
  
  return(df) # combined table
}

#---------------------------------------#
# PIVOTED TABLES
#---------------------------------------#

pivot_sample_table <- function(sample_name, parsed_table) {
  filtered_df <- parsed_table[which(parsed_table$Sample==sample_name),]
  # insert logic to check for repeating sample groups (e.g. Water) -- INTEGRATE QUADRANT VALUE. like if as.character(unique(filtered_df$Quadrant..))
  # idea: subset to quadrant and sample by unique quadrant groups... rename samples, then apply new name to those sample groups
  # should write separate function for this validation check
  last_col <- colnames(parsed_table)[ncol(parsed_table)] # capture last col name
  filtered_df <- filtered_df[c("Buffer","PEG %",last_col)]
  pivot <- reshape(filtered_df, idvar = "Buffer", timevar = "PEG %", direction = "wide") # pivot df
  colnames(pivot)[2:ncol(pivot)] <- as.character(unique(filtered_df$`PEG %`))
  return(pivot)
}

pivot_buffer_table <- function(buffer_name, parsed_table) {
  filtered_df <- parsed_table[which(parsed_table$Buffer==buffer_name),]
  last_col <- colnames(parsed_table)[ncol(parsed_table)] # capture last col name
  filtered_df <- filtered_df[c("Sample","PEG %",last_col)]
  pivot <- reshape(filtered_df, idvar = "Sample", timevar = "PEG %", direction = "wide") # pivot df
  colnames(pivot)[2:ncol(pivot)] <- as.character(unique(filtered_df$`PEG %`)) 
  return(pivot)
}


rename_redundant_samples <- function(parsed_table) {
  # dynamically create list of indexes for which first distinct quadrant value occurrs. 
  qlist <- as.character(unique(parsed_table$Quadrant))
  indexlist <- c()
  for (q in qlist) {
    index <- first(which(parsed_table$Quadrant==q))
    indexlist <- append(indexlist, index)
  }
  df <- parsed_table[indexlist,] # subset to distinct quadrants
  df$duplicate <- duplicated(df$Sample)
  
  if (TRUE %in% df$duplicate) {
    duplist <- df$Sample[which(df$duplicate==TRUE)]
    dupindices <- which(df$Sample %in% duplist)
    dup_df <- df[dupindices,] # df containing duplicates only
    dup_df$Sample_New <- make.unique(dup_df$Sample, sep = "_") # rename duplicates
    newdf <- join(parsed_table, dup_df[c("Quadrant","Sample_New")], by="Quadrant", type="left") # merge with original table
    replacelist <- which(!is.na(newdf$Sample_New))
    newdf$Sample[replacelist] <- newdf$Sample_New[replacelist] # replace sample with sample_new where sample_new != NA
    newdf$Sample_New <- NULL
    return(newdf)
  } else {
    return(parsed_table)
  }
}


generate_sample_checkboxes <- function(parsed_table) {
  sample_list <- as.character(unique(parsed_table$Sample))
  sample_length <- length(sample_list)
  value_list <- c()
  for (x in 1:sample_length) {
    val <- paste("sample",x)
    value_list <- append(value_list, val)
  }
  names(value_list) <- sample_list
  
  return(value_list)
}

generate_buffer_checkboxes <- function(parsed_table) {
  buffer_list <- as.character(unique(parsed_table$Buffer))
  buffer_length <- length(buffer_list)
  value_list <- c()
  for (x in 1:buffer_length) {
    val <- paste("buffer",x)
    value_list <- append(value_list, val)
  }
  names(value_list) <- buffer_list
  
  return(value_list)
}


#---------------------------------------#
# ONSET CALCULATION TESTING
#---------------------------------------#

calc_log_maxtan_by_sample <- function(parsed_table, onset_type = c("24hr","3hr")) {
  
  parsed_table <- calc_loess_by_sample(parsed_table, onset_type)
  
  parsed_table$transform_log_loess_inverse <- abs(1/log10(parsed_table$loess_sample))
  
  parsed_table$max_tang <- 0
  samples <- as.character(unique(parsed_table$Sample))
  
  for (sample in samples) {
    irange <- which(parsed_table$Sample==sample)
    temp <- parsed_table[parsed_table$Sample==sample,]
    parsed_table$max_tang[irange] <- loess(temp$Raw ~ temp$`PEG %`, data=temp, span=0.50)$fitted
  }
  
  parsed_table$tang <- tan(parsed_table$transform_log_loess_inverse)
  parsed_table$max_tang <- 0
  parsed_table$max_tang[which(parsed_table$tang==max(parsed_table$tang))] <- 1 # dummy var for the point where the maximum tangent happens
  
  return(parsed_table)
}

calc_single_onset_table <- function(parsed_table, sample, buffer, onset_type=c("24hr","3hr")) {
  df <- parsed_table[parsed_table$Sample==sample,]
  df <- df[df$Buffer==buffer,]
  df$`PEG %` <- as.numeric(as.character(df$`PEG %`))
  
  df$slope <- NA
  df$slope[1] <- 0
  
  if (onset_type == "24hr") {
    for (n in 2:nrow(df)) {
      y = df$`MAB=(Raw/[MAV*0.561])`[c(n-1,n)]
      x = df$`PEG %`[c(n-1,n)]
      slope <- lm(y ~ x)$coeff[[2]]
      df$slope[n] <- slope
      df$slope <- round(df$slope, digits = 4)
    }
    df$log_val <- log(df$`MAB=(Raw/[MAV*0.561])`)
  } else {
    for (n in 2:nrow(df)) {
      y = df$Raw[c(n-1,n)]
      x = df$`PEG %`[c(n-1,n)]
      slope <- lm(y ~ x)$coeff[[2]]
      df$slope[n] <- slope
      df$slope <- round(df$slope, digits = 4)
    }
    df$log_val <- log(df$Raw)
  }
  
  df$onset <- 0
  df$onset[which(df$slope==min(df$slope))-1] <- "1"
  
  df$`PEG %` <- as.character(df$`PEG %`)
  
  return(df)
}


plot_single_onset_by_slope <- function(onset_table, onset_type=c("24hr","3hr"), fix_y_axis=c(TRUE,FALSE), y_low=NULL, y_high=NULL) {
  onset_table$onset <- as.numeric(onset_table$onset)
  onset_table$`PEG %` <- as.numeric(onset_table$`PEG %`)
  if (onset_type == "24hr") {
    p <- ggplot(data=onset_table, aes(x=onset_table$`PEG %`, y = onset_table$`MAB=(Raw/[MAV*0.561])`)) + geom_line(color='darkseagreen') + geom_point()
    q <- geom_point(data=onset_table, aes(x=onset_table$`PEG %`[which(onset_table$onset==1)], y = onset_table$`MAB=(Raw/[MAV*0.561])`[which(onset_table$onset==1)], color="red"))
    p <- p + q +
      labs(subtitle = paste("Onset = ", onset_table$`PEG %`[which(onset_table$onset==1)]), x = "PEG %", y = "MAB=(Raw/[MAV*0.561])")
  } else {
    p <- ggplot(data=onset_table, aes(x=onset_table$`PEG %`, y = onset_table$Raw)) + geom_line(color='darkseagreen') + geom_point()
    q <- geom_point(data=onset_table, aes(x=onset_table$`PEG %`[which(onset_table$onset==1)], y = onset_table$Raw[which(onset_table$onset==1)], color="red"))
    p <- p + q + 
      labs(subtitle = paste("Onset = ", onset_table$`PEG %`[which(onset_table$onset==1)]), x = "PEG %", y = "Raw")
  }
  p <- p +
    ggtitle(paste("Microdrop Plot: Sample = ", onset_table$Sample[1], ", Buffer = ", onset_table$Buffer[1])) +
    theme(
      legend.position='none', 
      plot.title=element_text(face = "bold"),
      plot.subtitle=element_text(face = "bold", color = "red")
    ) 
  
  if (fix_y_axis==TRUE) {
    if (is.null(y_low) | is.null(y_high)) {
      p <- p + ylim(-0.05, 0.5)
    } else {
      p <- p + ylim(y_low, y_high)
    }
  }
  
  return(p)
}


#---------------------------------------#
# FUNCTIONS TO MAKE PLOTS
#---------------------------------------#

build_plot_by_sample_normal <- function(parsed_table, sample, onset_type = c("24hr","3hr")) {
  
  #refactorize Buffer values
  parsed_table$Buffer <- factor(parsed_table$Buffer, levels = as.character(unique(parsed_table$Buffer)))
  
  parsed_table <- parsed_table[parsed_table$Sample==sample,]
  parsed_table$`PEG %` <- as.numeric(as.character(parsed_table$`PEG %`))
  
  if (onset_type == "24hr") {
    p <- ggplot(data=parsed_table, aes(x=as.numeric(as.character(parsed_table$`PEG %`)), y = as.numeric(as.character(parsed_table$`MAB=(Raw/[MAV*0.561])`)), colour=Buffer)) + 
      geom_point() + geom_line() + ylim(-0.05, 0.5)
  } else {
    p <- ggplot(data=parsed_table, aes(x=as.numeric(as.character(parsed_table$`PEG %`)), y = as.numeric(as.character(parsed_table$Raw)), colour=Buffer)) + 
      geom_point() + geom_line() + ylim(-0.05, 2.0)
  }
  p <- p + labs(x = "PEG (%)", y = "Mab (9mg/mL)", title=paste(sample, "(Raw)")) + 
    theme(plot.title = element_text(hjust = 0.5), legend.position="top", 
          axis.line = element_line(size = 2, colour = "black"),
          axis.text = element_text(face = "bold", color = "black", size = 10),
          axis.ticks.length = unit(.3, "cm")) +
    scale_x_continuous(breaks=c(4,6.9,9.8,12.8,15.7,18.6,21.5,24.4,27.2,30.2,33.1,36))
  return(p)
  
}

build_plot_by_buffer_normal <- function(parsed_table, buffer, onset_type = c("24hr","3hr")) {
  
  #refactorize Buffer values
  parsed_table <- parsed_table[parsed_table$Buffer==buffer,]
  parsed_table$`PEG %` <- as.numeric(as.character(parsed_table$`PEG %`))
  
  if (onset_type == "24hr") {
    p <- ggplot(data=parsed_table, aes(x=as.numeric(as.character(parsed_table$`PEG %`)), y = as.numeric(as.character(parsed_table$`MAB=(Raw/[MAV*0.561])`)), colour=Sample)) + 
      geom_point() + geom_line() + ylim(-0.05, 0.5)
  } else {
    p <- ggplot(data=parsed_table, aes(x=as.numeric(as.character(parsed_table$`PEG %`)), y = as.numeric(as.character(parsed_table$Raw)), colour=Sample)) + 
      geom_point() + geom_line() + ylim(-0.05, 2.0)
  }
  p <- p + labs(x = "PEG (%)", y = "Mab (9mg/mL)", title=paste(buffer, "(Raw)")) + 
    theme(plot.title = element_text(hjust = 0.5), legend.position="top", 
          axis.line = element_line(size = 2, colour = "black"),
          axis.text = element_text(face = "bold", color = "black", size = 10),
          axis.ticks.length = unit(.3, "cm")) +
    scale_x_continuous(breaks=c(4,6.9,9.8,12.8,15.7,18.6,21.5,24.4,27.2,30.2,33.1,36))
  return(p)
  
}



#------------------------------------------#
# ONSET NEW METHOD 10/28/2019 + Curve Fit
#------------------------------------------#

filter_basic_xytable <- function(parsed_table, sample, buffer, onset_type) {
  parsed_table <- parsed_table[parsed_table$Sample==sample,]
  parsed_table <- parsed_table[parsed_table$Buffer==buffer,]
  parsed_table$x <- as.numeric(as.character(parsed_table$`PEG %`))
  if (onset_type=="24hr") {
    parsed_table$y <- parsed_table$`MAB=(Raw/[MAV*0.561])`
  } else {
    parsed_table$y <- parsed_table$Raw
  }
  parsed_table <- parsed_table[c('x','y')]
  parsed_table$x <- round(parsed_table$x, digits = 4)
  parsed_table$y <- round(parsed_table$y, digits = 4)
  return(parsed_table)
}

get_drm_fitted_output <- function(xytable) {
  
  ## adding starting parameters 10/12/2020:
  start_params <- c(1, min(xytable$y), max(xytable$y), mean(c(min(xytable$x, max(xytable$x) ))) )
  ## make starting params conditional on whether default drm convergence fails:
  if(isTRUE( is.error(drm(xytable$y ~ xytable$x, data = xytable, fct = LL.4(), type = "continuous"), tell = FALSE, force = FALSE) ) ) {
    mL <- drm(xytable$y ~ xytable$x, data = xytable, fct = LL.4(), type = "continuous", start = start_params)
  } else {
    mL <- drm(xytable$y ~ xytable$x, data = xytable, fct = LL.4(), type = "continuous") # LL.4() - drm docs
  }
  y_fitted <- fitted(mL)
  b <- as.numeric(mL$coefficients[1])
  c <- as.numeric(mL$coefficients[2])
  d <- as.numeric(mL$coefficients[3])
  e <- as.numeric(mL$coefficients[4])
  
  return(list(model=mL, y_fitted=y_fitted, b=b, c=c, d=d, e=e)) # list allows using $ on return
}


### Function to produce many x-values for curve fit ###
multiply_fitted_values <- function(xytable) {
  x_range <- seq(from = 4, to = 36, length.out = 100)
  df <- data.frame(matrix(nrow = 0, ncol = 2))
  m <- get_drm_fitted_output(xytable)
  for (x in x_range) {
    y <- solve_fitted_y(x, m$b, m$c, m$d, m$e)
    df[nrow(df)+1,] <- c(x,y)
  }
  colnames(df) <- c('x','y')
  df$x <- round(df$x, digits = 4)
  df$y <- round(df$y, digits = 4)
  return(df)
}


get_ic5 <- function(fitted_y_vals) {
  y_range <- fitted_y_vals[1]-fitted_y_vals[length(fitted_y_vals)]
  return(fitted_y_vals[1] - y_range*.05)
}

get_ic95_3hr <- function(fitted_y_vals) {
  y_range <- abs(fitted_y_vals[1]-fitted_y_vals[length(fitted_y_vals)])
  return(fitted_y_vals[1] + y_range*.05)
}

solve_onset <- function(y, b, c, d, e) { # where y = ic5
  inner <- (e^b)*(y-d)/(c-y)
  onset <- (inner)^(1/b)
  return(onset)
}

fake_ic50_y_24hr <- function(fitted_y_vals) { # after calculating this, will be able to plug into 'get_ic50_x' function for x val
  onset_y <- get_ic5(fitted_y_vals)
  y_range <- abs(onset_y - fitted_y_vals[length(fitted_y_vals)])
  halfway <- (.5)*(y_range)
  return(onset_y - halfway)
}

fake_ic50_y_3hr <- function(fitted_y_vals) { # after calculating this, will be able to plug into 'get_ic50_x' function for x val
  onset_y <- get_ic95_3hr(fitted_y_vals)
  y_range <- abs(onset_y - fitted_y_vals[length(fitted_y_vals)])
  halfway <- (.5)*(y_range)
  return(onset_y + halfway)
}

get_ic50_y <- function(fitted_y_vals, b, c, d, e, onset_type=c('24hr','3hr')) { ### NEED TO ADD FAKE IC-50 AS AN ARGUMENT!! 
  ## what about setting c equal to the minimum fitted value (max for 3hr onset)
  if (onset_type=='24hr') {
    c <- fitted_y_vals[length(fitted_y_vals)] # final fitted value
  } else {
    d <- fitted_y_vals[length(fitted_y_vals)]
  }
  
  return(c + (d - c)/(1+exp(b*( log(e)-log(e) ))) )
}

get_ic50_x <- function(y, b, c, d, e) {
  inner <- (e^b)*(y-d)/(c-y)
  val <- (inner)^(1/b)
  return(val)
}

solve_fitted_y <- function(x, b, c, d, e) {
  return(c + (d - c)/(1+exp(b*( log(x)-log(e) ))) )
}

plot_drm_fit_onset <- function(xytable, fitted_y_vals, horiz_y, onset) {
  multiplied <- multiply_fitted_values(xytable)
  
  p <- ggplot(data=xytable, aes(x=xytable$x,y=xytable$y)) + geom_point() + 
    labs(title="Curve Fit Onset Prediction", x="PEG 3350%", y="Protein Concentration [mg/mL]") +
    theme(plot.title = element_text(hjust = 0.5, face="bold"), plot.subtitle=element_text(hjust = 0.5)) +
    geom_line(data=multiplied, aes(multiplied$x, multiplied$y), color = 'deepskyblue3', size = 1)
  
  if (fitted_y_vals[1] < fitted_y_vals[length(fitted_y_vals)]) { # 3 hour indication
    p <- p + labs(y="Absorbance (350nm)")
    if (fitted_y_vals[length(fitted_y_vals)] > 0.2) { # 3 hour and sufficient range indication
      # validation for cases where onset gets set to Inf
      if (isTRUE(onset == Inf)) {
        i <- get_ic95_3hr(multiplied$y)
        n <- which.min(abs(multiplied$y-i))
        onset <- multiplied$x[n]
      }
      p <- p + 
        geom_line(aes(y=horiz_y), color="gray50") + geom_line(aes(x=onset), color="gray50") +
        geom_point(aes(x=onset, y=horiz_y), color="orange1", size = 2) +
        labs(subtitle=paste("3hr Onset =", round(onset, digits=4)))
    }
  }
  
  if (fitted_y_vals[1] >= fitted_y_vals[length(fitted_y_vals)] & abs(fitted_y_vals[1]-fitted_y_vals[length(fitted_y_vals)]) > 0.1) { # 24 hour and sufficient range indication
    # validation for cases where onset gets set to Inf
    if (isTRUE(onset == Inf)) {
      i <- get_ic95(multiplied$y)
      n <- which.min(abs(multiplied$y-i))
      onset <- multiplied$x[n]
    }
    p <- p +
      geom_line(aes(y=horiz_y), color="gray50") + geom_line(aes(x=onset), color="gray50") +
      geom_point(aes(x=onset, y=horiz_y), color="orange1", size = 2) +
      labs(subtitle = paste("24hr Onset =", round(onset, digits=4)))
  }
  
  return(p)
}

add_ic50_to_plot <- function(onset_plot, ic50_x, ic50_y) {
  return(onset_plot + geom_point(aes(x=ic50_x, y=ic50_y), shape=18, color="orange1", size = 4))
}


#---------------------------------------#
# ONSET DATA FRAMES
#---------------------------------------#

build_onset_df_24hr <- function(parsed_table) {
  
  sample_list <- as.character(unique(parsed_table$Sample))
  buffer_list <- as.character(unique(parsed_table$Buffer))
  
  onset_df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(onset_df) <- c('Sample','Buffer','Onset', 'IC-50')
  
  for (sample in sample_list) {
    for (buffer in buffer_list) {
      xytable <- filter_basic_xytable(parsed_table, sample, buffer, "24hr")
      m <- get_drm_fitted_output(xytable)
      ic5 <- get_ic5(m$y_fitted)
      
      fake_ic50y <- fake_ic50_y_24hr(m$y_fitted)
      fake_ic50x <- get_ic50_x(fake_ic50y, m$b, m$c, m$d, m$e)
      
      onset <- solve_onset(y=ic5, b=m$b, c=m$c, d=m$d, e=m$e)
      # validation for cases where onset gets set to Inf
      if (isTRUE(onset == Inf)) {
        i <- get_ic5(xytable$y)
        n <- which.min(abs(xytable$y-i))
        onset <- xytable$x[n]
      }
      # validation for cases where IC-50 gets set to Inf
      if (isTRUE(fake_ic50x==Inf)) {
        i <- fake_ic50_y_24hr(xytable$y)
        n <- which.min(abs(xytable$y-i))
        fake_ic50x <- xytable$x[n]
      }
      new_row <- c(sample, buffer, onset, fake_ic50x)
      onset_df[nrow(onset_df)+1,] <- new_row
    }
  }
  onset_df[,3:4] %<>% lapply(function(x) as.numeric(x))
  return(onset_df)
}

dcast_onset_df_24hr <- function(onset_df) {
  onset_df <- onset_df[c("Sample","Buffer","Onset")]
  onset_df <- dcast(onset_df, factor(Buffer, as.character(unique(Buffer))) ~ Sample, value.var="Onset")
  colnames(onset_df)[1] <- "24hr Onset"
  return(onset_df)
}

dcast_ic50_df_24hr <- function(onset_df) {
  onset_df <- onset_df[c("Sample","Buffer","IC-50")]
  onset_df <- dcast(onset_df, factor(Buffer, as.character(unique(Buffer))) ~ Sample, value.var="IC-50")
  colnames(onset_df)[1] <- "24hr IC-50"
  return(onset_df)
}


## 3hr onset output
build_onset_df_3hr <- function(parsed_table) {
  
  sample_list <- as.character(unique(parsed_table$Sample))
  buffer_list <- as.character(unique(parsed_table$Buffer))
  
  onset_df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(onset_df) <- c('Sample','Buffer','Onset', 'IC-50')
  
  for (sample in sample_list) {
    for (buffer in buffer_list) {
      xytable <- filter_basic_xytable(parsed_table, sample, buffer, "3hr")
      m <- get_drm_fitted_output(xytable)
      ic95 <- get_ic95_3hr(m$y_fitted)
      
      fake_ic50y <- fake_ic50_y_3hr(m$y_fitted)
      fake_ic50x <- get_ic50_x(fake_ic50y, m$b, m$c, m$d, m$e)
      
      onset <- solve_onset(y=ic95, b=m$b, c=m$c, d=m$d, e=m$e)
      # validation for cases where onset gets set to Inf
      if (isTRUE(onset == Inf)) {
        i <- get_ic95_3hr(xytable$y)
        n <- which.min(abs(xytable$y-i))
        onset <- xytable$x[n]
      }
      # validation for cases where IC-50 gets set to Inf
      if (fake_ic50x==Inf) {
        i <- fake_ic50_y_3hr(xytable$y)
        n <- which.min(abs(xytable$y-i))
        fake_ic50x <- xytable$x[n]
      }
      new_row <- c(sample, buffer, onset, fake_ic50x)
      onset_df[nrow(onset_df)+1,] <- new_row
    }
  }
  onset_df[,3:4] %<>% lapply(function(x) as.numeric(x))
  return(onset_df)
}

dcast_onset_df_3hr <- function(onset_df) {
  onset_df <- onset_df[c("Sample","Buffer","Onset")]
  onset_df <- dcast(onset_df, factor(Buffer, as.character(unique(Buffer))) ~ Sample, value.var="Onset")
  colnames(onset_df)[1] <- "3hr Onset"
  return(onset_df)
}

dcast_ic50_df_3hr <- function(onset_df) {
  onset_df <- onset_df[c("Sample","Buffer","IC-50")]
  onset_df <- dcast(onset_df, factor(Buffer, as.character(unique(Buffer))) ~ Sample, value.var="IC-50")
  colnames(onset_df)[1] <- "3hr IC-50"
  return(onset_df)
}

#---------------------------------------#
# INVALIDATE ONSET BASED ON THRESHOLD
#---------------------------------------#

invalidated_onsets_24hr <- function(parsed_table) {
  
  sample_list <- as.character(unique(parsed_table$Sample))
  buffer_list <- as.character(unique(parsed_table$Buffer))
  
  onset_df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(onset_df) <- c('Sample','Buffer','Onset', 'IC-50')
  
  for (sample in sample_list) {
    for (buffer in buffer_list) {
      xytable <- filter_basic_xytable(parsed_table, sample, buffer, "24hr")
      m <- get_drm_fitted_output(xytable)
      ic5 <- get_ic5(m$y_fitted)
      
      fake_ic50y <- fake_ic50_y_24hr(m$y_fitted)
      fake_ic50x <- get_ic50_x(fake_ic50y, m$b, m$c, m$d, m$e)
      onset <- solve_onset(y=ic5, b=m$b, c=m$c, d=m$d, e=m$e)
      
      # validation for cases where onset gets set to Inf
      if (isTRUE(onset == Inf)) {
        i <- get_ic5(xytable$y)
        n <- which.min(abs(xytable$y-i))
        onset <- xytable$x[n]
      }
      # validation for cases where IC-50 gets set to Inf
      if (isTRUE(fake_ic50x == Inf)) {
        i <- fake_ic50_y_24hr(xytable$y)
        n <- which.min(abs(xytable$y-i))
        fake_ic50x <- xytable$x[n]
      }
      
      ## for 24hr: invalidation condition = "if all fitted data points fall within
      # a vertical range of 0.05, invalidate that onset"
      ## April 2020: changed validation to 0.1
      if (abs(m$y_fitted[1]-m$y_fitted[length(m$y_fitted)]) <= 0.1) {
        onset <- NA
        fake_ic50x <- NA
      }
      new_row <- c(sample, buffer, onset, fake_ic50x)
      onset_df[nrow(onset_df)+1,] <- new_row
    }
  }
  onset_df[,3:4] %<>% lapply(function(x) as.numeric(x))
  return(onset_df)
}


invalidated_onsets_3hr <- function(parsed_table) {
  
  sample_list <- as.character(unique(parsed_table$Sample))
  buffer_list <- as.character(unique(parsed_table$Buffer))
  
  onset_df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(onset_df) <- c('Sample','Buffer','Onset', 'IC-50')
  
  for (sample in sample_list) {
    for (buffer in buffer_list) {
      xytable <- filter_basic_xytable(parsed_table, sample, buffer, "3hr")
      m <- get_drm_fitted_output(xytable)
      ic95 <- get_ic95_3hr(m$y_fitted)
      
      fake_ic50y <- fake_ic50_y_3hr(m$y_fitted)
      fake_ic50x <- get_ic50_x(fake_ic50y, m$b, m$c, m$d, m$e)
      onset <- solve_onset(y=ic95, b=m$b, c=m$c, d=m$d, e=m$e)
      
      # validation for cases where onset gets set to Inf
      if (isTRUE(onset == Inf)) {
        i <- get_ic95_3hr(xytable$y)
        n <- which.min(abs(xytable$y-i))
        onset <- xytable$x[n]
      }
      # validation for cases where IC-50 gets set to Inf
      if (isTRUE(fake_ic50x==Inf)) {
        i <- fake_ic50_y_3hr(xytable$y)
        n <- which.min(abs(xytable$y-i))
        fake_ic50x <- xytable$x[n]
      }
      
      ## for 3hr: invalidation condition = "cases where all fitted data points remain within 
      # the range from 0 to 0.2"
      if (max(m$y_fitted)<=0.2 & min(m$y_fitted)>=0) {
        onset <- ''
        fake_ic50x <- ''
      }
      new_row <- c(sample, buffer, onset, fake_ic50x)
      onset_df[nrow(onset_df)+1,] <- new_row
    }
  }
  onset_df[,3:4] %<>% lapply(function(x) as.numeric(x))
  return(onset_df)
}


#------------------------------------------#
#  RMSE TABLE OUTPUT
#------------------------------------------#

build_rmse_table <- function(parsed_table, sample, buffer, onset_type) {
  xy <- filter_basic_xytable(parsed_table, sample, buffer, onset_type)
  m <- get_drm_fitted_output(xy)
  r2 <- calcualte_r_squared(y_raw = xy$y, y_fitted = m$y_fitted)
  xy_mult <- multiply_fitted_values(xy)
  ic5 <- get_ic5(xy_mult$y)
  ic95 <- get_ic95_3hr(xy_mult$y)
  upper <- m$d
  lower <- m$c
  rmse <- rmse(xy$y, m$y_fitted)
  fixed <- xy_mult$y[length(xy_mult$y)]
  #slope at ic-50
  ic50_y <- get_ic50_y(xy_mult$y, m$b, m$c, m$d, m$e, onset_type)
  ic50_x <- get_ic50_x(ic50_y, m$b, m$c, m$d, m$e)
  # validation for cases where IC-50 gets set to Inf
  if (isTRUE(ic50_x==Inf)) {
    if (onset_type=="24hr") {
      i <- fake_ic50_y_24hr(xy_mult$y)
      n <- which.min(abs(xy_mult$y-i))
      fake_ic50x <- xy_mult$x[n]
    } else {
      i <- fake_ic50_y_3hr(xy_mult$y)
      n <- which.min(abs(xy_mult$y-i))
      fake_ic50x <- xy_mult$x[n]
    }
  }
  ante_i <- which.min(abs(ic50_y-xy_mult$y))-1
  post_i <- which.min(abs(ic50_y-xy_mult$y))+1
  slope_ic50 <- (xy_mult$y[ante_i]-xy_mult$y[post_i])/(xy_mult$x[ante_i]-xy_mult$x[post_i])
  # QC
  onset_qc <- ''
  if (onset_type=="24hr") {
    onset <- solve_onset(ic5, m$b, m$c, m$d, m$e)
    # validation for cases where onset gets set to Inf
    if (isTRUE(onset == Inf)) {
      i <- get_ic5(xy_mult$y)
      n <- which.min(abs(xy_mult$y-i))
      onset <- xy_mult$x[n]
    }
    if (abs(xy_mult$y[1]-xy_mult$y[length(xy_mult$y)]) <= 0.1) {
      onset <- NA
      ic50_x <- NA
      slope_ic50 <- NA
      onset_qc <- '> 36'
    }
    df <- data.frame("Sample"=sample, "Buffer"=buffer, "Onset"=onset, "Onset.Q"=onset_qc,"IC-50"=ic50_x, "Slope at IC-50"=slope_ic50, "RMSE"=rmse, "R-sq" = r2, "Upper bound"=upper, "Lower bound"=lower, "Fixed bound"=fixed)
  } else {
    onset <- solve_onset(ic95, m$b, m$c, m$d, m$e)
    # validation for cases where onset gets set to Inf
    if (isTRUE(onset == Inf)) {
      i <- get_ic95_3hr(xy_mult$y)
      n <- which.min(abs(xy_mult$y-i))
      onset <- xy_mult$x[n]
    }
    if (xy_mult$y[length(xy_mult$y)]<0.2) {
      onset <- NA
      slope_ic50 <- NA
      onset_qc <- '> 36'
    }
    df <- data.frame("Sample"=sample, "Buffer"=buffer, "Onset"=onset, "Onset.Q"=onset_qc, "Midway Slope"=slope_ic50, "RMSE"=rmse, "R-sq" = r2, "Upper bound"=upper, "Lower bound"=lower, "Fixed bound"=fixed)
  }
  
  return(df)
}

build_rmse_table_all <- function(parsed_table, onset_type=c('24hr','3hr')) {
  sample_list <- as.character(unique(parsed_table$Sample))
  buffer_list <- as.character(unique(parsed_table$Buffer))
  
  df <- data.frame(matrix(nrow=0,ncol=0))
  
  for (sample in sample_list) {
    for (buffer in buffer_list) {
      new_row <- build_rmse_table(parsed_table, sample, buffer, onset_type)[1,]
      df <- rbind(df, new_row)
    }
  }
  colnames(df)[colnames(df)=='Onset'] <- paste0(onset_type, " Onset")
  return(df)
}

round_digits4 <- function(dataframe) {
  nums <- vapply(dataframe, is.numeric, FUN.VALUE = logical(1))
  dataframe[,nums] <- round(dataframe[,nums], digits = 4)
  return(dataframe)
}

calcualte_r_squared <- function(y_raw, y_fitted) {
  y_mean <- mean(y_raw)
  tss <- sum((y_raw - y_mean)^2)
  rss <- sum((y_raw - y_fitted)^2)
  r2 <- 1 - (rss/tss)
  return(r2)
}


#---------------------------------------#
# GROUPED PLOTS WITH CURVE FIT
#---------------------------------------#

curvefit_plot_by_sample <- function(parsed_table, sample, onset_type = c("24hr","3hr")) {
  
  #refactorize Buffer values
  parsed_table$Buffer <- factor(parsed_table$Buffer, levels = as.character(unique(parsed_table$Buffer)))
  
  parsed_table <- parsed_table[parsed_table$Sample==sample,]
  parsed_table$`PEG %` <- as.numeric(as.character(parsed_table$`PEG %`))
  
  line_df <- data.frame(matrix(nrow = 0, ncol = 3))
  onset_df <- data.frame(matrix(nrow = 0, ncol = 3))
  
  ## fitted values PER BUFFER GROUP ##
  parsed_table$Fitted_Values <- NA
  buffer_list <- as.character(unique(parsed_table$Buffer))
  
  for (buffer in buffer_list) {
    temp <- parsed_table[parsed_table$Buffer==buffer,]
    x <- temp$`PEG %`
    if (onset_type == "24hr") {
      y <- temp$`MAB=(Raw/[MAV*0.561])`
    } else {
      y <- temp$Raw
    }
    xy <- data.frame(x=x, y=y)
    xy_mult <- multiply_fitted_values(xy) # larger df for geom_line later
    xy_mult$Buffer <- buffer # for identifying that chunk later for geom_line
    
    #bind to bigger data frame for referencing outside of loop
    line_df <- rbind(line_df, xy_mult)
    colnames(line_df) <- c('x','y','Buffer')
    
    #model
    mod <- get_drm_fitted_output(xy)
    
    #small df containing onset values
    if (onset_type == "24hr") {
      on_y <- get_ic5(mod$y_fitted)
    } else {
      on_y <- get_ic95_3hr(mod$y_fitted)
    }
    on <- solve_onset(on_y, mod$b, mod$c, mod$d, mod$e)
    # validation for cases where onset gets set to Inf
    if (isTRUE(on == Inf)) {
      if (onset_type == "24hr") {
        i <- get_ic5(xy_mult$y)
        n <- which.min(abs(xy_mult$y-i))
        on <- xy_mult$x[n]
      } else {
        i <- get_ic95_3hr(xy_mult$y)
        n <- which.min(abs(xy_mult$y-i))
        on <- xy_mult$x[n]
      }
    }
    onset_list <- list(on, on_y, buffer)
    onset_df[nrow(onset_df)+1,] <- onset_list
    colnames(onset_df) <- c('onset_x','onset_y','Buffer')
    
  }
  
  # invalidated onset df:
  if (onset_type == "24hr") {
    invalidated_onsets <- invalidated_onsets_24hr(parsed_table) 
  } else {
    invalidated_onsets <- invalidated_onsets_3hr(parsed_table) 
  }
  invalidated_onsets <- join(invalidated_onsets, onset_df, by="Buffer")
  for (n in 1:nrow(invalidated_onsets)) {
    if (is.na(invalidated_onsets$Onset[n])) {
      invalidated_onsets$onset_y[n] <- NA
    }
  }
  
  if (onset_type == "24hr") {
    p <- ggplot(data=parsed_table, aes(x=as.numeric(as.character(parsed_table$`PEG %`)), y = as.numeric(as.character(parsed_table$`MAB=(Raw/[MAV*0.561])`)), colour=Buffer)) + 
      geom_point() + geom_line(data=line_df, aes(x=line_df$x, y=line_df$y)) + 
      geom_point(data = invalidated_onsets, aes(x=Onset, y=onset_y), shape=8, size=4, na.rm=TRUE) # remove invalidated onset values
    p <- p + labs(x = "PEG 3350%", y = "Protein Concentration [mg/mL]", title=paste(sample, "(Curve Fit)")) + 
      theme(plot.title = element_text(hjust = 0.5), legend.position="top", 
            axis.line = element_line(size = 2, colour = "black"),
            axis.text = element_text(face = "bold", color = "black", size = 10),
            axis.ticks.length = unit(.3, "cm")) + 
      ylim(-0.05, 0.5) +
      scale_x_continuous(breaks=c(4,6.9,9.8,12.8,15.7,18.6,21.5,24.4,27.2,30.2,33.1,36))
    
    
  } else {
    p <- ggplot(data=parsed_table, aes(x=as.numeric(as.character(parsed_table$`PEG %`)), y = as.numeric(as.character(parsed_table$Raw)), colour=Buffer)) + 
      geom_point() + geom_line(data=line_df, aes(x=line_df$x, y=line_df$y)) + 
      geom_point(data = invalidated_onsets, aes(x=Onset, y=onset_y), shape=8, size=4)
    p <- p + labs(x = "PEG 3350%", y = "Absorbance (350nm)", title=paste(sample, "(Curve Fit)")) + 
      theme(plot.title = element_text(hjust = 0.5), legend.position="top", 
            axis.line = element_line(size = 2, colour = "black"),
            axis.text = element_text(face = "bold", color = "black", size = 10),
            axis.ticks.length = unit(.3, "cm")) + 
      ylim(-0.05, 2) +
      scale_x_continuous(breaks=c(4,6.9,9.8,12.8,15.7,18.6,21.5,24.4,27.2,30.2,33.1,36))
    
  }
  
  return(p)
}


curvefit_plot_by_buffer <- function(parsed_table, buffer, onset_type = c("24hr","3hr")) {
  
  parsed_table <- parsed_table[parsed_table$Buffer==buffer,]
  parsed_table$`PEG %` <- as.numeric(as.character(parsed_table$`PEG %`))
  
  line_df <- data.frame(matrix(nrow = 0, ncol = 3))
  onset_df <- data.frame(matrix(nrow = 0, ncol = 3))
  
  ## fitted values PER SAMPLE GROUP ##
  parsed_table$Fitted_Values <- NA
  sample_list <- as.character(unique(parsed_table$Sample))
  for (sample in sample_list) {
    temp <- parsed_table[parsed_table$Sample==sample,]
    x <- temp$`PEG %`
    if (onset_type == "24hr") {
      y <- temp$`MAB=(Raw/[MAV*0.561])`
    } else {
      y <- temp$Raw
    }
    xy <- data.frame(x=x, y=y)
    xy_mult <- multiply_fitted_values(xy) # larger df for geom_line later
    xy_mult$Sample <- sample # for identifying that chunk later for geom_line
    
    #bind to bigger data frame for referencing outside of loop
    line_df <- rbind(line_df, xy_mult)
    colnames(line_df) <- c('x','y','Sample')
    
    #model
    mod <- get_drm_fitted_output(xy)
    
    #small df containing onset values
    if (onset_type == "24hr") {
      on_y <- get_ic5(mod$y_fitted)
    } else {
      on_y <- get_ic95_3hr(mod$y_fitted)
    }
    on <- solve_onset(on_y, mod$b, mod$c, mod$d, mod$e)
    # validation for cases where onset gets set to Inf
    if (isTRUE(on == Inf)) {
      if (onset_type == "24hr") {
        i <- get_ic5(xy_mult$y)
        n <- which.min(abs(xy_mult$y-i))
        on <- xy_mult$x[n]
      } else {
        i <- get_ic95_3hr(xy_mult$y)
        n <- which.min(abs(xy_mult$y-i))
        on <- xy_mult$x[n]
      }
    }
    onset_list <- list(on, on_y, sample)
    onset_df[nrow(onset_df)+1,] <- onset_list
    colnames(onset_df) <- c('onset_x','onset_y','Sample')
    
  }
  
  # invalidated onset df:
  if (onset_type == "24hr") {
    invalidated_onsets <- invalidated_onsets_24hr(parsed_table) 
  } else {
    invalidated_onsets <- invalidated_onsets_3hr(parsed_table) 
  }
  invalidated_onsets <- join(invalidated_onsets, onset_df, by="Sample")
  for (n in 1:nrow(invalidated_onsets)) {
    if (is.na(invalidated_onsets$Onset[n])) {
      invalidated_onsets$onset_y[n] <- NA
    }
  }
  
  if (onset_type == "24hr") {
    p <- ggplot(data=parsed_table, aes(x=as.numeric(as.character(parsed_table$`PEG %`)), y = as.numeric(as.character(parsed_table$`MAB=(Raw/[MAV*0.561])`)), colour=Sample)) + 
      geom_point() + geom_line(data=line_df, aes(x=line_df$x, y=line_df$y)) + 
      geom_point(data = invalidated_onsets, aes(x=Onset, y=onset_y), shape=8, size=4, na.rm = TRUE)
    
    p <- p + labs(x = "PEG 3350%", y = "Protein Concentration [mg/mL]", title=paste(buffer, "(Curve Fit)")) + 
      theme(plot.title = element_text(hjust = 0.5), legend.position="top", 
            axis.line = element_line(size = 2, colour = "black"),
            axis.text = element_text(face = "bold", color = "black", size = 10),
            axis.ticks.length = unit(.3, "cm")) + 
      ylim(-0.05, 0.5) +
      scale_x_continuous(breaks=c(4,6.9,9.8,12.8,15.7,18.6,21.5,24.4,27.2,30.2,33.1,36))
    
  } else {
    p <- ggplot(data=parsed_table, aes(x=as.numeric(as.character(parsed_table$`PEG %`)), y = as.numeric(as.character(parsed_table$Raw)), colour=Sample)) + 
      geom_point() + geom_line(data=line_df, aes(x=line_df$x, y=line_df$y)) + 
      geom_point(data = onset_df, aes(x=onset_x, y=onset_y), shape=8, size=4)
    
    p <- p + labs(x = "PEG 3350%", y = "Absorbance (350nm)", title=paste(buffer, "(Curve Fit)")) + 
      theme(plot.title = element_text(hjust = 0.5), legend.position="top", 
            axis.line = element_line(size = 2, colour = "black"),
            axis.text = element_text(face = "bold", color = "black", size = 10),
            axis.ticks.length = unit(.3, "cm")) + 
      ylim(-0.05, 2) +
      scale_x_continuous(breaks=c(4,6.9,9.8,12.8,15.7,18.6,21.5,24.4,27.2,30.2,33.1,36))
    
  }
  
  return(p)
}


## decrease legend size
addSmallLegend <- function(myPlot, pointSize = 1.5, textSize = 8, spaceLegend = 0.01) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0))
}


return_list_of_plots_sample <- function(parsed_table, onset_type=c('24hr','3hr')) {
  
  sample_list <- as.character(unique(parsed_table$Sample))
  plot_list <- list()
  for (sample in sample_list) {
    plot <- suppressWarnings(build_plot_by_sample_normal(parsed_table, sample, onset_type))
    plot_list[[which(sample_list==sample)]] <- plot
  }
  return(plot_list)
}

return_list_of_plots_buffer <- function(parsed_table, onset_type=c('24hr','3hr')) {
  
  buffer_list <- as.character(unique(parsed_table$Buffer))
  plot_list <- list()
  for (buffer in buffer_list) {
    plot <- suppressWarnings(build_plot_by_buffer_normal(parsed_table, buffer, onset_type))
    plot_list[[which(buffer_list==buffer)]] <- plot
  }
  return(plot_list)
}


return_list_of_curvefits_sample <- function(parsed_table, onset_type=c('24hr','3hr')) {
  
  sample_list <- as.character(unique(parsed_table$Sample))
  plot_list <- list()
  for (sample in sample_list) {
    plot <- suppressWarnings(curvefit_plot_by_sample(parsed_table, sample, onset_type))
    plot_list[[which(sample_list==sample)]] <- plot
  }
  return(plot_list)
}

return_list_of_curvefits_buffer <- function(parsed_table, onset_type=c('24hr','3hr')) {
  
  buffer_list <- as.character(unique(parsed_table$Buffer))
  plot_list <- list()
  for (buffer in buffer_list) {
    plot <- suppressWarnings(curvefit_plot_by_buffer(parsed_table, buffer, onset_type))
    plot_list[[which(buffer_list==buffer)]] <- plot
  }
  return(plot_list)
}


#---------------------------------------#
# INFO TABLE FOR XLSX REPORT
#---------------------------------------#

generate_info_table <- function(title, onset_type, acq_date) {
  df <- data.frame("Title" = title, "Onset Type" = onset_type, "Acquisition Date" = acq_date, "Date of Analysis" = format(Sys.Date(), format="%Y-%m-%d"), check.names = FALSE)
  return(df)
}

#---------------------------------------#
# UPLOAD-RELATED
#---------------------------------------#

merge_valid_invalid_results <- function(invalid_table, valid_table) {
  df <- join(valid_table, invalid_table)
  df$Validation[which(is.na(df$Validation))] <- 'valid'
  df <- df[which(df$Validation != 'invalid'),]
  return(df)
}


#------------------------------------------#
# PARSE DATA TAKE TWO! REVISITED MARCH 2020
#------------------------------------------#

parse_hplate_8quads <- function(plate, plate_info) { # always 384-well plate
  df <- data.frame(matrix(nrow = 0, ncol = 5))
  colnames(df) <- c("Quadrant","Number", "Plate_Row", "Buffer_Role", "PEG %")
  #peg list:
  p <- plate_info$`PEG %`
  # buffer role index:
  b = 0
  ## quadrant 1 and 3:
  for (q in seq(1,7,by=2)) { # q = plate row
    vals_q1 <- t(plate[q, seq(1,23, by=2)])[1:12]
    df[(nrow(df)+1):(nrow(df)+12),'Number'] <- vals_q1
    df[(nrow(df)-11):(nrow(df)),'Plate_Row'] <- rep(q,12)
    b = b+1
    df[(nrow(df)-11):(nrow(df)),'Buffer_Role'] <- rep(b,12)
    df[(nrow(df)-11):(nrow(df)),'PEG %'] <- p
    
    vals_q3 <- t(plate[q, seq(2,24, by=2)])[1:12]
    df[(nrow(df)+1):(nrow(df)+12),'Number'] <- vals_q3
    df[(nrow(df)-11):(nrow(df)),'Plate_Row'] <- rep(q,12)
    df[(nrow(df)-11):(nrow(df)),'Buffer_Role'] <- rep(b,12)
    df[(nrow(df)-11):(nrow(df)),'PEG %'] <- p
    
  }
  df[1:nrow(df),'Quadrant'] <- rep.int(c(rep(1, 12),rep(3,12)),times=4)
  b = 0 # reset buffer role index
  
  # quadrant 5 and 7:
  for (q in seq(2,8,by=2)) { # q = plate row
    vals_q5 <- t(plate[q, seq(1,23, by=2)])[1:12]
    df[(nrow(df)+1):(nrow(df)+12),'Number'] <- vals_q5
    df[(nrow(df)-11):(nrow(df)),'Plate_Row'] <- rep(q,12)
    b = b+1
    df[(nrow(df)-11):(nrow(df)),'Buffer_Role'] <- rep(b,12)
    df[(nrow(df)-11):(nrow(df)),'PEG %'] <- p
    
    vals_q7 <- t(plate[q, seq(2,24, by=2)])[1:12]
    df[(nrow(df)+1):(nrow(df)+12),'Number'] <- vals_q7
    df[(nrow(df)-11):(nrow(df)),'Plate_Row'] <- rep(q,12) 
    df[(nrow(df)-11):(nrow(df)),'Buffer_Role'] <- rep(b,12)
    df[(nrow(df)-11):(nrow(df)),'PEG %'] <- p
  }
  
  df[97:192,'Quadrant'] <- rep.int(c(rep(5, 12),rep(7,12)),times=4)
  b = 0 # reset buffer role index
  
  ## quadrant 2 and 4:
  for (q in seq(9,15,by=2)) { # q = plate row
    vals_q2 <- t(plate[q, seq(1,23, by=2)])[1:12]
    df[(nrow(df)+1):(nrow(df)+12),'Number'] <- vals_q2
    df[(nrow(df)-11):(nrow(df)),'Plate_Row'] <- rep(q,12)
    b = b+1
    df[(nrow(df)-11):(nrow(df)),'Buffer_Role'] <- rep(b,12)
    df[(nrow(df)-11):(nrow(df)),'PEG %'] <- p
    
    vals_q4 <- t(plate[q, seq(2,24, by=2)])[1:12]
    df[(nrow(df)+1):(nrow(df)+12),'Number'] <- vals_q4
    df[(nrow(df)-11):(nrow(df)),'Plate_Row'] <- rep(q,12)
    df[(nrow(df)-11):(nrow(df)),'Buffer_Role'] <- rep(b,12)
    df[(nrow(df)-11):(nrow(df)),'PEG %'] <- p
  }
  
  df[193:288,'Quadrant'] <- rep.int(c(rep(2, 12),rep(4,12)),times=4)
  b = 0 # reset buffer role index
  
  # quadrant 6 and 8:
  for (q in seq(10,16,by=2)) { # q = plate row
    vals_q6 <- t(plate[q, seq(1,23, by=2)])[1:12]
    df[(nrow(df)+1):(nrow(df)+12),'Number'] <- vals_q6
    df[(nrow(df)-11):(nrow(df)),'Plate_Row'] <- rep(q,12)
    b = b+1
    df[(nrow(df)-11):(nrow(df)),'Buffer_Role'] <- rep(b,12)
    df[(nrow(df)-11):(nrow(df)),'PEG %'] <- p
    
    vals_q8 <- t(plate[q, seq(2,24, by=2)])[1:12]
    df[(nrow(df)+1):(nrow(df)+12),'Number'] <- vals_q8
    df[(nrow(df)-11):(nrow(df)),'Plate_Row'] <- rep(q,12)
    df[(nrow(df)-11):(nrow(df)),'Buffer_Role'] <- rep(b,12)
    df[(nrow(df)-11):(nrow(df)),'PEG %'] <- p
  }
  
  df[289:384,'Quadrant'] <- rep.int(c(rep(6, 12),rep(8,12)),times=4)
  
  # Ensure Number is numeric
  df$Number <- as.numeric(df$Number)
  
  return(df)
}


## Function to merge the parsed_half_plate with the plate info sheet
merge_hplate_plateinfo <- function(parsed_hplate, plate_info) {
  plate_info$Buffer_Role <- append(rep(c(1,2,3,4),times=2), rep(NA,times=4))
  df <- join(parsed_hplate, plate_info[c('Quadrant','Sample Name')], by="Quadrant")
  df <- join(df, plate_info[1:4,c('Buffer_Role','96w Plate Buffer')], by=c("Buffer_Role"))
  df <- join(df, plate_info[c('Quadrant','MAV')], by="Quadrant")
  
  df <- df[c('Quadrant','Sample Name','96w Plate Buffer','PEG %','MAV','Number')]
  colnames(df) <- c('Quadrant','Sample','Buffer','PEG %','MAV','Raw')
  df <- df[order(df$Quadrant),]
  return(df)
}

transform_parsedtable_to24hr <- function(parsed_table) { #parsed_table = merge_hplate_plateinfo(parse_hplate_8quads(plate, plate_info), plate_info)
  parsed_table$`MAB=(Raw/[MAV*0.561])` <- (parsed_table$Raw)/(parsed_table$MAV*0.561)
  return(parsed_table)
}

parse_fplate_4quads <- function(plate, plate_info) { # always 384-well plate; need to capture plate first
  df <- data.frame(matrix(nrow = 0, ncol = 5))
  colnames(df) <- c("Quadrant","Number", "Plate_Row", "Buffer_Role", "PEG %")
  #peg list:
  p <- plate_info$`PEG %`
  # buffer role index:
  b = 0
  ## quadrant 1 and 2:
  for (q in seq(1,15,by=2)) { # q = plate row
    vals_q1 <- t(plate[q, seq(1,23, by=2)])[1:12]
    df[(nrow(df)+1):(nrow(df)+12),'Number'] <- vals_q1
    df[(nrow(df)-11):(nrow(df)),'Plate_Row'] <- rep(q,12)
    b = b+1
    df[(nrow(df)-11):(nrow(df)),'Buffer_Role'] <- rep(b,12)
    df[(nrow(df)-11):(nrow(df)),'PEG %'] <- p
    
    vals_q2 <- t(plate[q, seq(2,24, by=2)])[1:12]
    df[(nrow(df)+1):(nrow(df)+12),'Number'] <- vals_q2
    df[(nrow(df)-11):(nrow(df)),'Plate_Row'] <- rep(q,12)
    df[(nrow(df)-11):(nrow(df)),'Buffer_Role'] <- rep(b,12)
    df[(nrow(df)-11):(nrow(df)),'PEG %'] <- p
    
  }
  df[1:nrow(df),'Quadrant'] <- rep.int(c(rep(1, 12),rep(2,12)),times=8)
  b = 0 # reset buffer role index
  
  ## quadrant 3 and 4:
  for (q in seq(2,16,by=2)) { # q = plate row
    vals_q3 <- t(plate[q, seq(1,23, by=2)])[1:12]
    df[(nrow(df)+1):(nrow(df)+12),'Number'] <- vals_q3
    df[(nrow(df)-11):(nrow(df)),'Plate_Row'] <- rep(q,12)
    b = b+1
    df[(nrow(df)-11):(nrow(df)),'Buffer_Role'] <- rep(b,12)
    df[(nrow(df)-11):(nrow(df)),'PEG %'] <- p
    
    vals_q4 <- t(plate[q, seq(2,24, by=2)])[1:12]
    df[(nrow(df)+1):(nrow(df)+12),'Number'] <- vals_q4
    df[(nrow(df)-11):(nrow(df)),'Plate_Row'] <- rep(q,12)
    df[(nrow(df)-11):(nrow(df)),'Buffer_Role'] <- rep(b,12)
    df[(nrow(df)-11):(nrow(df)),'PEG %'] <- p
    
  }
  df[193:nrow(df),'Quadrant'] <- rep.int(c(rep(3, 12),rep(4,12)),times=8)
  
  # Ensure Number is numeric
  df$Number <- as.numeric(df$Number)
  
  return(df) 
}

## Function to merge the parsed_full_plate with the plate info sheet
merge_fplate_plateinfo <- function(parsed_fplate, plate_info) {
  plate_info$Buffer_Role <- append(c(1:8), rep(NA,times=4))
  df <- join(parsed_fplate, plate_info[c('Quadrant','Sample Name')], by="Quadrant")
  df <- join(df, plate_info[1:8,c('Buffer_Role','96w Plate Buffer')], by=c("Buffer_Role"))
  df <- join(df, plate_info[c('Quadrant','MAV')], by="Quadrant")
  
  df <- df[c('Quadrant','Sample Name','96w Plate Buffer','PEG %','MAV','Number')]
  colnames(df) <- c('Quadrant','Sample','Buffer','PEG %','MAV','Raw')
  df <- df[order(df$Quadrant),]
  return(df)
}


#---------------------------------------#
# BENCHLING UPLOAD
#---------------------------------------#

# commit for testing on Aug 11

build_benchling_upload_file <- function(rmse_full_table, exp_info_table, onset_type=c('24hr','3hr')) {
  df <- rmse_full_table
  experiment_name <- exp_info_table$Title[1]
  acq_date <- exp_info_table$`Acquisition Date`[1]
  date_analysis <- exp_info_table$`Date of Analysis`[1]
  # insert new columns
  df$assay <- "Microdrop"
  df$`experiment name` <- experiment_name
  df$`acquisition date` <- acq_date
  df$`date of analysis` <- date_analysis
  df$`onset type` <- onset_type
  #rename column headers
  if (onset_type == '24hr') {
    colnames(df) <- c('Sample ID','buffer','onset','onset qualifier','IC-50',
                      'slope at IC-50','root mean square error','R-squared',
                      'upper bound','lower bound','fixed bound', 
                      'assay','experiment name','acquisition date',
                      'date of analysis','onset type')
    df <- df[c(1,12:16,2:11)]
    df[,c(8,10)] <- round(df[,c(8,10)], 2)
    df[,c(11:16)] <- round(df[,c(11:16)], 4)
  } else {
    colnames(df) <- c('Sample ID','buffer','onset','onset qualifier','midway slope',
                      'root mean square error','R-squared',
                      'upper bound','lower bound','fixed bound',
                      'assay','experiment name','acquisition date',
                      'date of analysis','onset type')
    df <- df[c(1,11:15,2:10)]
    df[,8] <- round(df[,8], 2)
    df[,c(10:15)] <- round(df[,c(10:15)], 4)
  }
  return(df)
}