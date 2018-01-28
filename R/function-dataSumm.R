#' dataSumm function
#'
#' This function outputs a list with data summary, factor summary and plots
#' @df is your model file, simple as that
#' dataSumm()

dataSumm <- function(df)
{

  # Produce metadata --------------------------------------------------------
  dfFactors.na.perc <- sapply(df, function(x) percent(sum(is.na(x)) / length(x)))
  dfFactors.na <- sapply(df, function(x) sum(is.na(x)))
  dfFactors.unique <- sapply(df, function(x) length(unique(x)))
  dfFactors.class <- unname(sapply(df, class))
  dfFactors.names <- names(df)
  dfFactors.id <- 1:length(dfFactors.names)
  df.columns <- ncol(df)
  df.rows <- nrow(df)

  dfFactors.plotdata = list()
  for (i in dfFactors.id) {

    if (dfFactors.class[i] == 'factor' ||
        dfFactors.class[i] == 'integer' & dfFactors.unique[i] <= 20){

      dfFactors.plotdata[[i]] <- aggregate(df[, i], by = list(df[, i]), FUN = length)
      dfFactors.plotdata[[i]][, 3] <- dfFactors.plotdata[[i]][2] / length(df[, i])

    } else if (dfFactors.class[i] == 'numeric' ||
               dfFactors.class[i] == 'integer' & dfFactors.unique[i] > 20){

      contfactor.vector <- as.vector(df[, i])
      bin.size <- (2 * IQR(contfactor.vector, na.rm = TRUE) / length(contfactor.vector)^(1/3))
      contfactor.df <- as.data.frame(sapply(contfactor.vector, function(x) ceiling(x / bin.size) * bin.size))

      dfFactors.plotdata[[i]] <- aggregate(contfactor.df[, 1], by = list(contfactor.df[, 1]), FUN = length)
      dfFactors.plotdata[[i]][, 3] <- dfFactors.plotdata[[i]][2] / length(df[, i])

    }

    colnames(dfFactors.plotdata[[i]]) <- c('Band', 'Count', 'Proportion')

  }

  # OUTPUT - data summary ---------------------------------------------------
  df.summary <- data.frame('Label' = as.character(), 'Value' = as.character(), stringsAsFactors = FALSE)
  df.summary[1, ] <- c('Project name:', paste0(project.name, ' v', project.version))
  df.summary[2, ] <- c('Date:', project.date)
  df.summary[3, ] <- c('Number of factors:', df.columns)
  df.summary[4, ] <- c('Number of rows:', df.rows)
  df.summary[5, ] <- c('Unique identifier:', df.pkey)


  # OUTPUT - factor summary -------------------------------------------------
  dfFactors.summary <- data.frame(dfFactors.id, dfFactors.names, dfFactors.class, dfFactors.unique, dfFactors.na, dfFactors.na.perc)
  colnames(dfFactors.summary) <- c('Factor ID', 'Name', 'Data type', 'Unique values', 'NA values', '% NA values')


  # OUTPUT - produce plots -----------------------------------------------------------
  df.plot <- lapply(dfFactors.id, function(x)
    ggplot(dfFactors.plotdata[[x]], aes(Band, Proportion))
    + geom_bar(stat = 'identity', colour = 'black', fill = 'lightsteelblue2')
    + ggtitle(paste0(x, '. ', dfFactors.names[x], ' (', dfFactors.class[x], ') [', dfFactors.na.perc[x], ' NA]'))
    + theme_classic()
    + theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 10))
    + scale_y_continuous(labels=scales::percent)
  )
  margin = theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  df.multi_plot <- marrangeGrob(df.plot,
                                nrow = 4,
                                ncol = 2,
                                grobs = lapply(df.plot , '+', margin),
                                top = NULL
  )


  # OUTPUT - final ----------------------------------------------------------
  list(
    data = df.summary,
    factor = dfFactors.summary,
    plots = df.multi_plot
  )

}
