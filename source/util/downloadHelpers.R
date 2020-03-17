library(webshot)
library(plyr)
library(tibble)
library(manipulateWidget)

downloadWidget <- function(fname, func) {
  downloadHandler(
    filename = function() {
      paste0(Sys.time(), "-", fname, ".html")
    },
    content = function(file) {
      htmlwidgets::saveWidget(func(), file = file)
    }
  )
}

downloadWidget_ <- function(fname, widget) {
  downloadHandler(
    filename = function() {
      paste0(Sys.time(), "-", fname, ".html")
    },
    content = function(file) {
      htmlwidgets::saveWidget(widget, file = file)
    }
  )
}

downloadWidgets <- function(fname, ...) {
  funcs <- list(...)
  downloadHandler(
    filename = function() {
      paste0(Sys.time(), fname(), ".zip")
    },
    content = function(file) {
      dir <- tempdir()
      oldwd <- getwd()
      setwd(dir)

      tempNames <- sapply(names(funcs), function(nm) {
        try({
          tmpName <- paste0("chart-", nm, ".html")
          htmlwidgets::saveWidget(funcs[[nm]](), file = tmpName)
          tmpName
        }, silent = T)
      })

      zip(file, tempNames)

      setwd(oldwd)
    }
  )
}

replacemaorifun <- function(x) {
  str_replace_all(x, "\u0101ori", "aori")
}

downloadCsv <- function(fname, header, func, est.type = NULL) {
  downloadHandler(
    filename = function() {
      paste0(Sys.time(), replacemaorifun(fname()), ".csv")},
    content = function(file) {
      df <- func()
      df[is.na(df)] <- ""
      

      #In all CSV files: Maori macron is displaying as ā - fix or show without macron if tricky
      df[] <- lapply(df, function(x) replacemaorifun(x))

      #In columns with CI values in it - split out into two columns, add ".low" and ".high" to relating column
      cicols <- df %>% dplyr::select(ends_with(".CI"))
      ci.low.fun.process <- function(x) ifelse(is.na(x) == FALSE, trimws(substring(x, (regexpr('[(]', x))[1]+2, (regexpr('[-]', x))[1]-2)), x)
      ci.low.fun <- function(x) lapply(gsub(",", "", x), ci.low.fun.process) 
      
      ci.high.fun.process <- function(x) ifelse(is.na(x) == FALSE, trimws(substring(x, (regexpr('[-]', x))[1]+2, (regexpr('[)]', x))[1]-2)), x)
      ci.high.fun <- function(x) lapply(gsub(",", "", x), ci.high.fun.process)   
      
      for (ciname in names(cicols))
      {
        ci <- unlist(df[ciname], use.names = FALSE)
        df <- add_column(df, newaddedforCI = ci, .after = ciname)
        ci.low.name <- paste0(ciname, ".low")
        ci.high.name <- paste0(ciname, ".high")
        colnames(df)[colnames(df) == ciname] <- ci.low.name
        colnames(df)[colnames(df) == 'newaddedforCI'] <- ci.high.name
        df[ci.low.name] <- unlist(lapply(df[ci.low.name], ci.low.fun), use.names = FALSE)
        df[ci.high.name] <- unlist(lapply(df[ci.high.name], ci.high.fun), use.names = FALSE)
      }
      
      if(!is.null(est.type))
      {
        if(est.type() == "Mean")
        {
          names(df)[names(df) == "total"] <- "average"
          names(df)[names(df) == "total.CI.low"] <- "average.low.CI"
          names(df)[names(df) == "total.CI.high"] <- "average.high.CI"
        }
        else
        {
          names(df)[names(df) == "total.CI.low"] <- "total.low.CI"
          names(df)[names(df) == "total.CI.high"] <- "total.high.CI"
        }
      }
      
      
      names(df)[names(df) == "male.CI.low"] <- "male.low.CI"
      names(df)[names(df) == "male.CI.high"] <- "male.high.CI"
      names(df)[names(df) == "female.CI.low"] <- "female.low.CI"
      names(df)[names(df) == "female.CI.high"] <- "female.high.CI"
      names(df)[names(df) == "estimated.number.CI.low"] <- "estimated.number.low.CI"
      names(df)[names(df) == "estimated.number.CI.high"] <- "estimated.number.high.CI"
      names(df)[names(df) == "adjusted.rate.ratio.CI.low"] <- "adjusted.rate.ratio.low.CI"
      names(df)[names(df) == "adjusted.rate.ratio.CI.high"] <- "adjusted.rate.ratio.high.CI"
      

      if("estimated.number" %in% names(df))
      {
        df$estimated.number <- gsub(",","",df$estimated.number)
      }
      

      if(length(header) > 0)
      {
        colnamelist <- colnames(df)
        colnames(df) <- NULL
        df.csv  <- rbind(colnamelist , df)
        for (h in header)
        {
          head <- h()

          head <- replacemaorifun(head)
          if(head != "Subpopulation: Adults") #Default value, not need to add to CSV
            
            df.csv <- rbind(c(head, rep(NA, dim(df.csv)[2]-length(head))), df.csv)

        }

        df.csv[is.na(df.csv)] <- ""
        print(is.na(df.csv))
        write.table(df.csv, file=file, row.names = FALSE, col.names =  FALSE,  sep=",", quote = FALSE)
      }
      else
      {
        df.csv <- df
        df.csv[is.na(df.csv)] <- ""
        write.csv(df.csv, file=file, row.names = FALSE, quote = FALSE)
      }
    }
  )
}

downloadWidgetPNG <- function(fname, func) {
  downloadHandler(
    filename = function() {
      paste0(Sys.time(), "-", fname, ".png")
    },
    content = function(file) {
      tmpName <-
        tempfile(pattern = "webshot-temp-html-",
                 tmpdir = ".",
                 fileext = ".html")
      htmlwidgets::saveWidget(func(), file = tmpName)
      webshot(tmpName, file = file, cliprect = "viewport")
      if (file.exists(tmpName))
        file.remove(tmpName)
    }
  )
}