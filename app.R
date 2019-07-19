#-------------------------------------------------------------------------------
# Name - EndoC_Beta
# Desc - Shiny App to analyse Endoc Beta experiements
#        (gene silencing and insulin secretion)
# Version - 1.2.0
# Date - 2018/02/06
# Author - Mickaël Canouil, Ph.D.
#-------------------------------------------------------------------------------

author_name <- "Firstname LASTNAME"
author_title <- "Ph.D."
author_email <- "Firstname@LASTNAME.com"

appVersion <- "1.2.0"

options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")

library(shiny)
library(shinydashboard)

library(parallel)
library(ggplot2)
library(grid)
library(scales)
library(broom)
library(readxl)
library(lme4)
library(tidyr)
library(viridis)

dir.create("www/CT/", showWarnings = FALSE)
dir.create("www/EXCEL/", showWarnings = FALSE)

duplicated2 <- function(x) {
  if (sum(dup <- duplicated(x)) == 0) {
    return(dup)
  }
  if (class(x) %in% c("data.frame", "matrix")) {
    return(duplicated(rbind(x[dup, ], x))[-seq_len(sum(dup))])
  } else {
    return(duplicated(c(x[dup], x))[-seq_len(sum(dup))])
  }
}

colSe <- function(x, na.rm = TRUE) {
  if (na.rm) {
    n <- colSums(!is.na(x))
  } else {
    n <- nrow(x)
  }
  colVar <- colMeans(x * x, na.rm = na.rm) - (colMeans(x, na.rm = na.rm))^2
  return(sqrt(colVar / n))
}
se <- function(x, na.rm = TRUE) {
  return(sqrt((mean(x^2, na.rm = na.rm) - mean(x, na.rm = na.rm)^2) / sum(!is.na(x))))
}

colSd <- function(x, na.rm = TRUE) {
  if (na.rm) {
    n <- colSums(!is.na(x))
  } else {
    n <- nrow(x)
  }
  colVar <- colMeans(x * x, na.rm = na.rm) - (colMeans(x, na.rm = na.rm))^2
  return(sqrt(colVar * n / (n - 1)))
}

signstars <- function(ivalue) {
  if (is.na(ivalue)) {
    return("NA")
  } else {
    return(c("ns", "ns", "*", "**", "***")[which.max(which(ivalue < c(1, 0.1, 0.05, 0.01, 0.001)))])
  }
}


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = HTML(paste0("EndoC &beta; 2 (v", appVersion, ")")),
    dropdownMenuOutput("messageMenu")
  ),
  dashboardSidebar(
    sidebarUserPanel(
      name = a(
        tags$i(style = "color:#777777", icon("envelope", lib = "glyphicon")),
        author_name,
        href = paste0("mailto:", author_email)
      ),
      subtitle = tags$span(style = "color:#777777", paste0("(", author_title, ")"))
    ),
    hr(),
    HTML(
      '<div style="text-align:center">
        <a id="downloadData" class="btn btn-default shiny-download-link", style="color:#ffffff" href="" target="_blank">
          <i class="fa fa-download" style="text-align:center;color:#ffffff"></i>
          Download Data
        </a>
      </div>'
    ),
    hr(),
    sidebarMenu(
      menuItem(
        text = "Pictures Settings",
        tabName = "PicturesSettings",
        icon = tags$i(style = "color:#777777", icon("picture", lib = "glyphicon"))
      ),
      hr(),
      menuItem(
        text = "Import Excel Files",
        tabName = "importexcelfiles",
        icon = tags$i(style = "color:#777777", icon("import", lib = "glyphicon")),
        selected = TRUE
      ),
      hr(),
      HTML(
        '<li class="false">
          <a aria-expanded="true" data-toggle="tab" data-value="RRthreshold">
            <i style="color:#777777"><i class="glyphicon glyphicon-cog"></i></i>
            <span>Technical Error Threshold</span>
          </a>
        </li>'
      ),
      sliderInput(
        inputId = "RRthreshold",
        label = NULL,
        min = 0,
        max = 1,
        value = 0.20,
        step = 0.05
      ),
      # hr(),
      # HTML(
      # '<li class="false">
      # <a aria-expanded="true" data-toggle="tab" data-value="IQRthreshold">
      # <i style="color:#777777"><i class="glyphicon glyphicon-cog"></i></i>
      # <span>IQR Multiplicator Threshold</span>
      # </a>
      # </li>'
      # ),
      # sliderInput(
      # inputId = "IQRthreshold",
      # label = NULL,
      # min = 1,
      # max = 3,
      # value = 2,
      # step = 0.05
      # ),
      hr(),
      menuItem(
        text = "Quality Control",
        menuSubItem(
          text = "Summary",
          tabName = "QCsummary"
        ),
        menuSubItem(
          text = "Technical Error (OD)",
          tabName = "ODDiffSummary"
        ),
        menuSubItem(
          text = "Technical Error Correction",
          tabName = "CorrectionSummary"
        ),
        icon = tags$i(style = "color:#777777", icon("search", lib = "glyphicon"))
      ),
      menuItem(
        text = "Analysis",
        tabName = "NTPanalysis",
        icon = tags$i(style = "color:#777777", icon("tasks", lib = "glyphicon"))
      ),
      hr()
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/theme.css")
    ),
    tabItems(
      tabItem(
        tabName = "PicturesSettings",
        fluidRow(
          box(
            HTML(
              '<div id="plotunits" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
                <label class="control-label" for="plotunits">Unit:</label>
                <div class="shiny-options-group">
                  <label class="radio-inline">
                    <input type="radio" name="plotunits" value="in" checked="checked"/>
                    <span>inch</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="plotunits" value="cm"/>
                    <span>centimetre</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="plotunits" value="mm"/>
                    <span>millimetre</span>
                  </label>
                </div>
              </div>
              <div class="form-group shiny-input-container">
                <label for="fontsize">Font size:</label>
                <input id="fontsize" type="number" class="form-control" value="16" min="1" max="100"/>
              </div>
              <div class="form-group shiny-input-container">
                <label for="plotwidth">Width:</label>
                <input id="plotwidth" type="number" class="form-control" value="7.5" min="1" max="100"/>
              </div>
              <div class="form-group shiny-input-container">
                <label for="plotheight">Height:</label>
                <input id="plotheight" type="number" class="form-control" value="6" min="1" max="100"/>
              </div>
              <div id="plotformat" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
                <label class="control-label" for="plotformat">Format:</label>
                <div class="shiny-options-group">
                  <label class="radio-inline">
                    <input type="radio" name="plotformat" value="jpg" checked="checked"/>
                    <span>jpg</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="plotformat" value="svg"/>
                    <span>svg</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="plotformat" value="tiff"/>
                    <span>tiff</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="plotformat" value="eps"/>
                    <span>eps</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="plotformat" value="png"/>
                    <span>png</span>
                  </label>
                </div>
              </div>
              <div id="plotdpi" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
                <label class="control-label" for="plotdpi">Resolution:</label>
                <div class="shiny-options-group">
                  <label class="radio-inline">
                    <input type="radio" name="plotdpi" value="150"/>
                    <span>150 dpi</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="plotdpi" value="300" checked="checked"/>
                    <span>300 dpi</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="plotdpi" value="600"/>
                    <span>600 dpi</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="plotdpi" value="1200"/>
                    <span>1200 dpi</span>
                  </label>
                </div>
              </div>
              <div id="plotcolour" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
                <label class="control-label" for="plotcolour">Colour:</label>
                <div class="shiny-options-group">
                  <label class="radio-inline">
                    <input type="radio" name="plotcolour" value="Colour"/>
                    <span>Colour</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="plotcolour" value="Gray" checked="checked"/>
                    <span>Gray</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="plotcolour" value="Custom"/>
                    <span>Custom</span>
                  </label>
              
                </div>
              </div>
               <div id="reportstars" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
                <label class="control-label" for="reportstars">Significativity Stars:</label>
                <div class="shiny-options-group">
                  <label class="radio-inline">
                    <input type="radio" name="reportstars" value="TRUE" checked="checked"/>
                    <span>Yes</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="reportstars" value="FALSE"/>
                    <span>No</span>
                  </label>
                </div>
              </div>'
            ),
            width = 6,
            collapsible = FALSE,
            title = "Pictures Settings",
            solidHeader = TRUE,
            status = "info",
            height = 600
          ),
          htmlOutput("coloursUi")
        )
      ),
      tabItem(
        tabName = "importexcelfiles",
        fluidRow(
          valueBoxOutput("filesimportedBox"),
          valueBoxOutput("progressBox"),
          valueBoxOutput("clearBox")
        ),
        fluidRow(
          box(
            fileInput(inputId = "fileid", label = NULL, accept = ".xlsx", multiple = TRUE),
            width = 6,
            collapsible = FALSE,
            title = "File Input",
            solidHeader = TRUE,
            status = "info",
            height = 125
          ),
          box(
            column(
              9,
              htmlOutput("filelist")
            ),
            column(
              3,
              tags$div(
                style = "text-align:center margin-top:",
                actionButton(
                  inputId = "delete",
                  label = "Delete File",
                  width = "100%",
                  icon = icon("trash", lib = "glyphicon")
                )
              )
            ),
            width = 6,
            collapsible = FALSE,
            title = "File Management",
            solidHeader = TRUE,
            status = "danger",
            height = 125
          )
        ),
        fluidRow(
          box(
            dataTableOutput("filesimportedSummary"),
            width = 12,
            collapsible = FALSE,
            title = "Files Already Imported",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      ),
      tabItem(
        tabName = "ODDiffSummary",
        fluidRow(
          box(
            HTML(
              '<div style="text-align:center">
                <a id="QC_plot4download" class="btn btn-default shiny-download-link " href="" target="_blank">
                  <i class="fa fa-download" style="text-align:center"></i>
                  Download
                </a>
              </div>'
            ),
            plotOutput("QC_plot4"),
            width = 6,
            collapsible = TRUE,
            title = "Relative Error Distribution for Blank Measures",
            solidHeader = TRUE,
            status = "primary"
          ),
          box(
            HTML(
              '<div style="text-align:center">
                <a id="QC_plot5download" class="btn btn-default shiny-download-link " href="" target="_blank">
                  <i class="fa fa-download" style="text-align:center"></i>
                  Download
                </a>
              </div>'
            ),
            plotOutput("QC_plot5"),
            width = 6,
            collapsible = TRUE,
            title = "Relative Error Distribution for Experiments measures",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      ),
      tabItem(
        tabName = "CorrectionSummary",
        fluidRow(
          box(
            HTML(
              '<div style="text-align:center">
                <a id="QC_plot1download" class="btn btn-default shiny-download-link " href="" target="_blank">
                  <i class="fa fa-download" style="text-align:center"></i>
                  Download
                </a>
              </div>'
            ),
            plotOutput("QC_plot1"),
            width = 6,
            collapsible = TRUE,
            title = "Raw Optical Density Relative Error for Insulin Concentration",
            solidHeader = TRUE,
            status = "primary"
          ),
          box(
            HTML(
              '<div style="text-align:center">
                <a id="QC_plot2download" class="btn btn-default shiny-download-link " href="" target="_blank">
                  <i class="fa fa-download" style="text-align:center"></i>
                  Download
                </a>
              </div>'
            ),
            plotOutput("QC_plot2"),
            width = 6,
            collapsible = TRUE,
            title = "Corrected Optical Density Relative Error for Insulin Concentration",
            solidHeader = TRUE,
            status = "primary"
          )
        ),
        fluidRow(
          box(
            HTML(
              '<div style="text-align:center">
                <a id="QC_plot3download" class="btn btn-default shiny-download-link " href="" target="_blank">
                  <i class="fa fa-download" style="text-align:center"></i>
                  Download
                </a>
              </div>'
            ),
            plotOutput("QC_plot3"),
            width = 12,
            collapsible = TRUE,
            title = "Linear Regression Estimations for Optical Density and Insulin concentration correspondence",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      ),
      tabItem(
        tabName = "QCsummary",
        fluidRow(
          box(
            dataTableOutput("QCsummarytable"),
            width = 12,
            collapsible = FALSE,
            title = "Quality Control Summary Information for Suspected NTP Experiment Failure",
            solidHeader = TRUE,
            status = "primary"
          )
        )
      ),
      tabItem(
        tabName = "NTPanalysis",
        fluidRow(
          box(
            checkboxGroupInput(
              inputId = "QCfilter",
              label = "Quality Filter:",
              choices = list("Clear!" = "Clear!", "Warning!" = "Warning!", "Legend" = "Legend"),
              selected = c("Clear!", "Warning!", "Legend"),
              inline = TRUE
            ),
            htmlOutput("CheckBoxGroupUi"),
            radioButtons(
              inputId = "FoldChangeUiActivate",
              label = "Fold Change Threshold (siNTP):",
              choices = list("Yes" = TRUE, "No" = FALSE),
              selected = FALSE,
              inline = TRUE
            ),
            htmlOutput("FoldChangeUi"),
            width = 4,
            height = 300,
            collapsible = FALSE,
            title = "Grouping Experiments Settings",
            solidHeader = TRUE,
            status = "info"
          ),
          box(
            htmlOutput("SelectGroupUi"),
            width = 4,
            height = 300,
            collapsible = FALSE,
            title = "Available Data",
            solidHeader = TRUE,
            status = "info"
          ),
          box(
            htmlOutput("NTPselectExperimentUi"),
            width = 4,
            height = 300,
            collapsible = FALSE,
            title = "Available Experiments",
            solidHeader = TRUE,
            status = "info"
          )
        ),
        fluidRow(
          box(
            HTML(
              '<div style="text-align:center">
                <a id="NTPAnalysis_plot1download" class="btn btn-default shiny-download-link " href="" target="_blank">
                  <i class="fa fa-download" style="text-align:center"></i>
                  Download
                </a>
              </div>'
            ),
            plotOutput("NTPAnalysis_plot1"),
            width = 6,
            collapsible = TRUE,
            title = "Blank Curve",
            solidHeader = TRUE,
            status = "success",
            collapsed = TRUE
          ),
          box(
            HTML(
              '<div style="text-align:center">
                <a id="FoldChangeNTP_plotdownload" class="btn btn-default shiny-download-link " href="" target="_blank">
                  <i class="fa fa-download" style="text-align:center"></i>
                  Download
                </a>
              </div>'
            ),
            plotOutput("FoldChangeNTP_plot"),
            width = 6,
            collapsible = TRUE,
            title = "Fold Change siNTP",
            solidHeader = TRUE,
            status = "success",
            collapsed = TRUE
          )
        ),
        fluidRow(
          box(
            HTML(
              '<div style="text-align:center">
                <a id="NTPAnalysis_plot2download" class="btn btn-default shiny-download-link " href="" target="_blank">
                  <i class="fa fa-download" style="text-align:center"></i>
                  Download
                </a>
              </div>'
            ),
            plotOutput("NTPAnalysis_plot2"),
            width = 12,
            collapsible = TRUE,
            title = "Insulin Secretion (OD)",
            solidHeader = TRUE,
            status = "primary",
            collapsed = TRUE
          )
        ),
        fluidRow(
          box(
            HTML(
              '<div style="text-align:center">
                <a id="NTPAnalysis_plot2bisdownload" class="btn btn-default shiny-download-link " href="" target="_blank">
                  <i class="fa fa-download" style="text-align:center"></i>
                  Download
                </a>
              </div>'
            ),
            HTML(
              '<div style="text-align:center">
                <div class="form-group shiny-input-container">
                  <div class="radio-inline">
                    <label>
                      <input id="removelowfC" type="checkbox" checked="checked"/>
                      <span>Red Points</span>
                    </label>
                  </div>
                  <div class="radio-inline">
                    <label>
                      <input id="removeallpoints" type="checkbox" checked="checked"/>
                      <span>Points</span>
                    </label>
                  </div>
                  <div class="radio-inline">
                    <label>
                      <input id="removeoutlier" type="checkbox" checked="checked"/>
                      <span>Outliers</span>
                    </label>
                  </div>
                </div>
              </div>'
            ),
            plotOutput("NTPAnalysis_plot2bis"),
            width = 6,
            collapsible = TRUE,
            title = "Insulin Secretion (% of content)",
            solidHeader = TRUE,
            status = "primary",
            collapsed = FALSE,
            height = 555
          ),
          box(
            HTML(
              '<div style="text-align:center">
                <a id="NTPAnalysis_plot3download" class="btn btn-default shiny-download-link " href="" target="_blank">
                  <i class="fa fa-download" style="text-align:center"></i>
                  Download
                </a>
              </div>'
            ),
            HTML(
              '<div style="text-align:center">
                <div id="seorsd" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
                  <label class="radio-inline">
                    <input type="radio" name="seorsd" value="SEM" checked="checked"/>
                    <span>SEM</span>
                  </label>
                  <label class="radio-inline">
                    <input type="radio" name="seorsd" value="SD"/>
                    <span>SD</span>
                  </label>
                  <label class="radio-inline">
                    <input id="drawn" type="checkbox" checked="checked"/>
                    <span>Draw N</span>
                  </label>
                </div>
              </div>'
            ),
            plotOutput("NTPAnalysis_plot3"),
            width = 6,
            collapsible = TRUE,
            title = "Fold Change Insulin Secretion",
            solidHeader = TRUE,
            status = "primary",
            collapsed = FALSE,
            height = 555
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  customcolourstable <- reactive({
    color.rgb <- t(col2rgb(colors()))
    color.hex <- rgb(color.rgb[, 1], color.rgb[, 2], color.rgb[, 3], maxColorValue = 255)
    color.df <- color.hex
    names(color.df) <- colors()
    color.df <- as.list(color.df)
    return(color.df)
  })
  output$coloursUi <- renderUI({
    color.df <- customcolourstable()
    return(box(
      selectInput(
        inputId = "customcoloursNTP",
        label = "Available Colours for NTP:",
        choices = color.df,
        selected = c("#1E90FF"),
        width = "100%",
        selectize = FALSE,
        multiple = FALSE,
        size = 12
      ),
      selectInput(
        inputId = "customcolours",
        label = "Available Colours for Targets:",
        choices = color.df,
        selected = c("#EE2C2C", "#008B45", "#EE30A7", "#EEB422", "#00BFFF", "#AB82FF"),
        width = "100%",
        selectize = FALSE,
        multiple = TRUE,
        size = 12
      ),
      width = 6,
      collapsible = FALSE,
      title = "Colours Settings",
      solidHeader = TRUE,
      status = "info",
      height = 600
    ))
  })

  myPalette <- reactive({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
    validate(need(length(input$plotcolour) != 0, "No data available!"))
    if ("Custom" %in% input$plotcolour) {
      myPalette <- c(input$customcoloursNTP, input$customcolours)
    } else {
      colorOn <- ifelse(is.null(input$plotcolour), TRUE, "Colour" %in% input$plotcolour)
      if (colorOn) {
        myPalettes <- rbind(
          c(Name = "dodgerblue", Hex = "#1E90FF", RGB = "rgb(30/255, 144/255, 255/255)"),
          c(Name = "firebrick2", Hex = "#EE2C2C", RGB = "rgb(238/255, 44/255, 44/255)"),
          c(Name = "springgreen3", Hex = "#008B45", RGB = "rgb(0/255, 139/255, 69/255)"),
          c(Name = "maroon2", Hex = "#EE30A7", RGB = "rgb(238/255, 48/255, 167/255)"),
          c(Name = "goldenrod2", Hex = "#EEB422", RGB = "rgb(238/255, 180/255, 34/255)"),
          c(Name = "deepskyblue", Hex = "#00BFFF", RGB = "rgb(0/255, 191/255, 255/255)"),
          c(Name = "mediumpurple1", Hex = "#AB82FF", RGB = "rgb(171/255, 130/255, 255/255)") # ,
          # c(Name = "tan1",  Hex = "#FFA54F", RGB = "rgb(255/255, 165/255, 79/255)")
        )
        myPalette <- myPalettes[, "Name"]
      } else {
        myPalette <- gray.colors(n = 7, start = 0.3, end = 0.75)
      }
    }
    return(myPalette)
  })

  formatdata <- reactive({
    delete <- input$delete
    inFiles <- input$fileid
    withProgress(message = "Loading...", value = NULL, {
      qcthresh <- input$RRthreshold # input$IQRthreshold
      if (!is.null(inFiles)) {
        for (iFile in seq_len(nrow(inFiles))) {
          file.copy(from = inFiles[iFile, "datapath"], to = paste0("www/EXCEL/", inFiles[iFile, "name"]))
        }
      }
      if (file.exists("www/importedfilesinfo.Rdata") & is.null(inFiles)) {
        load(file = "www/importedfilesinfo.Rdata")
        return(importedfilesinfo)
      } else {
        if (length(list.files("www/EXCEL")) != 0) {
          inFiles <- data.frame(
            datapath = list.files("www/EXCEL", full.names = TRUE),
            Name = list.files("www/EXCEL", full.names = FALSE)
          )

          for (iFile in seq_len(nrow(inFiles))) {
            inFile <- inFiles[iFile, , drop = FALSE]
            argfile <- inFile$datapath
            # listsheets <- getSheets(loadWorkbook(argfile))
            listsheets <- excel_sheets(argfile)
            for (isheetname in listsheets) {
              rawfile <- as.data.frame(read_xlsx(
                path = argfile,
                sheet = isheetname,
                col_names = FALSE
              ))
              postablename <- unlist(lapply(c("LYSAT", "SN1", "SN2"), grep, rawfile[, 1]))
              pos <- unlist(
                x = apply(cbind(postablename + 1, c(postablename[-1] - 1, max(postablename) + 7)), 1, list),
                recursive = FALSE
              )

              operator <- rawfile[grep("operateur", rawfile[, 1]), 2]

              info <- unlist(lapply(sapply(pos, "[", 1) - 1, function(i) {
                rawfile[i, 1]
              }))
              info <- gsub(".*\\((.*)\\).*", "\\1", info)
              info[duplicated2(info)] <- paste0(info[duplicated2(info)], seq_along(info[duplicated2(info)]))
              for (ipos in seq_along(pos)) {
                out <- rawfile[(pos[[ipos]][1] + 1):pos[[ipos]][2], ]
                colnames(out) <- gsub(" ", "_", rawfile[(pos[[ipos]][1]), ])
                colnames(out) <- gsub("^_*", "", colnames(out))
                whichCol <- unique(c(grep("NA", colnames(out)), which(is.na(colnames(out)))))
                if (length(whichCol) != 0) {
                  out <- out[which(rowSums(is.na(out)) != ncol(out)), -whichCol]
                } else {
                  out <- out[which(rowSums(is.na(out)) != ncol(out)), ]
                }
                out <- out[seq_len(6), seq_len(3)]
                out[, 2] <- as.numeric(out[, 2])
                out[, 3] <- as.numeric(out[, 3])
                assign(x = info[ipos], value = out)
              }
              out <- rawfile[8:13, ]
              colnames(out) <- rawfile[7, ]
              colnames(out)[grep("Dulicate O.D", colnames(out)) + 1] <- "Dulicate O.D"
              colnames(out) <- gsub(" ", "_", colnames(out))
              colnames(out) <- gsub("^_*", "", colnames(out))
              whichCol <- unique(c(grep("NA", colnames(out)), which(is.na(colnames(out)))))
              if (length(whichCol) != 0) {
                out <- out[which(rowSums(is.na(out)) != ncol(out)), -whichCol]
              } else {
                out <- out[which(rowSums(is.na(out)) != ncol(out)), ]
              }

              out <- out[, seq_len(4)]
              assign(x = "Blank", value = out)
              save(
                list = c("operator", info, "Blank"),
                file = paste0("www/CT/", gsub(".xlsx", "", inFile$Name), "_Sheet_", isheetname, ".Rdata")
              )
            }

            checkrdata <- any(!apply(sapply(
              X = list.files(path = "www/CT", pattern = ".*.Rdata", full.names = TRUE),
              FUN = function(xfile) {
                tmp <- new.env()
                load(xfile, envir = tmp)
                sapply(setdiff(ls(tmp), "operator"), function(xobject) {
                  dim(get(xobject, envir = tmp))
                })
              }
            ), 1, all))

            files <- list.files(path = "www/CT", pattern = ".*.Rdata", full.names = TRUE)
            dataset <- do.call("rbind", lapply(seq_along(files), function(ifile) {
              load(files[ifile])
              colnames(LYSAT1) <- c("Samples", "OD1", "OD2")
              colnames(SN1) <- c("Samples", "OD1", "OD2")
              colnames(SN2) <- c("Samples", "OD1", "OD2")

              LYSAT1[, "Samples"] <- rep(LYSAT1[c(1, 4), "Samples"], each = 3)
              SN1[, "Samples"] <- rep(SN1[c(1, 4), "Samples"], each = 3)
              SN2[, "Samples"] <- rep(SN2[c(1, 4), "Samples"], each = 3)

              LYSAT1[, "Replicates"] <- rep(seq_len(3), 2)
              SN1[, "Replicates"] <- rep(seq_len(3), 2)
              SN2[, "Replicates"] <- rep(seq_len(3), 2)

              LYSAT1 <- cbind.data.frame(LYSAT1, What = "LYSAT")
              SN1 <- cbind.data.frame(SN1, What = "SN1")
              SN2 <- cbind.data.frame(SN2, What = "SN2")

              dta <- cbind.data.frame(
                rbind(LYSAT1, SN1, SN2),
                Exp = gsub(".*/(.*)_Sheet_(.*).Rdata", "\\1", files[ifile]),
                ExpType = gsub(".*/(.*)_Sheet_(.*).Rdata", "\\2", files[ifile]),
                Op = operator
              )
              dta[, c("OD1", "OD2")] <- lapply(dta[, c("OD1", "OD2")], as.numeric)
              return(dta)
            }))
            rownames(dataset) <- NULL

            blankdta <- do.call("rbind", lapply(seq_along(files), function(ifile) {
              load(files[ifile])
              Blank <- Blank[, c(1, 3, 4)]
              colnames(Blank) <- c("mUL", "OD1", "OD2")
              Blank[] <- lapply((Blank), as.numeric)
              Blank[, "OD"] <- rowMeans(Blank[, c("OD1", "OD2")])
              Blank[, "ODdiff"] <- apply(Blank[, c("OD2", "OD1")], 1, diff) / Blank[, "OD1"]
              dta <- cbind.data.frame(
                ID = seq_len(6),
                do.call("rbind", (apply(Blank[, c("mUL", "OD")], 1, `-`, Blank[1, c("mUL", "OD")]))),
                ODraw = Blank[, "OD"],
                ODdiff = Blank[, "ODdiff"],
                Exp = gsub(".*/(.*)_Sheet_(.*).Rdata", "\\1", files[ifile]),
                ExpType = gsub(".*/(.*)_Sheet_(.*).Rdata", "\\2", files[ifile]),
                Op = operator
              )
              return(dta)
            }))
            rownames(blankdta) <- NULL

            save(list = c("dataset", "blankdta"), file = "www/EndoC_Beta.Rdata")

            if (file.exists("www/importedfilesinfo.Rdata")) {
              load(file = "www/importedfilesinfo.Rdata")
              if (inFile[, "Name"] %in% importedfilesinfo[, "Name"]) {
                newFile <- cbind.data.frame(
                  Name = inFile[, "Name"],
                  Sheets = length(listsheets),
                  Operator = operator,
                  Check = as.integer(0)
                )
                importedfilesinfo[importedfilesinfo[, "Name"] %in% inFile[, "Name"], ] <- newFile
              } else {
                importedfilesinfo <- unique(rbind(
                  importedfilesinfo,
                  cbind.data.frame(
                    Name = inFile[, "Name"],
                    Sheets = length(listsheets),
                    Operator = operator,
                    Check = as.integer(0)
                  )
                ))
              }
              save(importedfilesinfo, file = "www/importedfilesinfo.Rdata")
            } else {
              importedfilesinfo <- cbind.data.frame(
                Name = inFile[, "Name"],
                Sheets = length(listsheets),
                Operator = operator,
                Check = as.integer(0)
              )
              save(importedfilesinfo, file = "www/importedfilesinfo.Rdata")
            }
          }
          return(importedfilesinfo)
        }
      }
    })
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      "EndoC_Beta.tar.gz"
    },
    content = function(file) {
      system(paste("tar czvf", file, "www/EXCEL"))
    }
  )

  output$filesimportedSummary <- renderDataTable({
    delete <- input$delete
    inFile <- input$fileid
    out <- formatdata()
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
    out <- formatdata()
    return(out)
  }, options = list(searching = FALSE))

  output$messageMenu <- renderMenu({
    delete <- input$delete
    trash <- formatdata()
    if (file.exists("www/importedfilesinfo.Rdata")) {
      load(file = "www/importedfilesinfo.Rdata")
      dropdownMenu(
        type = "notifications",
        notificationItem(
          text = ifelse(
            file.exists("www/EndoC_Beta.Rdata"),
            paste("Data computed on", format(file.info("www/EndoC_Beta.Rdata")[, "mtime"], "%Y-%d-%m"), "!"),
            "No data available!"
          ),
          icon = icon("exclamation-sign", lib = "glyphicon"),
          status = "info"
        ),
        notificationItem(
          text = paste0("Last file uploaded \"", importedfilesinfo[nrow(importedfilesinfo), "Name"], "\" !"),
          icon = icon("exclamation-sign", lib = "glyphicon"),
          status = "info"
        )
      )
    } else {
      dropdownMenu(
        type = "notifications",
        notificationItem(
          text = ifelse(
            file.exists("www/EndoC_Beta.Rdata"),
            paste("Data computed on", format(file.info("www/EndoC_Beta.Rdata")[, "mtime"], "%Y-%d-%m!")),
            "No data available!"
          ),
          icon = icon("exclamation-sign", lib = "glyphicon"),
          status = "info"
        )
      )
    }
  })

  output$filesimportedBox <- renderValueBox({
    delete <- input$delete
    trash <- formatdata()
    if (file.exists("www/importedfilesinfo.Rdata")) {
      load(file = "www/importedfilesinfo.Rdata")
      nfiles <- nrow(importedfilesinfo)
    } else {
      nfiles <- 0
    }
    valueBox(
      value = nfiles,
      subtitle = "File(s)",
      icon = icon("edit", lib = "glyphicon"),
      color = "blue"
    )
  })

  output$progressBox <- renderValueBox({
    delete <- input$delete
    trash <- formatdata()
    if (file.exists("www/importedfilesinfo.Rdata")) {
      load(file = "www/importedfilesinfo.Rdata")
      nexperiments <- sum(importedfilesinfo[, "Sheets"])
    } else {
      nexperiments <- 0
    }
    valueBox(
      value = nexperiments,
      subtitle = "Experiment(s)",
      icon = icon("share", lib = "glyphicon"),
      color = "maroon"
    )
  })

  output$clearBox <- renderValueBox({
    delete <- input$delete
    trash <- formatdata()
    if (file.exists("www/importedfilesinfo.Rdata")) {
      load(file = "www/importedfilesinfo.Rdata")
      trash <- QCsummarytableReac()
      if ("Check" %in% colnames(importedfilesinfo)) {
        ncheck <- sum(importedfilesinfo[, "Check"])
        ncheck <- paste(ncheck, sum(importedfilesinfo[, "Sheets"]) - ncheck, sep = " / ")
      } else {
        ncheck <- paste(0, sum(importedfilesinfo[, "Sheets"]) - 0, sep = " / ")
      }
    } else {
      ncheck <- "0 / 0"
    }
    valueBox(
      value = ncheck,
      subtitle = "Clear / Warning(s)",
      icon = icon("check", lib = "glyphicon"),
      color = "green"
    )
  })

  computeQC <- reactive({
    delete <- input$delete
    trash <- formatdata()
    withProgress(message = "Loading...", value = NULL, {
      validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
      validate(need(file.exists("www/EndoC_Beta.Rdata"), "No data available!"))

      load(file = "www/EndoC_Beta.Rdata")
      # thresholdBlank <- input$IQRthreshold*IQR(blankdta[, "ODdiff"], na.rm = TRUE)
      thresholdBlank <- input$RRthreshold
      blankdta[, "ODdiffCor"] <- ifelse(abs(blankdta[, "ODdiff"]) > thresholdBlank, NA, blankdta[, "ODdiff"])

      ## Compute slope and intercept
      out <- do.call("rbind", by(blankdta, paste(blankdta[, "Exp"], blankdta[, "ExpType"]), function(dta) {
        lm0 <- tidy(lm(log10(OD) ~ log10(mUL / 23), data = subset(dta, OD != 0)))[, 2]
        lm1 <- tidy(lm(log10(OD) ~ log10(mUL / 23), data = subset(dta, !is.na(ODdiffCor) & OD != 0)))[, 2]

        return(cbind.data.frame(
          Exp = dta[seq_len(2), "Exp"],
          ExpType = dta[seq_len(2), "ExpType"],
          Type = c("RAW", "COR"),
          rbind(lm0, lm1)
        ))
      }))
      colnames(out) <- c("Exp", "ExpType", "Type", "intercept", "slope")
      out[, "Warning"] <- as.character(out[, "ExpType"]) %in%
        names(which(by(abs(out[, "intercept"]) > abs(mean(out[, "intercept"])) + IQR(out[, "intercept"]), out[, "ExpType"], all))) |
        as.character(out[, "ExpType"]) %in%
          names(which(by(abs(out[, "slope"]) > abs(mean(out[, "slope"])) + 3 * IQR(out[, "slope"]), out[, "ExpType"], all)))
      out[, "TypeWarning"] <- factor(
        x = paste(out[, "Type"], out[, "Warning"], sep = ":"),
        levels = c("COR:FALSE", "COR:TRUE", "RAW:FALSE", "RAW:TRUE")
      )
      out[, "Type"] <- as.factor(out[, "Type"])
      wrongBlank <- unique(subset(out, Warning, c("Exp", "ExpType")))


      dataset[, "Samples"] <- gsub(" ", "_", dataset[, "Samples"])
      dataset[, "ODdiff"] <- apply(dataset[, c("OD2", "OD1")], 1, diff) / dataset[, "OD1"]
      # thresholdCell <- input$IQRthreshold*IQR(dataset[, "ODdiff"], na.rm = TRUE)
      thresholdCell <- input$RRthreshold
      dataset[, "ODdiffCor"] <- ifelse(abs(dataset[, "ODdiff"]) > thresholdCell, NA, dataset[, "ODdiff"])
      dataset[, "TechnicalQC"] <- !(abs(dataset[, "ODdiff"]) > thresholdCell)
      dataset[is.na(dataset[, "TechnicalQC"]), "TechnicalQC"] <- FALSE

      checkNTP <- table(
        apply(dataset[, c("Samples", "Exp", "ExpType", "What")], 1, paste, collapse = "-"),
        factor(dataset[, "TechnicalQC"], levels = c(FALSE, TRUE))
      )
      technicalNTP <- unique(do.call(
        "rbind",
        strsplit(gsub(".*-(.*)-(.*)-.*", "\\1-\\2", rownames(checkNTP[checkNTP[, "FALSE"] > 1, ])), "-")
      ))
      if (is.matrix(technicalNTP) | is.data.frame(technicalNTP)) {
        colnames(technicalNTP) <- c("Exp", "ExpType")
      }

      wrongNTP <- rbind(technicalNTP, wrongBlank)
      colnames(wrongNTP) <- c("Exp", "ExpType")

      blankdta[, "Group"] <- paste(blankdta[, "Exp"], blankdta[, "ExpType"], sep = "-")
      dataset[, "Group"] <- paste(dataset[, "Exp"], dataset[, "ExpType"], sep = "-")
      out[, "Group"] <- paste(out[, "Exp"], out[, "ExpType"], sep = "-")

      return(list(
        out = out,
        wrongBlank = wrongBlank,
        blankdta = blankdta,
        thresholdBlank = thresholdBlank,
        thresholdCell = thresholdCell,
        wrongNTP = wrongNTP,
        dataset = dataset
      ))
    })
  })
  QC_plot1 <- reactive({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))

    blankdta <- computeQC()[["blankdta"]]
    thresholdBlank <- computeQC()[["thresholdBlank"]]
    return(
      ggplot(data = blankdta, mapping = aes(x = ID, y = ODdiff, colour = factor(Group))) +
        theme_minimal(base_size = input$fontsize) +
        geom_smooth(method = "lm", se = FALSE) +
        geom_point(shape = 4) +
        geom_hline(yintercept = thresholdBlank, linetype = 2, colour = myPalette()[3]) +
        geom_hline(yintercept = -thresholdBlank, linetype = 2, colour = myPalette()[3]) +
        scale_y_continuous(
          labels = percent,
          limits = range(range(blankdta[, "ODdiff"], na.rm = TRUE), c(thresholdBlank, -thresholdBlank), na.rm = TRUE)
        ) +
        scale_x_continuous(breaks = seq_len(6)) +
        scale_colour_manual(values = rep(myPalette(), ceiling(length(unique(blankdta[, "Group"])) / length(myPalette())))) +
        labs(y = bquote("Relative error" ~ (OD[2] - OD[1]) / OD[1]), x = "Blank (Insulin concentrations)") +
        theme(legend.position = "none") +
        theme(
          axis.text = element_text(colour = ifelse(myPalette()[1] == "dodgerblue", "dodgerblue", "black"), face = "bold")
        )
    )
  })
  output$QC_plot1 <- renderPlot({
    QC_plot1()
  })
  output$QC_plot1download <- downloadHandler(
    filename = function() {
      paste0("RawOD_RE_Insulin.", input$plotformat)
    },
    content = function(file) {
      ggsave(
        file = file,
        plot = QC_plot1() +
          theme(legend.title = element_blank()),
        width = input$plotwidth,
        height = input$plotheight,
        units = input$plotunits,
        dpi = as.numeric(input$plotdpi)
      )
    }
  )
  QC_plot2 <- reactive({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))

    blankdta <- computeQC()[["blankdta"]]
    thresholdBlank <- computeQC()[["thresholdBlank"]]
    return(
      ggplot(data = blankdta, mapping = aes(x = ID, y = ODdiffCor, colour = factor(Group))) +
        theme_minimal(base_size = input$fontsize) +
        geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
        geom_point(shape = 4) +
        geom_hline(yintercept = thresholdBlank, linetype = 2, colour = myPalette()[3]) +
        geom_hline(yintercept = -thresholdBlank, linetype = 2, colour = myPalette()[3]) +
        scale_y_continuous(
          labels = percent,
          limits = range(range(blankdta[, "ODdiff"], na.rm = TRUE), c(thresholdBlank, -thresholdBlank), na.rm = TRUE)
        ) +
        scale_x_continuous(breaks = seq_len(6)) +
        scale_colour_manual(values = rep(myPalette(), ceiling(length(unique(blankdta[, "Group"])) / length(myPalette())))) +
        labs(y = bquote("Relative error" ~ (OD[2] - OD[1]) / OD[1]), x = "Blank (Insulin concentrations)") +
        theme(legend.position = "none") +
        theme(
          axis.text = element_text(colour = ifelse(myPalette()[1] == "dodgerblue", "dodgerblue", "black"), face = "bold")
        )
    )
  })
  output$QC_plot2 <- renderPlot({
    QC_plot2()
  })
  output$QC_plot2download <- downloadHandler(
    filename = function() {
      paste0("CorOD_RE_Insulin.", input$plotformat)
    },
    content = function(file) {
      ggsave(
        file = file,
        plot = QC_plot2() +
          theme(legend.title = element_blank()),
        width = input$plotwidth,
        height = input$plotheight,
        units = input$plotunits,
        dpi = as.numeric(input$plotdpi)
      )
    }
  )
  QC_plot3 <- reactive({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))

    blankdta <- computeQC()[["blankdta"]]
    thresholdBlank <- computeQC()[["thresholdBlank"]]
    out <- computeQC()[["out"]]
    dtaplot <- merge(
      x = subset(blankdta, OD != 0),
      y = out[, c("Exp", "ExpType", "Warning", "Type", "intercept", "slope")],
      by = c("Exp", "ExpType")
    )
    dtaplot[, "Type"] <- factor(gsub("COR", "CORRECTED", dtaplot[, "Type"]), levels = c("RAW", "CORRECTED"))
    return(
      ggplot(
        data = subset(dtaplot, abs(ODdiff) <= thresholdBlank),
        mapping = aes(y = log10(OD), x = log10(mUL / 23), colour = Warning)
      ) +
        theme_minimal(base_size = input$fontsize) +
        geom_point(shape = 4) +
        geom_abline(mapping = aes(intercept = intercept, slope = slope, colour = Warning)) +
        scale_colour_manual(name = NULL, values = myPalette()) +
        labs(y = bquote(log[10](OD)), x = bquote("Insulin" ~ (log[10](mu * g / L)))) +
        theme(legend.position = "none") +
        facet_wrap(~Type) +
        theme(
          axis.text = element_text(colour = ifelse(myPalette()[1] == "dodgerblue", "dodgerblue", "black"), face = "bold")
        )
    )
  })
  output$QC_plot3 <- renderPlot({
    QC_plot3()
  })
  output$QC_plot3download <- downloadHandler(
    filename = function() {
      paste0("LM_OD_Insulin.", input$plotformat)
    },
    content = function(file) {
      ggsave(
        file = file,
        plot = QC_plot3() +
          theme(legend.title = element_blank()),
        width = input$plotwidth,
        height = input$plotheight,
        units = input$plotunits,
        dpi = as.numeric(input$plotdpi)
      )
    }
  )
  QC_plot4 <- reactive({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))

    blankdta <- computeQC()[["blankdta"]]
    dataset <- computeQC()[["dataset"]]
    thresholdBlank <- computeQC()[["thresholdBlank"]]
    scalex <- range(c(blankdta[, "ODdiff"], dataset[, "ODdiff"]), na.rm = TRUE)
    return(
      ggplot(data = blankdta) +
        theme_minimal(base_size = input$fontsize) +
        geom_density(mapping = aes(x = ODdiff, y = ..scaled..), colour = myPalette()[1], fill = myPalette()[1], alpha = 0.5) +
        geom_vline(xintercept = thresholdBlank, linetype = 2, colour = myPalette()[3]) +
        geom_vline(xintercept = -thresholdBlank, linetype = 2, colour = myPalette()[3]) +
        geom_text(
          label = percent(thresholdBlank),
          x = thresholdBlank,
          y = 1,
          colour = myPalette()[3],
          hjust = 1.05,
          vjust = 1,
          size = 6
        ) +
        geom_text(
          label = paste0("-", percent(thresholdBlank)),
          x = -thresholdBlank,
          y = 1,
          colour = myPalette()[3],
          hjust = -0.05,
          vjust = 1,
          size = 6
        ) +
        scale_x_continuous(labels = percent, limits = scalex) +
        labs(x = bquote("Relative error" ~ ((OD[2] - OD[1]) / OD[1])), y = "Density") +
        theme(
          axis.text = element_text(colour = myPalette()[1], hjust = 0.65, face = "bold")
        )
    )
  })
  output$QC_plot4 <- renderPlot({
    QC_plot4()
  })
  output$QC_plot4download <- downloadHandler(
    filename = function() {
      paste0("RE_DistBlank.", input$plotformat)
    },
    content = function(file) {
      ggsave(
        file = file,
        plot = QC_plot4() +
          theme(legend.title = element_blank()),
        width = input$plotwidth,
        height = input$plotheight,
        units = input$plotunits,
        dpi = as.numeric(input$plotdpi)
      )
    }
  )
  QC_plot5 <- reactive({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))

    blankdta <- computeQC()[["blankdta"]]
    dataset <- computeQC()[["dataset"]]
    thresholdCell <- computeQC()[["thresholdCell"]]
    scalex <- range(c(blankdta[, "ODdiff"], dataset[, "ODdiff"]), na.rm = TRUE)
    return(
      ggplot(data = dataset) +
        theme_minimal(base_size = input$fontsize) +
        geom_density(mapping = aes(x = ODdiff, y = ..scaled..), colour = myPalette()[1], fill = myPalette()[1], alpha = 0.5) +
        geom_vline(xintercept = thresholdCell, linetype = 2, colour = myPalette()[3]) +
        geom_vline(xintercept = -thresholdCell, linetype = 2, colour = myPalette()[3]) +
        geom_text(
          label = percent(thresholdCell),
          x = thresholdCell,
          y = 1,
          colour = myPalette()[3],
          hjust = 1.05,
          vjust = 1,
          size = 6
        ) +
        geom_text(
          label = paste0("-", percent(thresholdCell)),
          x = -thresholdCell,
          y = 1,
          colour = myPalette()[3],
          hjust = -0.05,
          vjust = 1,
          size = 6
        ) +
        scale_x_continuous(labels = percent, limits = scalex) +
        labs(x = bquote("Relative error" ~ ((OD[2] - OD[1]) / OD[1])), y = "Density") +
        theme(
          axis.text = element_text(colour = myPalette()[1], hjust = 0.65, face = "bold")
        )
    )
  })
  output$QC_plot5 <- renderPlot({
    QC_plot5()
  })
  output$QC_plot5download <- downloadHandler(
    filename = function() {
      paste0("RE_DistExp.", input$plotformat)
    },
    content = function(file) {
      ggsave(
        file = file,
        plot = QC_plot5() +
          theme(legend.title = element_blank()),
        width = input$plotwidth,
        height = input$plotheight,
        units = input$plotunits,
        dpi = as.numeric(input$plotdpi)
      )
    }
  )

  NTPdata <- reactive({
    withProgress(message = "Loading...", value = NULL, {
      validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))

      dataset <- computeQC()[["dataset"]]
      blankdta <- computeQC()[["blankdta"]]
      out <- computeQC()[["out"]]
      return(
        lapply(unique(dataset[, "Group"]), function(iNTP) {
          XX <- subset(dataset, Group == iNTP)
          YY <- subset(blankdta, Group == iNTP)
          ZZ <- subset(out, Group == iNTP & Type == "COR")

          XX[!XX[, "TechnicalQC"], c("OD1", "OD2")] <- NA

          XX[, "Mean"] <- rowMeans(XX[, c("OD1", "OD2")])
          XX[, "Mean-BK"] <- XX[, "Mean"] - YY[1, "ODraw"]
          XX[, "log(Abs)"] <- log10(XX[, "Mean-BK"])
          XX[, "log conc"] <- (XX[, "log(Abs)"] - ZZ[, "intercept"]) / ZZ[, "slope"]
          XX[, "ug/L"] <- 10^XX[, "log conc"]
          XX[, "dilutions to measure"] <- as.vector(mapply(rep, c(500, 16, 16), table(XX[, "What"])))
          XX[, "Final conc ug/L"] <- XX[, "ug/L"] * XX[, "dilutions to measure"]
          XX[, "Total ng (in 50 ul)"] <- ifelse(
            XX[, "What"] == "LYSAT",
            0.05 * XX[, "Final conc ug/L"],
            0.1 * XX[, "Final conc ug/L"]
          )
          XX[, "Total content (SN1)"] <- sapply(seq_len(nrow(XX) / length(unique(XX[, "What"]))) - 1, function(i) {
            index <- seq(1, nrow(XX), nrow(XX) / length(unique(XX[, "What"]))) + 1 * i
            sum(XX[index, "Total ng (in 50 ul)"])
          })
          XX[, "Total content (SN2)"] <- sapply(seq_len(nrow(XX) / length(unique(XX[, "What"]))) - 1, function(i) {
            index <- seq(1, nrow(XX), nrow(XX) / length(unique(XX[, "What"]))) + 1 * i
            sum(XX[index[-3], "Total ng (in 50 ul)"])
          })
          XX[, "ug insulin/million cells"] <- sapply(seq_len(nrow(XX)), function(i) {
            switch(XX[i, "What"],
              "LYSAT" = {
                XX[i, "Total content (SN1)"] / 50
              },
              "SN1" = {
                NA
              },
              "SN2" = {
                NA
              }
            )
          })
          XX[, "Insulin"] <- sapply(seq_len(nrow(XX)), function(i) {
            switch(XX[i, "What"],
              "LYSAT" = {
                NA
              },
              "SN1" = {
                (XX[i, "Total ng (in 50 ul)"] * 100) / XX[i, "Total content (SN1)"]
              },
              "SN2" = {
                (XX[i, "Total ng (in 50 ul)"] * 100) / XX[i, "Total content (SN2)"]
              }
            )
          })
          XX <- subset(
            x = XX,
            subset = What != "LYSAT",
            c("Samples", "OD1", "OD2", "What", "Exp", "ExpType", "Group", "Op", "ODdiffCor", "Insulin")
          )
          XX[, "Samples"] <- factor(
            x = XX[, "Samples"],
            levels = c("0,5_mM_Glc", "16,7_mM_Glc", "0,5_mM_Glc_+_IBMX", "16,7_mM_Glc_+_IBMX")
          )
          return(XX)
        })
      )
    })
  })
  QCsummarytableReac <- reactive({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
    load(file = "www/importedfilesinfo.Rdata")
    wrongNTP <- computeQC()[["wrongNTP"]]
    NTPdata <- NTPdata()
    NTPfail <- do.call("rbind", lapply(NTPdata, function(dta) {
      tab <- table(
        dta[, "Samples"],
        factor(is.na(dta[, "Insulin"]), levels = c(FALSE, TRUE))
      )[, "FALSE"]
      return(cbind.data.frame(
        Exp = unique(dta[, "Exp"]),
        ExpType = unique(dta[, "ExpType"]),
        Op = unique(dta[, "Op"]),
        Product = c("Glc", "Glc+IBMX"),
        Fail = !c(
          all(tab[c("0,5_mM_Glc", "16,7_mM_Glc")] >= 2),
          all(tab[c("0,5_mM_Glc_+_IBMX", "16,7_mM_Glc_+_IBMX")] >= 2)
        )
      ))
    }))
    wrongNTPfinal <- merge(
      x = NTPfail[which(NTPfail[, "Fail"]), c("Exp", "ExpType")],
      y = wrongNTP,
      by = c("Exp", "ExpType"),
      all = TRUE
    )
    lfiles <- as.data.frame(do.call(
      "rbind",
      strsplit(gsub("(.*)_Sheet_(.*).Rdata", "\\1-\\2", list.files(path = "www/CT", pattern = ".*.Rdata")), "-")
    ))
    colnames(lfiles) <- c("Exp", "ExpType")
    lfiles[, "Group"] <- paste(lfiles[, "Exp"], lfiles[, "ExpType"], sep = "-")
    lfiles[, "File"] <- sapply(
      paste0("^", lfiles[, "Exp"], ".xlsx"),
      grep,
      importedfilesinfo[, "Name"],
      value = TRUE
    )
    lfiles[, "Operator"] <- importedfilesinfo[sapply(
      paste0("^", lfiles[, "Exp"], ".xlsx"),
      grep,
      importedfilesinfo[, "Name"]
    ), "Operator"]
    lfiles[, "Status"] <- ifelse(
      lfiles[, "Group"] %in% paste(wrongNTPfinal[, "Exp"], wrongNTPfinal[, "ExpType"], sep = "-"),
      "Warning!",
      "Clear!"
    )
    NTPfile <- lfiles[, c("File", "ExpType", "Operator", "Status")]
    colnames(NTPfile) <- c("File", "Sheet", "Operator", "Status")
    importedfilesinfo[, "Check"] <- sapply(importedfilesinfo[, "Name"], function(ifile) {
      sum(NTPfile[NTPfile[, "File"] %in% ifile, "Status"] %in% "Clear!")
    })
    save(importedfilesinfo, file = "www/importedfilesinfo.Rdata")
    NTPfile[, "ID"] <- paste0(NTPfile[, "Sheet"], "; ", NTPfile[, "Operator"], " (", NTPfile[, "File"], ")")
    NTPfile[, "Group"] <- paste(gsub(".xlsx", "", NTPfile[, "File"]), NTPfile[, "Sheet"], sep = "-")
    return(NTPfile)
  })

  output$QCsummarytable <- renderDataTable({
    trash <- formatdata()
    return(QCsummarytableReac()[, c("File", "Sheet", "Operator", "Status")])
  }, options = list(pageLength = 10))

  experiments <- reactive({
    withProgress(message = "Loading...", value = NULL, {
      dataset <- computeQC()[["dataset"]]
      blankdta <- computeQC()[["blankdta"]]
      out <- computeQC()[["out"]]

      scaleNTPy <- c(0, max(dataset[, c("OD1", "OD2")], na.rm = TRUE))
      scaleBlankx <- range(log10(blankdta[, "mUL"] / 23)[is.finite(log10(blankdta[, "mUL"] / 23))], na.rm = TRUE)
      scaleBlanky <- range(log10(blankdta[, "OD"])[is.finite(log10(blankdta[, "OD"]))], na.rm = TRUE)

      validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
      NTPs <- lapply(NTPdata(), function(iNTPdata) {
        iNTP <- unique(iNTPdata[, "Exp"])
        YY <- subset(blankdta, Exp == iNTP & ExpType == unique(iNTPdata[, "ExpType"]))
        out <- subset(out, Exp == iNTP & ExpType == unique(iNTPdata[, "ExpType"]))

        XX <- gather(iNTPdata[, -grep("Insulin", colnames(iNTPdata))], "OD", "InsulinODvalue", c(2, 3))
        scaleNTPy <- c(0, max(iNTPdata[, c(2, 3)], na.rm = TRUE))
        dtaXX <- do.call("rbind", by(XX, XX[, "Samples"], function(dta) {
          cbind.data.frame(
            Samples = unique(dta[, "Samples", drop = FALSE], na.rm = TRUE),
            Mean = colMeans(dta[, "InsulinODvalue", drop = FALSE], na.rm = TRUE),
            Se = colSe(dta[, "InsulinODvalue", drop = FALSE], na.rm = TRUE),
            Sd = colSd(dta[, "InsulinODvalue", drop = FALSE], na.rm = TRUE)
          )
        }))
        dtaXX[, "Sd"] <- ifelse(is.na(dtaXX[, "Sd"]), 0, dtaXX[, "Sd"])

        tab <- table(iNTPdata[, "Samples"], factor(is.na(iNTPdata[, "Insulin"]), levels = c(FALSE, TRUE)))[, "FALSE"]
        if (all(tab[c("0,5_mM_Glc", "16,7_mM_Glc")] >= 2)) {
          anovaODA <- tidy(anova(lm(
            formula = InsulinODvalue ~ Samples,
            data = XX,
            subset = Samples %in% c("0,5_mM_Glc", "16,7_mM_Glc")
          )))[1, "p.value"]
        } else {
          anovaODA <- NA
        }
        if (all(tab[c("0,5_mM_Glc_+_IBMX", "16,7_mM_Glc_+_IBMX")] >= 2)) {
          anovaODB <- tidy(anova(lm(
            formula = InsulinODvalue ~ Samples,
            data = XX,
            subset = Samples %in% c("0,5_mM_Glc_+_IBMX", "16,7_mM_Glc_+_IBMX")
          )))[1, "p.value"]
        } else {
          anovaODB <- NA
        }
        anovaAnnot <- cbind.data.frame(
          Samples = c("Glc", "Glc+IBMX"),
          p = c(anovaODA, anovaODB),
          y = ifelse(is.na(c(anovaODA, anovaODB)), NA, c(
            max(dtaXX[dtaXX[, "Samples"] %in% c("0,5_mM_Glc", "16,7_mM_Glc"), "Mean"]) +
              max(dtaXX[dtaXX[, "Samples"] %in% c("0,5_mM_Glc", "16,7_mM_Glc"), "Sd"]),
            max(dtaXX[dtaXX[, "Samples"] %in% c("0,5_mM_Glc_+_IBMX", "16,7_mM_Glc_+_IBMX"), "Mean"]) +
              max(dtaXX[dtaXX[, "Samples"] %in% c("0,5_mM_Glc_+_IBMX", "16,7_mM_Glc_+_IBMX"), "Sd"])
          )),
          xmin = c(1, 3),
          x = c(1.5, 3.5),
          xmax = c(2, 4),
          labels = ifelse(is.na(c(anovaODA, anovaODB)), "NA", ifelse(c(anovaODA, anovaODB) < 0.05, "p<0.05", "ns"))
        )
        anovaAnnot[, "y"] <- anovaAnnot[, "y"] + max(scaleNTPy) * 0.10

        dtaXX[, "Mean"] <- ifelse(is.na(dtaXX[, "Mean"]), 0, dtaXX[, "Mean"])

        return(list(
          dtaXX = dtaXX,
          anovaAnnot = anovaAnnot,
          scaleNTPy = scaleNTPy,
          YY = YY,
          out = out,
          scaleBlankx = scaleBlankx,
          scaleBlanky = scaleBlanky,
          tab = tab,
          iNTPdata = iNTPdata
        ))
      })
      names(NTPs) <- sapply(NTPdata(), function(iNTPdata) {
        paste(unique(iNTPdata[, c("Exp", "ExpType")]), collapse = "-")
      })
      return(NTPs)
    })
  })

  output$CheckBoxGroupUi <- renderUI({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
    reftableQC <- QCsummarytableReac()
    refexp <- reftableQC[, "ID"]
    return(
      radioButtons(
        inputId = "NTPgroup",
        label = "Group By:",
        choices = list("Target" = "Sheet", "File" = "File"),
        selected = "Sheet",
        inline = TRUE
      )
    )
  })
  output$FoldChangeUi <- renderUI({
    if (input$FoldChangeUiActivate) {
      return(sliderInput(
        inputId = "FCthreshold",
        label = NULL,
        min = 0,
        max = 2,
        value = 1,
        step = 0.1
      ))
    } else {
      return(NULL)
    }
  })
  output$SelectGroupUi <- renderUI({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
    reftableQC <- QCsummarytableReac()
    validate(need(input$NTPgroup %in% c(colnames(reftableQC), "All"), ""))
    if (input$NTPgroup %in% "All") {
      refexp <- sort(unique(reftableQC[, "File"]))
      whichselect <- seq_along(refexp)
    } else {
      refexp <- sort(unique(reftableQC[, input$NTPgroup]))
      whichselect <- 1
    }
    return(selectInput(
      inputId = "Groupfilter",
      label = "Available Data:",
      choices = refexp,
      selected = refexp[whichselect],
      width = "100%",
      selectize = FALSE,
      multiple = TRUE,
      size = 10
    ))
  })
  output$NTPselectExperimentUi <- renderUI({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
    reftableQC <- QCsummarytableReac()
    if (is.null(input$NTPgroup) || input$NTPgroup %in% "All") {
      reftableQC <- reftableQC[reftableQC[, "Status"] %in% input$QCfilter, ]
    } else {
      reftableQC <- reftableQC[reftableQC[, "Status"] %in% input$QCfilter & reftableQC[, input$NTPgroup] %in% input$Groupfilter, ]
    }
    refexp <- sort(reftableQC[, "ID"])
    return(selectInput(
      inputId = "NTPselectExperiment",
      label = "Available Experiments:",
      choices = refexp,
      selected = refexp[unlist(ifelse(input$NTPgroup %in% "Sheet", list(seq_along(refexp)), list(1)))],
      width = "100%",
      selectize = FALSE,
      multiple = TRUE,
      size = 10
    ))
  })

  getindex <- reactive({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
    validate(need(length(input$NTPselectExperiment) != 0, "No data available!"))
    validate(need(length(input$NTPgroup) != 0, "No data available!"))

    reftableQC <- QCsummarytableReac()
    if (input$NTPgroup %in% "Sheet") {
      expslected <- reftableQC[reftableQC[, "ID"] %in% input$NTPselectExperiment, ]
      targets <- unique(c("siNTP", expslected[, "Sheet"]))
      reftableQCSheet <- reftableQC[reftableQC[, "File"] %in% expslected[, "File"] & reftableQC[, "Sheet"] %in% targets, ]
      index <- reftableQCSheet[, "Group"]
    } else {
      index <- reftableQC[reftableQC[, "ID"] %in% input$NTPselectExperiment, "Group"]
    }
    return(index)
  })

  FoldChangeNTP <- reactive({
    foldChangedta <- do.call("rbind", lapply(seq_along(experiments()), function(iindex) {
      iNTPdata <- experiments()[[iindex]][["iNTPdata"]]
      out <- cbind.data.frame(
        Samples = c("Glc", "Glc+IBMX"),
        Mean = rbind(
          iNTPdata[iNTPdata[, "Samples"] %in% c("16,7_mM_Glc"), "Insulin"] /
            iNTPdata[iNTPdata[, "Samples"] %in% c("0,5_mM_Glc"), "Insulin"],
          iNTPdata[iNTPdata[, "Samples"] %in% c("16,7_mM_Glc_+_IBMX"), "Insulin"] /
            iNTPdata[iNTPdata[, "Samples"] %in% c("0,5_mM_Glc_+_IBMX"), "Insulin"]
        ),
        Exp = iindex,
        Op = unique(iNTPdata[, "Op"]),
        ExpType = unique(iNTPdata[, "ExpType"])
      )
      out <- gather(out, "Mean", "Value", grep("Mean", colnames(out)))
      out <- out[, -grep("Mean", colnames(out))]
      return(out)
    }))
    foldChangedta[, "ExpShort"] <- gsub("-.*", "", foldChangedta[, "Exp"])
    if (is.null(input$FCthreshold)) {
      FCthreshold <- 1
    } else {
      FCthreshold <- input$FCthreshold
    }
    fcplot <- ggplot(data = subset(foldChangedta, Samples == "Glc")) +
      theme_minimal(base_size = input$fontsize) +
      geom_density(mapping = aes(x = Value, y = ..scaled..), colour = myPalette()[1], fill = myPalette()[1], alpha = 0.5) +
      geom_vline(xintercept = FCthreshold, linetype = 2, colour = myPalette()[3]) +
      geom_text(
        label = FCthreshold,
        x = FCthreshold,
        y = 1,
        colour = myPalette()[3],
        hjust = 1.05,
        vjust = 1,
        size = 6
      ) +
      scale_x_continuous(
        limits = range(pretty_breaks()(range(foldChangedta[, "Value"], na.rm = TRUE)), na.rm = TRUE)
      ) +
      labs(x = "Fold Change Insulin Secretion (Glc)", y = "Density") +
      theme(
        axis.text = element_text(colour = myPalette()[1], hjust = 0.65, face = "bold")
      )
    return(fcplot)
  })
  output$FoldChangeNTP_plot <- renderPlot({
    FoldChangeNTP()
  })
  output$FoldChangeNTP_plotdownload <- downloadHandler(
    filename = function() {
      return(paste0("FoldChange_siNTP_Distribution.", input$plotformat))
    },
    content = function(file) {
      ggsave(
        file = file,
        plot = FoldChangeNTP_plot() +
          theme(legend.title = element_blank()),
        width = input$plotwidth,
        height = input$plotheight,
        units = input$plotunits,
        dpi = as.numeric(input$plotdpi)
      )
    }
  )

  NTPAnalysis_plot1 <- reactive({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
    validate(need(length(input$NTPselectExperiment) != 0, "No data available!"))
    validate(need(length(input$NTPgroup) != 0, "No data available!"))
    legendOn <- ifelse(is.null(input$QCfilter), TRUE, "Legend" %in% input$QCfilter)

    index <- getindex()

    out <- do.call("rbind", lapply(index, function(iindex) {
      cbind.data.frame(experiments()[[iindex]][["out"]], Exp = iindex)
    }))
    YY <- do.call("rbind", lapply(index, function(iindex) {
      cbind.data.frame(experiments()[[iindex]][["YY"]], Exp = iindex)
    }))
    scaleBlankx <- range(unlist(lapply(index, function(iindex) {
      experiments()[[iindex]][["scaleBlankx"]]
    })), na.rm = TRUE)
    scaleBlanky <- range(unlist(lapply(index, function(iindex) {
      experiments()[[iindex]][["scaleBlanky"]]
    })), na.rm = TRUE)

    if (
      length(unique(gsub("(.*)-(.*)", "\\1", YY[, "Group"]))) == 1 &
        length(unique(gsub("(.*)-(.*)", "\\1", out[, "Group"]))) == 1
    ) {
      YY[, "Group"] <- gsub("(.*)-(.*)", "\\2", YY[, "Group"])
      xNTP <- grep("NTP", unique(YY[, "Group"]), value = TRUE)
      yNTP <- sort(setdiff(unique(YY[, "Group"]), xNTP))
      YY[, "Group"] <- factor(YY[, "Group"], levels = c(xNTP, yNTP))

      out[, "Group"] <- gsub("(.*)-(.*)", "\\2", out[, "Group"])
      xNTP <- grep("NTP", unique(out[, "Group"]), value = TRUE)
      yNTP <- sort(setdiff(unique(out[, "Group"]), xNTP))
      out[, "Group"] <- factor(out[, "Group"], levels = c(xNTP, yNTP))
    }
    if (length(unique(gsub("(.*)-(.*)", "\\2", YY[, "Group"]))) != 1) {
      palcolors <- rep(myPalette()[-1], ceiling(length(index) / length(myPalette()[-1])))
      levfactor <- levels(factor(YY[, "Group"]))
      palcolors[grep("NTP", levfactor)] <- myPalette()[1]
    } else {
      palcolors <- rep(myPalette(), ceiling(length(index) / length(myPalette())))
    }

    pout2 <- ggplot(
      data = subset(YY, OD != 0),
      mapping = aes(y = log10(OD), x = log10(mUL / 23), colour = Group, shape = Group)
    ) +
      theme_minimal(base_size = input$fontsize) +
      geom_point(size = 2) +
      geom_abline(data = subset(out, Type == "COR"), mapping = aes(intercept = intercept, slope = slope, colour = Group)) +
      labs(y = bquote(log[10](OD)), x = bquote("Insulin" ~ (log[10](mu * g / L)))) +
      scale_shape_manual(
        name = ifelse(any(subset(out, Type == "COR")[, "Warning"]), "Warning!", "Clear!"),
        values = rep(seq_along(myPalette()), ceiling(length(unique(YY[, "Group"])) / length(myPalette()))),
        labels = function(x) {
          gsub("CT_stat_(..).*_(.*)", "\\1_\\2", x)
        }
      ) +
      scale_colour_manual(
        name = ifelse(any(subset(out, Type == "COR")[, "Warning"]), "Warning!", "Clear!"),
        values = unlist(ifelse(length(index) == 1, "gray30", list(palcolors))),
        labels = function(x) {
          gsub("CT_stat_(..).*_(.*)", "\\1_\\2", x)
        }
      ) +
      scale_x_continuous(limits = scaleBlankx) +
      scale_y_continuous(limits = scaleBlanky) +
      theme(
        axis.text = element_text(colour = ifelse(myPalette()[1] == "dodgerblue", "dodgerblue", "black"), face = "bold"),
        legend.title = element_text(colour = myPalette()[3 - any(subset(out, Type == "COR")[, "Warning"])]),
        legend.position = "top",
        legend.justification = c(0.5, 0.5)
      ) +
      guides(
        shape = guide_legend(nrow = ceiling(length(unique(YY[, "Group"])) / 3)),
        colour = guide_legend(nrow = ceiling(length(unique(YY[, "Group"])) / 3))
      )
    if (!legendOn | length(palcolors) > 6) {
      pout2 <- pout2 + theme(legend.position = "none")
    }
    return(pout2)
  })
  output$NTPAnalysis_plot1 <- renderPlot({
    NTPAnalysis_plot1()
  })
  output$NTPAnalysis_plot1download <- downloadHandler(
    filename = function() {
      if (length(input$NTPselectExperiment) != 0) {
        index <- getindex()
        if (length(index) == 1) {
          return(paste0("BlankCurve_", index, ".", input$plotformat))
        } else {
          return(paste0("BlankCurve_Combine.", input$plotformat))
        }
      } else {
        return(NULL)
      }
    },
    content = function(file) {
      ggsave(
        file = file,
        plot = NTPAnalysis_plot1() +
          theme(legend.title = element_blank()),
        width = input$plotwidth,
        height = input$plotheight,
        units = input$plotunits,
        dpi = as.numeric(input$plotdpi)
      )
    }
  )

  NTPAnalysis_plot2 <- reactive({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
    validate(need(length(input$NTPselectExperiment) != 0, "No data available!"))
    validate(need(length(input$NTPgroup) != 0, "No data available!"))
    legendOn <- ifelse(is.null(input$QCfilter), TRUE, "Legend" %in% input$QCfilter)

    index <- getindex()

    widthbar <- 0.9
    scaleNTPy <- range(unlist(lapply(index, function(iindex) {
      experiments()[[iindex]][["scaleNTPy"]]
    })), na.rm = TRUE)
    dtaXX <- do.call("rbind", lapply(index, function(iindex) {
      cbind.data.frame(experiments()[[iindex]][["dtaXX"]], Exp = iindex)
    }))
    tab <- any(unlist(lapply(index, function(iindex) {
      experiments()[[iindex]][["tab"]] < 2
    })))
    anovaAnnot <- do.call("rbind", lapply(index, function(iindex) {
      cbind.data.frame(experiments()[[iindex]][["anovaAnnot"]], Exp = iindex)
    }))
    anovaAnnot <- do.call("rbind", lapply(index, function(iindex) {
      cbind.data.frame(experiments()[[iindex]][["anovaAnnot"]], Exp = iindex)
    }))
    anovaAnnot[, "y"] <- ifelse(is.na(anovaAnnot[, "y"]), NA, c(
      max(dtaXX[dtaXX[, "Samples"] %in% c("0,5_mM_Glc", "16,7_mM_Glc"), "Mean"]) +
        max(dtaXX[dtaXX[, "Samples"] %in% c("0,5_mM_Glc", "16,7_mM_Glc"), "Sd"]),
      max(dtaXX[dtaXX[, "Samples"] %in% c("0,5_mM_Glc_+_IBMX", "16,7_mM_Glc_+_IBMX"), "Mean"]) +
        max(dtaXX[dtaXX[, "Samples"] %in% c("0,5_mM_Glc_+_IBMX", "16,7_mM_Glc_+_IBMX"), "Sd"])
    ))
    dtaXX <- do.call("rbind", by(dtaXX, gsub("-.*", "", dtaXX[, "Exp"]), function(idtaXX) {
      xNTP <- sort(grep("NTP", unique(idtaXX[, "Exp"]), value = TRUE))
      yNTP <- sort(setdiff(unique(idtaXX[, "Exp"]), xNTP))
      idtaXX[, "Exp"] <- factor(idtaXX[, "Exp"], levels = c(xNTP, yNTP))
      idtaXX <- idtaXX[order(idtaXX[, "Exp"]), ]
      return(idtaXX)
    }))
    dtaXX[, "Exp"] <- factor(dtaXX[, "Exp"], levels = unique(dtaXX[, "Exp"]))
    anovaAnnot[, "Exp"] <- factor(anovaAnnot[, "Exp"], levels = levels(dtaXX[, "Exp"]))
    anovaAnnot <- anovaAnnot[order(anovaAnnot[, "Samples"], anovaAnnot[, "Exp"]), ]
    anovaAnnot <- do.call("rbind", by(anovaAnnot, anovaAnnot[, "xmin"], function(dta) {
      widthbar2 <- widthbar / 2
      nsize <- length(unique(anovaAnnot[, "Exp"]))
      dta[, "xmin"] <- seq(
        unique(dta[, "xmin"]) - widthbar2,
        unique(dta[, "xmin"]) + widthbar2,
        widthbar / nsize
      )[seq_len(nsize)] + widthbar2 / nsize
      dta[, "xmax"] <- seq(
        unique(dta[, "xmax"]) - widthbar2,
        unique(dta[, "xmax"]) + widthbar2,
        widthbar / nsize
      )[seq_len(nsize)] + widthbar2 / nsize
      return(dta)
    }))
    anovaAnnot[, "x"] <- (anovaAnnot[, "xmin"] + anovaAnnot[, "xmax"]) / 2
    anovaAnnot[, "y"] <- anovaAnnot[, "y"] + max(scaleNTPy) * rep(seq_along(unique(anovaAnnot[, "Exp"])) * 0.1, 2)
    if (length(unique(gsub("(.*)-(.*)", "\\2", dtaXX[, "Exp"]))) != 1) {
      palcolors <- rep(myPalette()[-1], ceiling(length(index) / length(myPalette()[-1])))
      levfactor <- levels(factor(dtaXX[, "Exp"]))
      palcolors[grep("NTP", levfactor)] <- myPalette()[1]
    } else {
      palcolors <- rep(myPalette(), ceiling(length(index) / length(myPalette())))
    }
    pout <- ggplot() +
      theme_minimal(base_size = input$fontsize) +
      geom_errorbar(
        data = dtaXX,
        mapping = aes(x = Samples, y = Mean, ymin = Mean * 0.95, ymax = Mean + Sd, colour = Exp, fill = Exp),
        width = 0.2,
        position = position_dodge(widthbar)
      ) +
      geom_bar(
        data = dtaXX,
        mapping = aes(x = Samples, y = Mean, colour = Exp, fill = Exp),
        stat = "identity",
        position = "dodge",
        width = widthbar
      ) +
      scale_colour_manual(
        name = paste0(ifelse(tab, "Warning!", "Clear!")),
        values = unlist(ifelse(length(index) == 1, "gray30", list(palcolors))),
        labels = function(x) {
          gsub("CT_stat_(..).*_(.*)", "\\1_\\2", x)
        }
      ) +
      scale_fill_manual(
        name = paste0(ifelse(tab, "Warning!", "Clear!")),
        values = unlist(ifelse(length(index) == 1, "gray30", list(palcolors))),
        labels = function(x) {
          gsub("CT_stat_(..).*_(.*)", "\\1_\\2", x)
        }
      )
    if (legendOn) {
      if (!all(is.na(anovaAnnot[, "y"])) & length(palcolors) <= 6) {
        anovaAnnot <- anovaAnnot[!is.na(anovaAnnot[, "y"]), ]
        if (input$reportstars) {
          anovaAnnot[, "labels"] <- factor(
            x = sapply(anovaAnnot[, "p"], signstars),
            levels = c("ns", "ns", "*", "**", "***")
          )
          pout <- pout +
            geom_errorbarh(data = anovaAnnot, mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, colour = Exp), height = 0)
          if (any(anovaAnnot[, "labels"] %in% "ns")) {
            pout <- pout +
              geom_text(
                data = subset(anovaAnnot, labels %in% "ns"),
                mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, label = labels, colour = Exp),
                vjust = -0.25,
                size = 5
              )
          }
          if (any(!anovaAnnot[, "labels"] %in% c("ns", "ns"))) {
            pout <- pout +
              geom_text(
                data = subset(anovaAnnot, !labels %in% c("ns", "ns")),
                mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, label = labels, colour = Exp),
                vjust = 0.40,
                size = 8
              )
          }
          if (any(anovaAnnot[, "labels"] %in% ".")) {
            pout <- pout +
              geom_text(
                data = subset(anovaAnnot, labels %in% "."),
                mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, label = labels, colour = Exp),
                vjust = -0.25,
                size = 10
              )
          }
        } else {
          pout <- pout +
            geom_errorbarh(
              data = anovaAnnot,
              mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, colour = Exp),
              height = 0
            ) +
            geom_text(
              data = anovaAnnot,
              mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, label = labels, colour = Exp),
              vjust = -0.25,
              size = 5
            )
        }
        scaleNTPy <- range(scaleNTPy, anovaAnnot[, "y"], na.rm = TRUE)
      }
    }
    pout <- pout +
      scale_x_discrete(labels = function(value) {
        gsub(",", ".", gsub("mM Glc", "mM\nGlc", gsub("_", " ", value)))
      }) +
      labs(x = "Stimulus", y = "Insulin secretion (OD)") +
      scale_y_continuous(limits = scaleNTPy) +
      theme(
        axis.text = element_text(colour = ifelse(myPalette()[1] == "dodgerblue", "dodgerblue", "black"), face = "bold"),
        legend.title = element_text(colour = myPalette()[3 - tab]),
        legend.position = "top",
        legend.justification = c(0.5, 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold")
      ) +
      guides(
        fill = guide_legend(nrow = ceiling(length(unique(dtaXX[, "Exp"])) / 3)),
        colour = guide_legend(nrow = ceiling(length(unique(dtaXX[, "Exp"])) / 3))
      )
    if (!legendOn) {
      pout <- pout + theme(legend.position = "none")
    }
    return(pout)
  })
  output$NTPAnalysis_plot2 <- renderPlot({
    NTPAnalysis_plot2()
  })
  output$NTPAnalysis_plot2download <- downloadHandler(
    filename = function() {
      if (length(input$NTPselectExperiment) != 0) {
        index <- getindex()
        if (length(index) == 1) {
          return(paste0("InsulinSecretion_", index, ".", input$plotformat))
        } else {
          return(paste0("InsulinSecretion_Combine.", input$plotformat))
        }
      } else {
        return(NULL)
      }
    },
    content = function(file) {
      ggsave(
        file = file,
        plot = NTPAnalysis_plot2() +
          theme(legend.title = element_blank()),
        width = input$plotwidth,
        height = input$plotheight,
        units = input$plotunits,
        dpi = as.numeric(input$plotdpi)
      )
    }
  )

  NTPAnalysis_plot2bis <- reactive({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
    validate(need(length(input$NTPselectExperiment) != 0, "No data available!"))
    validate(need(length(input$NTPgroup) != 0, "No data available!"))
    legendOn <- ifelse(is.null(input$QCfilter), TRUE, "Legend" %in% input$QCfilter)

    index <- getindex()

    widthbar <- 0.9
    dtaXX <- do.call("rbind", lapply(index, function(iindex) {
      tmp <- experiments()[[iindex]][["iNTPdata"]]
      do.call("rbind", by(tmp, tmp[, "Samples"], function(tmpdta) {
        cbind.data.frame(tmpdta, Replicates = seq_len(nrow(tmpdta)))
      }))
    }))
    rownames(dtaXX) <- NULL
    validate(need(any(!is.na(dtaXX[, "Insulin"])), "No Insulin data available!"))

    tab <- any(unlist(lapply(index, function(iindex) {
      experiments()[[iindex]][["tab"]] < 2
    })))

    scaleNTPy <- range(unlist(lapply(index, function(iindex) {
      experiments()[[iindex]][["iNTPdata"]][, "Insulin"]
    })), na.rm = TRUE)

    dtaXX <- do.call("rbind", by(dtaXX, gsub("-.*", "", dtaXX[, "Exp"]), function(idtaXX) {
      xNTP <- sort(grep("NTP", unique(idtaXX[, "Exp"]), value = TRUE))
      yNTP <- sort(setdiff(unique(idtaXX[, "Exp"]), xNTP))
      idtaXX[, "Exp"] <- factor(idtaXX[, "Exp"], levels = c(xNTP, yNTP))
      idtaXX <- idtaXX[order(idtaXX[, "Exp"]), ]
      idtaXX[, "PrettySamples"] <- gsub("_", "", gsub(".*mM_", "", idtaXX[, "Samples"]))
      return(idtaXX)
    }))
    rownames(dtaXX) <- NULL
    dtaXX[, "Exp"] <- factor(dtaXX[, "Exp"], levels = unique(dtaXX[, "Exp"]))

    foldChangedta <- do.call("rbind", lapply(index, function(iindex) {
      iNTPdata <- experiments()[[iindex]][["iNTPdata"]]
      out <- cbind.data.frame(
        Samples = c("Glc", "Glc+IBMX"),
        Mean = rbind(
          iNTPdata[iNTPdata[, "Samples"] %in% c("16,7_mM_Glc"), c("Insulin")] /
            iNTPdata[iNTPdata[, "Samples"] %in% c("0,5_mM_Glc"), c("Insulin")],
          iNTPdata[iNTPdata[, "Samples"] %in% c("16,7_mM_Glc_+_IBMX"), c("Insulin")] /
            iNTPdata[iNTPdata[, "Samples"] %in% c("0,5_mM_Glc_+_IBMX"), c("Insulin")]
        ),
        Exp = iindex,
        Op = unique(iNTPdata[, "Op"]),
        ExpType = unique(iNTPdata[, "ExpType"])
      )
      out <- gather(out, "Replicates", "FCValue", grep("Mean", colnames(out)))
      out[, "Replicates"] <- gsub("Mean.", "", out[, "Replicates"])
      return(out)
    }))
    foldChangedta[, "ExpShort"] <- gsub("-.*", "", foldChangedta[, "Exp"])

    FullDta <- do.call("rbind", by(index, gsub("-.*", "", index), function(iindex) {
      YYY <- subset(foldChangedta, Exp %in% iindex)
      XXX <- subset(dtaXX, Group %in% iindex)
      ZZZ <- merge(
        x = XXX,
        y = YYY,
        by.x = c("Group", "ExpType", "Replicates", "PrettySamples", "Op", "Exp"),
        by.y = c("Exp", "ExpType", "Replicates", "Samples", "Op", "ExpShort")
      )
      if (input$FoldChangeUiActivate) {
        checkFCsiNTP <- ZZZ[grep("NTP", ZZZ[, "ExpType"]), ]
        checkFCsiNTP[which(checkFCsiNTP[, "FCValue"] < input$FCthreshold), c("Insulin", "FCValue")] <- NA
        lowFCvalue <- names(which(table(
          paste(checkFCsiNTP[, "Exp"], checkFCsiNTP[, "Samples"], sep = ":"),
          factor(!is.na(checkFCsiNTP[, "FCValue"]), levels = c(FALSE, TRUE))
        )[, "FALSE"] >= 2))
        ZZZ[, "LowFC"] <- paste(ZZZ[, "Exp"], ZZZ[, "Samples"], sep = ":") %in% lowFCvalue
      } else {
        ZZZ[, "LowFC"] <- FALSE
      }
      return(ZZZ)
    }))
    rownames(FullDta) <- NULL
    FullDta[, "InsulinAll"] <- FullDta[, "Insulin"]
    FullDta[FullDta[, "LowFC"], "Insulin"] <- NA
    if (length(unique(FullDta[, "ExpType"])) != 1) {
      xNTP <- grep("NTP", unique(FullDta[, "ExpType"]), value = TRUE)
      yNTP <- sort(setdiff(unique(FullDta[, "ExpType"]), xNTP))
      FullDta[, "ExpType"] <- factor(FullDta[, "ExpType"], levels = c(xNTP, yNTP))
    }

    if (length(unique(gsub("(.*)-(.*)", "\\2", FullDta[, "ExpType"]))) != 1) {
      palcolors <- rep(myPalette()[-1], ceiling(length(index) / length(myPalette()[-1])))
      levfactor <- levels(factor(FullDta[, "ExpType"]))
      palcolors[grep("NTP", levfactor)] <- myPalette()[1]
    } else {
      palcolors <- rep(myPalette(), ceiling(length(index) / length(myPalette())))
    }

    if (!input$removeoutlier) {
      FullDta <- do.call("rbind", by(FullDta, FullDta[, c("Samples", "ExpType")], function(idta) {
        subset(
          x = idta,
          subset = (Insulin <= median(Insulin, na.rm = TRUE) + 1.5 * IQR(Insulin, na.rm = TRUE)) &
            (Insulin >= median(Insulin, na.rm = TRUE) - 0.5 * IQR(Insulin, na.rm = TRUE))
        )
      }))
    }
    pout <- ggplot(data = FullDta, mapping = aes(x = Samples, y = Insulin, colour = ExpType, fill = ExpType)) +
      theme_minimal(base_size = input$fontsize) +
      geom_boxplot(fill = "white", outlier.shape = ifelse(input$removeallpoints | !input$removeoutlier, NA, 16))
    if (input$removeallpoints) {
      pout <- pout +
        geom_point(position = position_jitterdodge(jitter.width = widthbar / 2), shape = 4)
      if (input$removelowfC) {
        pout <- pout +
          geom_point(
            data = subset(FullDta, LowFC),
            mapping = aes(y = InsulinAll),
            colour = myPalette()[2],
            position = position_jitterdodge(jitter.width = widthbar / 2),
            shape = 3
          )
      }
    }
    pout <- ggplot(
      data = FullDta, 
      mapping = aes(x = Samples, y = Insulin, colour = ExpType, fill = ExpType)
    ) + 
      theme_minimal(base_size = input$fontsize) +
      geom_boxplot(
        fill = "white", 
        outlier.shape = ifelse(input$removeallpoints | !input$removeoutlier, NA, 16)
      )
    if (input$removeallpoints) {
      pout <- pout + 
        geom_point(position = position_jitterdodge(jitter.width = widthbar / 2), shape = 4)
      if (input$removelowfC) {
        pout <- pout + 
          geom_point(
            data = subset(FullDta, LowFC), 
            mapping = aes(y = InsulinAll), 
            colour = myPalette()[2], 
            position = position_jitterdodge(jitter.width = widthbar / 2), 
            shape = 3
          )
      }
    }
    pout <- pout + 
      scale_colour_manual(
        name = paste0(ifelse(tab, "Warning!", "Clear!")),
        values = unlist(ifelse(length(index) == 1, "gray30", list(palcolors))),
        labels = function(x) {
          gsub("CT_stat_(..).*_(.*)", "\\1_\\2", x)
        }
      ) +
      scale_fill_manual(
        name = paste0(ifelse(tab, "Warning!", "Clear!")),
        values = unlist(ifelse(length(index) == 1, "gray30", list(palcolors))),
        labels = function(x) {
          gsub("CT_stat_(..).*_(.*)", "\\1_\\2", x)
        }
      ) +
      scale_x_discrete(labels = function(value) {
        gsub(",", ".", gsub("mM Glc", "mM\nGlc", gsub("_", " ", value)))
      }) +
      labs(x = "Stimulus", y = "Insulin secretion (% of content)") +
      # scale_y_continuous(limits = scaleNTPy) +
      theme(
        axis.text = element_text(
          colour = ifelse(myPalette()[1] == "dodgerblue", "dodgerblue", "black"), 
          face = "bold"
        ),
        legend.title = element_text(colour = myPalette()[3 - tab]),
        legend.position = "top",
        legend.justification = c(0.5, 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold")
      ) +
      guides(
        fill = guide_legend(nrow = ceiling(length(unique(dtaXX[, "ExpType"])) / 3)),
        colour = guide_legend(nrow = ceiling(length(unique(dtaXX[, "ExpType"])) / 3))
      )
    
    test_check <- length(unique(FullDta[, "ExpType"]))==2 & 
      any(grepl("siNTP", FullDta[, "ExpType"])) &
      all(table(na.exclude(FullDta[, c("Insulin", "ExpType", "Samples")])[, c("Samples", "ExpType")])>=3)
    
    if (test_check) {
      lm_ins <- do.call(
        "rbind",
        by(data = FullDta, INDICES = FullDta[, "Samples"], FUN = function(idta) {
          out <- broom::tidy(lm(formula = Insulin ~ ExpType, data = idta))
          out <- out[-grep("Intercept", out[, "term"]), ]
          out[, "term"] <- gsub("ExpType", "", out[, "term"])
          out[, "y"] <- max(idta[, "Insulin"], na.rm = TRUE) + 0.1 * diff(range(idta[, "Insulin"], na.rm = TRUE))
          out
        })
      )
      lm_ins <- as.data.frame(lm_ins)
      lm_ins[, "labels"] <- prettyNum(lm_ins[, "p.value"], digits = 3)
      lm_ins[, "Samples"] <- factor(rownames(lm_ins), levels = levels(FullDta[, "Samples"]))
      lm_ins[, "x"] <- as.numeric(lm_ins[, "Samples"])
      lm_ins[, "xmin"] <- lm_ins[, "x"] - 0.45 / 2
      lm_ins[, "xmax"] <- lm_ins[, "x"] + 0.45 / 2

      if (legendOn & !all(is.na(lm_ins[, "y"]))) {
        if (input$reportstars) {
          lm_ins[, "labels"] <- factor(sapply(lm_ins[, "p.value"], signstars), levels = c("ns", "ns", "*", "**", "***"))
          pout <- pout +
            geom_errorbarh(
              data = lm_ins,
              mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax),
              height = 0, 
              inherit.aes = FALSE, 
              colour = "grey30"
            )
          if (any(lm_ins[, "labels"] %in% "ns")) {
            pout <- pout +
              geom_text(
                data = subset(lm_ins, labels %in% "ns"),
                mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, label = labels),
                vjust = -0.25,
                size = 5, 
                inherit.aes = FALSE, 
                colour = "grey30"
              )
          }
          if (any(!lm_ins[, "labels"] %in% c("ns", "ns"))) {
            pout <- pout +
              geom_text(
                data = subset(lm_ins, !labels %in% c("ns", "ns")),
                mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, label = labels),
                vjust = 0.40,
                size = 8, 
                inherit.aes = FALSE, 
                colour = "grey30"
              )
          }
          if (any(lm_ins[, "labels"] %in% ".")) {
            pout <- pout +
              geom_text(
                data = subset(lm_ins, labels %in% "."),
                mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, label = labels),
                vjust = -0.25,
                size = 10, 
                inherit.aes = FALSE, 
                colour = "grey30"
              )
          }
        } else {
          pout <- pout +
            geom_errorbarh(
              data = lm_ins,
              mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax),
              height = 0, 
              inherit.aes = FALSE, 
              colour = "grey30"
            ) +
            geom_text(
              data = lm_ins,
              mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, label = labels),
              vjust = -0.25,
              size = 5, 
              inherit.aes = FALSE, 
              colour = "grey30"
            )
        }
      }
    }
    
    return(pout)
  })
  output$NTPAnalysis_plot2bis <- renderPlot({
    NTPAnalysis_plot2bis()
  })
  output$NTPAnalysis_plot2bisdownload <- downloadHandler(
    filename = function() {
      if (length(input$NTPselectExperiment) != 0) {
        index <- getindex()
        if (length(index) == 1) {
          return(paste0("InsulinSecretionPercent_", index, ".", input$plotformat))
        } else {
          return(paste0("InsulinSecretionPercent_Combine.", input$plotformat))
        }
      } else {
        return(NULL)
      }
    },
    content = function(file) {
      ggsave(
        file = file,
        plot = NTPAnalysis_plot2bis() +
          theme(legend.title = element_blank()),
        width = input$plotwidth,
        height = input$plotheight,
        units = input$plotunits,
        dpi = as.numeric(input$plotdpi)
      )
    }
  )

  NTPAnalysis_plot3 <- reactive({
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
    validate(need(length(input$NTPselectExperiment) != 0, "No data available!"))
    validate(need(length(input$NTPgroup) != 0, "No data available!"))
    legendOn <- ifelse(is.null(input$QCfilter), TRUE, "Legend" %in% input$QCfilter)

    index <- getindex()

    widthbar <- 0.9
    dtaXX <- do.call("rbind", lapply(index, function(iindex) {
      tmp <- experiments()[[iindex]][["iNTPdata"]]
      do.call("rbind", by(tmp, tmp[, "Samples"], function(tmpdta) {
        cbind.data.frame(tmpdta, Replicates = seq_len(nrow(tmpdta)))
      }))
    }))
    rownames(dtaXX) <- NULL
    validate(need(any(!is.na(dtaXX[, "Insulin"])), "No Insulin data available!"))

    tab <- any(unlist(lapply(index, function(iindex) {
      experiments()[[iindex]][["tab"]] < 2
    })))

    scaleNTPy <- range(unlist(lapply(index, function(iindex) {
      experiments()[[iindex]][["iNTPdata"]][, "Insulin"]
    })), na.rm = TRUE)

    dtaXX <- do.call("rbind", by(dtaXX, gsub("-.*", "", dtaXX[, "Exp"]), function(idtaXX) {
      xNTP <- sort(grep("NTP", unique(idtaXX[, "Exp"]), value = TRUE))
      yNTP <- sort(setdiff(unique(idtaXX[, "Exp"]), xNTP))
      idtaXX[, "Exp"] <- factor(idtaXX[, "Exp"], levels = c(xNTP, yNTP))
      idtaXX <- idtaXX[order(idtaXX[, "Exp"]), ]
      idtaXX[, "PrettySamples"] <- gsub("_", "", gsub(".*mM_", "", idtaXX[, "Samples"]))
      return(idtaXX)
    }))
    rownames(dtaXX) <- NULL
    dtaXX[, "Exp"] <- factor(dtaXX[, "Exp"], levels = unique(dtaXX[, "Exp"]))

    foldChangedta <- do.call("rbind", lapply(index, function(iindex) {
      iNTPdata <- experiments()[[iindex]][["iNTPdata"]]
      out <- cbind.data.frame(
        Samples = c("Glc", "Glc+IBMX"),
        Mean = rbind(
          iNTPdata[iNTPdata[, "Samples"] %in% c("16,7_mM_Glc"), c("Insulin")] /
            iNTPdata[iNTPdata[, "Samples"] %in% c("0,5_mM_Glc"), c("Insulin")],
          iNTPdata[iNTPdata[, "Samples"] %in% c("16,7_mM_Glc_+_IBMX"), c("Insulin")] /
            iNTPdata[iNTPdata[, "Samples"] %in% c("0,5_mM_Glc_+_IBMX"), c("Insulin")]
        ),
        Exp = iindex,
        Op = unique(iNTPdata[, "Op"]),
        ExpType = unique(iNTPdata[, "ExpType"])
      )
      out <- gather(out, "Replicates", "FCValue", grep("Mean", colnames(out)))
      out[, "Replicates"] <- gsub("Mean.", "", out[, "Replicates"])
      return(out)
    }))
    foldChangedta[, "ExpShort"] <- gsub("-.*", "", foldChangedta[, "Exp"])

    FullDta <- do.call("rbind", by(index, gsub("-.*", "", index), function(iindex) {
      YYY <- subset(foldChangedta, Exp %in% iindex)
      XXX <- subset(dtaXX, Group %in% iindex)
      ZZZ <- merge(
        x = XXX,
        y = YYY,
        by.x = c("Group", "ExpType", "Replicates", "PrettySamples", "Op", "Exp"),
        by.y = c("Exp", "ExpType", "Replicates", "Samples", "Op", "ExpShort")
      )
      if (input$FoldChangeUiActivate) {
        checkFCsiNTP <- ZZZ[grep("NTP", ZZZ[, "ExpType"]), ]
        checkFCsiNTP[which(checkFCsiNTP[, "FCValue"] < input$FCthreshold), c("Insulin", "FCValue")] <- NA
        lowFCvalue <- names(which(table(
          paste(checkFCsiNTP[, "Exp"], checkFCsiNTP[, "Samples"], sep = ":"),
          factor(!is.na(checkFCsiNTP[, "FCValue"]), levels = c(FALSE, TRUE))
        )[, "FALSE"] >= 2))
        ZZZ[, "LowFC"] <- paste(ZZZ[, "Exp"], ZZZ[, "Samples"], sep = ":") %in% lowFCvalue
      } else {
        ZZZ[, "LowFC"] <- FALSE
      }
      return(ZZZ)
    }))
    rownames(FullDta) <- NULL
    if (length(unique(FullDta[, "ExpType"])) != 1) {
      xNTP <- grep("NTP", unique(FullDta[, "ExpType"]), value = TRUE)
      yNTP <- sort(setdiff(unique(FullDta[, "ExpType"]), xNTP))
      FullDta[, "ExpType"] <- factor(FullDta[, "ExpType"], levels = c(xNTP, yNTP))
    }

    if (length(unique(gsub("(.*)-(.*)", "\\2", FullDta[, "ExpType"]))) != 1) {
      palcolors <- rep(myPalette()[-1], ceiling(length(index) / length(myPalette()[-1])))
      levfactor <- levels(factor(FullDta[, "ExpType"]))
      palcolors[grep("NTP", levfactor)] <- myPalette()[1]
    } else {
      palcolors <- rep(myPalette(), ceiling(length(index) / length(myPalette())))
    }

    FullDtaUnique <- unique(FullDta[, c("Group", "ExpType", "Replicates", "PrettySamples", "Op", "Exp", "FCValue", "LowFC")])
    colnames(FullDtaUnique) <- c("Group", "ExpType", "Replicates", "Samples", "Op", "ExpShort", "Value", "LowFC")
    FullDtaUnique[FullDtaUnique[, "LowFC"], "Value"] <- NA
    foldChangedtaplot <- do.call("rbind", by(FullDtaUnique, FullDtaUnique[, c("Samples", "ExpType")], function(dta) {
      dta[, "Mean"] <- mean(dta[, "Value"], na.rm = TRUE)
      if (input$seorsd == "SD") {
        dta[, "Sd"] <- sd(dta[, "Value"], na.rm = TRUE)
      } else {
        dta[, "Sd"] <- se(dta[, "Value"], na.rm = TRUE)
      }
      dta[, "N"] <- sum(!is.na(dta[, "Value"]))
      dta <- dta[, c("Samples", "ExpType", "Mean", "Sd", "N")]
      return(unique(dta))
    }))
    if (length(unique(foldChangedtaplot[, "ExpType"])) != 1) {
      xNTP <- grep("NTP", unique(foldChangedtaplot[, "ExpType"]), value = TRUE)
      yNTP <- sort(setdiff(unique(foldChangedtaplot[, "ExpType"]), xNTP))
      foldChangedtaplot[, "ExpType"] <- factor(foldChangedtaplot[, "ExpType"], levels = c(xNTP, yNTP))
    }
    if (length(unique(foldChangedtaplot[, "ExpType"])) != 1) {
      palcolors <- rep(myPalette()[-1], ceiling(length(index) / length(myPalette()[-1])))
      levfactor <- levels(factor(foldChangedtaplot[, "ExpType"]))
      palcolors[grep("NTP", levfactor)] <- myPalette()[1]
    } else {
      palcolors <- rep(myPalette(), ceiling(length(index) / length(myPalette())))
    }

    enoughExp <- length(unique(FullDtaUnique[!is.na(FullDtaUnique[, "Value"]), "ExpType"])) != 1
    enoughMeasures <- by(FullDtaUnique, FullDtaUnique[, "Samples"], function(ifcdta) {
      all(table(factor(!is.na(ifcdta[, "Value"]), levels = c(FALSE, TRUE)), ifcdta[, "ExpType"])["TRUE", ] >= 3)
    })
    validate(need(any(!is.na(FullDtaUnique[, "Value"])), "Fold change cannot be computed for low quality reasons!"))

    if (enoughExp) {
      if (length(unique(FullDtaUnique[!is.na(FullDtaUnique[, "Value"]), "Op"])) != 1) {
        if (length(unique(FullDtaUnique[!is.na(FullDtaUnique[, "Value"]), "ExpShort"])) != 1) {
          form0 <- Value ~ Samples + ExpType + ExpShort + Op
          form1 <- Value ~ ExpType + ExpShort + Op
        } else {
          form0 <- Value ~ Samples + ExpType + Op
          form1 <- Value ~ ExpType + Op
        }
      } else {
        if (length(unique(FullDtaUnique[!is.na(FullDtaUnique[, "Value"]), "ExpShort"])) != 1) {
          form0 <- Value ~ Samples + ExpType + ExpShort
          form1 <- Value ~ ExpType + ExpShort
        } else {
          form0 <- Value ~ Samples + ExpType
          form1 <- Value ~ ExpType
        }
      }
      form0 <- update(form0, . ~ . - Op)
      form1 <- update(form1, . ~ . - Op)
      if (any(!enoughMeasures)) {
        form0 <- update(form0, . ~ . - Samples)
        lmRes_GLC_IBMX <- tidy(lm(form0, data = FullDtaUnique))
      } else {
        lmRes_GLC_IBMX <- tidy(lm(form0, data = FullDtaUnique))
      }

      if (enoughMeasures[1]) {
        if (length(unique(subset(FullDtaUnique, Samples == "Glc" & !is.na(Value))[, "ExpShort"])) == 1) {
          form1b <- update(form1, . ~ . - ExpShort)
        } else {
          form1b <- form1
        }
        lmRes_NTP_GLC <- tidy(lm(form1b, data = subset(FullDtaUnique, Samples == "Glc")))
      } else {
        lmRes_NTP_GLC <- as.data.frame(matrix(
          data = NA,
          nrow = 0,
          ncol = ncol(lmRes_GLC_IBMX),
          dimnames = list(NULL, colnames(lmRes_GLC_IBMX))
        ))
      }
      if (enoughMeasures[2]) {
        if (length(unique(subset(FullDtaUnique, Samples == "Glc+IBMX" & !is.na(Value))[, "ExpShort"])) == 1) {
          form1b <- update(form1, . ~ . - ExpShort)
        } else {
          form1b <- form1
        }
        lmRes_NTP_GLCIBMX <- tidy(lm(form1b, data = subset(FullDtaUnique, Samples == "Glc+IBMX")))
      } else {
        lmRes_NTP_GLCIBMX <- as.data.frame(matrix(
          data = NA,
          nrow = 0,
          ncol = ncol(lmRes_GLC_IBMX),
          dimnames = list(NULL, colnames(lmRes_GLC_IBMX))
        ))
      }
    } else {
      if (length(unique(FullDtaUnique[!is.na(FullDtaUnique[, "Value"]), "Op"])) != 1) {
        if (length(unique(FullDtaUnique[!is.na(FullDtaUnique[, "Value"]), "ExpShort"])) != 1) {
          form0 <- Value ~ Samples + ExpShort + Op
        } else {
          form0 <- Value ~ Samples + Op
        }
      } else {
        if (length(unique(FullDtaUnique[!is.na(FullDtaUnique[, "Value"]), "ExpShort"])) != 1) {
          form0 <- Value ~ Samples + ExpShort
        } else {
          form0 <- Value ~ Samples
        }
      }
      form0 <- update(form0, . ~ . - Op)
      if (any(!enoughMeasures)) {
        form0 <- update(form0, . ~ . - Samples)
        lmRes_GLC_IBMX <- tidy(lm(form0, data = FullDtaUnique))
      } else {
        lmRes_GLC_IBMX <- tidy(lm(form0, data = FullDtaUnique))
      }
      lmRes_NTP_GLC <- as.data.frame(matrix(
        data = NA,
        nrow = 0,
        ncol = ncol(lmRes_GLC_IBMX),
        dimnames = list(NULL, colnames(lmRes_GLC_IBMX))
      ))
      lmRes_NTP_GLCIBMX <- as.data.frame(matrix(
        data = NA,
        nrow = 0,
        ncol = ncol(lmRes_GLC_IBMX),
        dimnames = list(NULL, colnames(lmRes_GLC_IBMX))
      ))
    }

    if (enoughExp) {
      lmAnnot <- rbind.data.frame(
        cbind(Samples = "Global", lmRes_GLC_IBMX[grep("Samples", lmRes_GLC_IBMX[, "term"])[1], c("term", "p.value")]),
        if (enoughMeasures[1]) {
          cbind.data.frame(
            Samples = "Glc",
            lmRes_NTP_GLC[grep("ExpType", lmRes_NTP_GLC[, "term"]), c("term", "p.value")]
          )
        } else {
          cbind.data.frame(
            Samples = "Glc",
            term = levels(FullDtaUnique[, "ExpType"])[1],
            p.value = NA
          )
        },
        if (enoughMeasures[2]) {
          cbind.data.frame(
            Samples = "Glc+IBMX",
            lmRes_NTP_GLCIBMX[grep("ExpType", lmRes_NTP_GLCIBMX[, "term"]), c("term", "p.value")]
          )
        } else {
          cbind.data.frame(
            Samples = "Glc+IBMX",
            term = levels(FullDtaUnique[, "ExpType"])[2],
            p.value = NA
          )
        }
      )
      lmAnnot[, "ExpType"] <- gsub("ExpType", "", lmAnnot[, "term"])
      lmAnnot[, "ExpType"] <- gsub("Samples", "", lmAnnot[, "ExpType"])
      lmAnnot[, "labels"] <- ifelse(is.na(lmAnnot[, "p.value"]), NA, ifelse(lmAnnot[, "p.value"] < 0.05, "p<0.05", "ns"))
      lmAnnot <- do.call("rbind", by(lmAnnot, lmAnnot[, "Samples"], function(dta) {
        if (unique(dta[, "Samples"]) == "Global") {
          dta[, "y"] <- ifelse(is.na(dta[, "p.value"]), NA, max(rowSums(foldChangedtaplot[, c("Mean", "Sd")])))
        } else {
          dta[, "y"] <- ifelse(
            is.na(dta[, "p.value"]),
            NA,
            max(rowSums(
              foldChangedtaplot[foldChangedtaplot[, "Samples"] %in% unique(dta[, "Samples"]), c("Mean", "Sd")]
            ))
          )
        }
        return(dta)
      }))
      lmAnnot <- subset(lmAnnot, Samples != "Global")
      lmAnnot <- lmAnnot[order(lmAnnot[, "Samples"], lmAnnot[, "ExpType"]), ]
      nsize <- length(unique(lmAnnot[, "ExpType"])) + 1
      lmAnnot[, "xmin"] <- unlist(ifelse(
        lmAnnot[, "Samples"] == "Glc",
        list(seq(1 - widthbar / 2, 1 + widthbar / 2, widthbar / nsize)[1] + widthbar / (nsize * 2)),
        list(seq(2 - widthbar / 2, 2 + widthbar / 2, widthbar / nsize)[1] + widthbar / (nsize * 2))
      ))
      lmAnnot <- do.call("rbind", by(lmAnnot, lmAnnot[, "Samples"], function(dta) {
        if (unique(dta[, "Samples"]) == "Glc") {
          dta[, "xmax"] <- seq(1 - widthbar / 2, 1 + widthbar / 2, widthbar / nsize)[as.integer(as.factor(dta[, "ExpType"])) + 1] +
            widthbar / (nsize * 2)
        } else {
          dta[, "xmax"] <- seq(2 - widthbar / 2, 2 + widthbar / 2, widthbar / nsize)[as.integer(as.factor(dta[, "ExpType"])) + 1] +
            widthbar / (nsize * 2)
        }
        return(dta)
      }))

      lmAnnot[, "x"] <- (lmAnnot[, "xmin"] + lmAnnot[, "xmax"]) / 2

      scaleFCy <- max(rowSums(foldChangedtaplot[, c("Mean", "Sd")]), na.rm = TRUE)

      lmAnnot <- lmAnnot[!is.na(lmAnnot[, "y"]), ]
      lmAnnot[, "y"] <- c(
        lmAnnot[, "y"] + max(scaleFCy) * rep(seq_along(unique(lmAnnot[, "ExpType"])) * 0.10, 2)
      )[seq_len(nrow(lmAnnot))]
      lmAnnot[, "ExpType"] <- factor(lmAnnot[, "ExpType"], levels = levels(foldChangedtaplot[, "ExpType"]))
    }

    pout <- ggplot() +
      theme_minimal(base_size = input$fontsize) +
      geom_errorbar(
        data = foldChangedtaplot,
        mapping = aes(x = Samples, y = Mean, ymin = Mean * 0.95, ymax = Mean + Sd, colour = ExpType, fill = ExpType),
        width = 0.2, position = position_dodge(widthbar)
      ) +
      geom_bar(
        data = foldChangedtaplot,
        mapping = aes(x = Samples, y = Mean, colour = ExpType, fill = ExpType),
        stat = "identity",
        position = "dodge",
        width = widthbar
      )
    if (input$drawn) {
      pout <- pout + geom_text(
        data = foldChangedtaplot,
        mapping = aes(x = Samples, y = Mean, label = paste0("N=", N), colour = ExpType, fill = ExpType),
        position = position_dodge(widthbar),
        colour = ifelse(myPalette()[1] == "dodgerblue", "gray30", "white"),
        hjust = 0.5,
        vjust = 2,
        size = 5
      )
    }
    pout <- pout +
      scale_colour_manual(
        name = paste0(ifelse(tab, "Warning!", "Clear!")),
        values = unlist(ifelse(length(index) == 1, "gray30", list(palcolors)))
      ) +
      scale_fill_manual(
        name = paste0(ifelse(tab, "Warning!", "Clear!")),
        values = unlist(ifelse(length(index) == 1, "gray30", list(palcolors)))
      )
    if (legendOn & enoughExp) {
      if (!all(is.na(lmAnnot[, "y"]))) {
        if (input$reportstars) {
          lmAnnot[, "labels"] <- factor(sapply(lmAnnot[, "p.value"], signstars), levels = c("ns", "ns", "*", "**", "***"))
          pout <- pout +
            geom_errorbarh(data = lmAnnot, mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, colour = ExpType), height = 0)
          if (any(lmAnnot[, "labels"] %in% "ns")) {
            pout <- pout +
              geom_text(
                data = subset(lmAnnot, labels %in% "ns"),
                mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, label = labels, colour = ExpType),
                vjust = -0.25,
                size = 5
              )
          }
          if (any(!lmAnnot[, "labels"] %in% c("ns", "ns"))) {
            pout <- pout +
              geom_text(
                data = subset(lmAnnot, !labels %in% c("ns", "ns")),
                mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, label = labels, colour = ExpType),
                vjust = 0.40,
                size = 8
              )
          }
          if (any(lmAnnot[, "labels"] %in% ".")) {
            pout <- pout +
              geom_text(
                data = subset(lmAnnot, labels %in% "."),
                mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, label = labels, colour = ExpType),
                vjust = -0.25,
                size = 10
              )
          }
        } else {
          pout <- pout +
            geom_errorbarh(
              data = lmAnnot,
              mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, colour = ExpType),
              height = 0
            ) +
            geom_text(
              data = lmAnnot,
              mapping = aes(x = x, y = y, xmin = xmin, xmax = xmax, label = labels, colour = ExpType),
              vjust = -0.25,
              size = 5
            )
        }
        # scaleFCy <- range(scaleFCy, lmAnnot[, "y"], na.rm = TRUE)
      }
    }
    pout <- pout +
      scale_x_discrete(labels = function(value) {
        gsub(
          ",", ".",
          gsub("+", " + ",
            gsub(
              "mM Glc", "mM\nGlc",
              gsub("_", " ", value)
            ),
            fixed = TRUE
          )
        )
      }) +
      labs(x = "Stimulus", y = "Fold Change") +
      theme(
        axis.text = element_text(colour = ifelse(myPalette()[1] == "dodgerblue", "dodgerblue", "black"), face = "bold"),
        legend.title = element_text(colour = myPalette()[3 - tab]),
        legend.position = "top",
        legend.justification = c(0.5, 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold")
      ) +
      guides(
        fill = guide_legend(nrow = ceiling(length(unique(foldChangedtaplot[, "ExpType"])) / 3)),
        colour = guide_legend(nrow = ceiling(length(unique(foldChangedtaplot[, "ExpType"])) / 3))
      )
    if (!legendOn) {
      pout <- pout + theme(legend.position = "none")
    }
    return(pout)
  })
  output$NTPAnalysis_plot3 <- renderPlot({
    NTPAnalysis_plot3()
  })
  output$NTPAnalysis_plot3download <- downloadHandler(
    filename = function() {
      if (length(input$NTPselectExperiment) != 0) {
        index <- getindex()
        if (length(index) == 1) {
          return(paste0("FoldChange_", index, ".", input$plotformat))
        } else {
          return(paste0("FoldChange_Combine.", input$plotformat))
        }
      } else {
        return(NULL)
      }
    },
    content = function(file) {
      ggsave(
        file = file,
        plot = NTPAnalysis_plot3() +
          theme(legend.title = element_blank()),
        width = input$plotwidth,
        height = input$plotheight,
        units = input$plotunits,
        dpi = as.numeric(input$plotdpi)
      )
    }
  )

  output$filelist <- renderUI({
    input$delete
    trash <- formatdata()
    validate(need(file.exists("www/importedfilesinfo.Rdata"), "No data available!"))
    trash <- computeQC()
    load(file = "www/importedfilesinfo.Rdata")
    return(selectInput(
      inputId = "filetodelete",
      label = NULL,
      choices = importedfilesinfo[, "Name"],
      selected = NULL,
      width = "100%",
      selectize = FALSE,
      multiple = FALSE
    ))
  })
  observeEvent(input$delete, {
    trash <- formatdata()
    file.remove(list.files(path = "www/EXCEL", pattern = input$filetodelete, full.names = TRUE))
    file.remove(c("www/importedfilesinfo.Rdata", "www/EndoC_Beta.Rdata"))
    file.remove(list.files(path = "www/CT", full.names = TRUE))
    trash <- formatdata()
  })
}

shinyApp(ui, server)
