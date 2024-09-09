# Max upload size in megabytes
max_upload_size_mb = 200

# Parameter IDs to search for in CSV log to extract required data
pids = list(
  gear = c(4120, 14100),
  rpm = c(12, 2135),
  speed = c(13),
  pedal = c(2114, 2115, 2116, 2117, 73, 74),
  load = c(4)
)

################################################################################
# Do NOT edit anything below this!

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
options(shiny.maxRequestSize = max_upload_size_mb * 1024^2)
load('ideal.RData')
source('www/dll.R')
source('www/viz.R')

################################################################################
## SHINY UI
################################################################################
shinyApp(
  ui = dashboardPage(
    skin = 'red',
    header = dashboardHeader(title="Driver Demand Editor"),
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem("Disclaimer", tabName = "Disclaimer", icon = icon("warning-sign", lib = "glyphicon")),
        menuItem("Inputs", tabName = "Inputs", icon = icon("open", lib = "glyphicon")),
        menuItem("Load vs Pedal", tabName = "Load-Pedal", icon = icon("chart-line")),
        menuItem("Results", tabName = "Results", icon = icon("equalizer", lib="glyphicon")),
        menuItem("Documentation", tabName = "Documentation", icon = icon("book"))
      )
    ),
    body = dashboardBody(
      tags$head(tags$style(HTML(".main-sidebar { font-size: 18px; }"))),
      tabItems(
        tabItem(tabName = "Disclaimer",
                markdown('### >>> *This free tool is for educational purposes only.* <<<'),
                markdown('#### By proceeding, you accept all responsibility for how you utilize it.'),
                markdown('#### The app developer is not liable for any potential loss or damage resulting from its use or associated data logging.')
        ),
        tabItem(tabName = "Inputs",
                box(
                  fluidRow(
                    column(5,
                           fluidRow(
                             column(7, fileInput('log_in', 'HP Tuners CSV logs (NOT interpolated)', accept='.csv', multiple=TRUE)),
                             column(5, radioButtons('log_units', 'Log Speed Units',
                                                    choices=c('KPH', 'MPH'), selected='MPH'))
                           ),
                           fluidRow(
                             column(12, markdown('*Max upload size is **200 MB**. Be sure to UN-CHECK the "Interpolate Data Gaps" setting when exporting from VCM Scanner.*')),
                             column(12, markdown('*If you still have problems, delete some unnecessary columns from the file to reduce its size. Required columns are listed in the Documentation tab.*'))
                           )
                    ),
                    column(7,
                           fluidRow(
                             column(9, textAreaInput('dd_in', 'Paste Driver Demand table *with axes* here', width='100%', rows=7)),
                             column(3, radioButtons('dd_units', 'DD Speed Units',
                                           choices=c('KPH', 'MPH'), selected='KPH'))
                           )
                    ),
                  ),
                  width=12
                ),
                fluidRow(
                  column(6,
                         fluidRow(
                           textOutput('data_checks'),
                           column(6, DTOutput('freq_table')),
                           column(6, hr(), uiOutput('freq_msg'))
                         ),
                         hr(),
                         plotOutput('gear_by_cell', height='600px')
                  ),
                  column(6, plotOutput('rpm_by_gear', height='1100px'))
                )
        ),
        tabItem(tabName = "Load-Pedal",
                fluidRow(
                  column(3, h3("Select target profile:"),
                         radioButtons('profile', 'Pedal profile',
                                      choices = c('Extreme'='5',
                                                  'Peppy'='4',
                                                  'Default'='3',
                                                  'Progressive'='2',
                                                  'Linear'='1',
                                                  'Custom'='Custom'),
                                      selected = '3')),
                  column(6, plotOutput('profile_plot', height='400px', width='400px'))
                ),
                hr(),
                conditionalPanel("input.profile == 'Custom'",
                                 fluidRow(
                                   column(2, selectInput('custom_autofill', 'Auto-fill',
                                                         choices = c('Extreme'='5',
                                                                     'Peppy'='4',
                                                                     'Default'='3',
                                                                     'Progressive'='2',
                                                                     'Linear'='1'),
                                                         selected = '3')),
                                   column(6, uiOutput('custom_input')),
                                   column(2, textOutput('custom_msg')),
                                   column(2, actionButton('apply_custom', 'Apply Changes',
                                                          style="background-color: DodgerBlue; color: White; border-color: Black"))
                                 )
                ),
                markdown('*Utilize your web browser\'s zoom functionality if this plot is too large or small for your screen.*'),
                fluidRow(plotOutput('load_by_speed', height='1000px', width='1300px'))
        ),
        tabItem(tabName = 'Results',
                fluidRow(
                  column(6, sliderInput('decel_mult', 'Deceleration Multiplier',
                                        min=1, max=1.4, value=1, step=0.05)),
                  column(6, uiOutput('max_speed'))
                ),
                markdown('*This app will NOT increase your maximum torque value.*'),
                DTOutput('table_out'),
                markdown('#### <span style="color:blue">Click the \'CSV\' button directly above this to download results. Then paste into HP Tuners and fine-tune based on 1-D profile views.</span>'),
                markdown(' '),
                markdown('### 1-dimensional profile views'),
                fluidRow(
                  column(6, uiOutput('speed_slider')),
                  column(6, uiOutput('pedal_slider'))
                ),
                fluidRow(
                  column(6, plotOutput('speed_1d')),
                  column(6, plotOutput('pedal_1d'))
                )
        ),
        tabItem(tabName = "Documentation",
                markdown('### Overview'),
                markdown('#### Driver Demand Editor is applicable to Gen 5 GM vehicles that use a two-dimensional table for drive-by-wire throttle mapping with these characteristics:'),
                markdown('- rows indexed by accelerator pedal percent'),
                markdown('- columns indexed by vehicle speed -OR- engine RPM'),
                markdown('- cell values representing torque requested'),
                fluidRow(
                  column(2, markdown('#### Example:')),
                  column(10, tags$img(src = "dd.png", width="600"))
                ),
                markdown('#### This app generates Engine Load vs. Pedal Position curves from the logged data for each DD table column, and automatically calculates corrections by comparing these curves against a user-selectable ideal curve. Since this relies on load calculations, you should first make sure your MAF, VE, and torque model are in good shape.'),
                markdown('### Data Logging'),
                markdown('#### 1. Try to populate as much of the DD pedal-speed matrix as possible by varying not only pedal position and vehicle speed, but also transmission gear and engine RPM.'),
                markdown('#### 2. Ideally, try to limit the drive to mostly flat roads (driving uphill skews engine load high, and driving downhill skews engine load low).'),
                markdown('#### 3. This app automatically filters the log to only include observations with increasing accelerator pedal position, so cruising at steady state for extended periods is not beneficial.'),
                markdown("#### After logging, from within VCM Scanner click 'Log File' > 'Export Log File', then export entire log as CSV, and **disable interpolation of data gaps**. This is the file format you must upload to this app."),
                markdown('### Required Parameters'),
                markdown('#### The log file MUST include these five parameters, plus the \'Offset\' column that VCM Scanner automatically records:'),
                markdown('- Vehicle Speed [13]'),
                markdown('- Engine RPM [12 or 2135]'),
                markdown('- Calculated Engine Load [4]'),
                markdown('- Accelerator Pedal Position [2114]  *(but will also accept 2115, 2116, 2117, 73, or 74)*'),
                markdown('- Transmission Current Gear [4120 or 14100]  *(but will be heuristically deduced from Speed/RPM ratio if missing)*'),
                markdown('### Video Walk-Through'),
                markdown('#### https://youtu.be/GWhjPFLw89Y'),
                markdown('### Discussion thread for Q&A, Bug Reports, and Feature Requests'),
                markdown('#### https://forum.hptuners.com/showthread.php?107808-Driver-Demand-Editor-new-tool-for-tuning-DBW-throttle-mapping'),
                markdown('#### *App last updated 27-Aug-2024*')
        )
      )
    )
  ),

################################################################################
## SHINY SERVER
################################################################################
  server = function(input, output, session) {
    # closing window also terminates app gracefully
    session$onSessionEnded(function(){stopApp()})
    
    # load dd table
    dd_df <- reactive({
      validate(need(input$dd_in!="", 'Paste DD table with axes'))
      dd_df = read.delim(text=input$dd_in, header=FALSE)
      if (dd_df[nrow(dd_df),1] == '%') dd_df = dd_df[1:(nrow(dd_df)-1),]
      if (is.na(dd_df[1,ncol(dd_df)])) dd_df = dd_df[,1:(ncol(dd_df)-1)]
      validate(need(!any(is.na(dd_df)), 'ERROR: missing values detected. Copy DD table from your tune - NOT your log'))
      validate(need(min(dim(dd_df)) >= 14, 'DD table read error'))
      return(dd_df)
    })
    pedal_bins <- reactive({as.numeric(dd_df()[-1,1])})
    n_pedal <- reactive({length(pedal_bins())})
    speed_bins <- reactive({as.numeric(dd_df()[1,-1])})
    n_speed <- reactive({length(speed_bins())})
    pedal_breaks <- reactive({c(0, pedal_bins()[1:n_pedal()-1] + diff(pedal_bins())/2, pedal_bins()[n_pedal()])})
    speed_breaks <- reactive({c(0, speed_bins()[1:n_speed()-1] + diff(speed_bins())/2, speed_bins()[n_speed()])})
    
    # determine whether columns represent vehicle speed or engine speed
    speed_toggle <- reactive({
      max_speed = max(speed_bins())
      speed_toggle = 'vehicle'
      if (max_speed > 1000) speed_toggle = 'engine'
      return(speed_toggle)
    })
    dd_units <- reactive({
      dd_units = input$dd_units
      if (speed_toggle() == 'engine') dd_units = 'RPM'
      return(dd_units)
    })
    
    # create dd_mat object
    dd_mat <- reactive({
      dd_mat = as.matrix(dd_df()[2:(n_pedal()+1), 2:(n_speed()+1)])
      rownames(dd_mat) = paste0('p_', pedal_bins())
      colnames(dd_mat) = paste0('s_', speed_bins())
      return(dd_mat)
    })
    
    # load data log(s)
    num_files <- reactive({
      validate(need(!is.null(input$log_in), 'Upload data log'))
      return(nrow(input$log_in))
    })
    log_list <- reactive({
      validate(need(num_files() > 0, 'Upload data log'))
      log_list = list()
      for (i in 1:num_files()) {
        df = read.csv(input$log_in[[i,'datapath']], header=FALSE, skip=14, quote="")
        log_list[[i]] = df[-(2:5),]
      }
      return(log_list)
    })
    
    log_df_0 <- reactive({
      # identify column indices we need
      pid_indices = vector('numeric', length(pids))
      for (i in 1:length(pids)) {
        pid_indices[i] = na.omit(match(pids[[i]], log_list()[[1]][1,]))[1]
        if (i > 1) {
          validate(need(!is.na(pid_indices[i]),
                        paste0('ERROR: no ', names(pids)[i], ' pid detected [', pids[i], ']')))
        }
      }
      ii = which(!is.na(pid_indices))
      
      # extract only columns we need, and rename them for simplicity
      data_list = list()
      num_logs = length(log_list())
      for (i in 1:num_logs) {
        data_list[[i]] = log_list()[[i]][-1, c(1, pid_indices[ii])]
        colnames(data_list[[i]]) = c('time', names(pids[ii]))
        if (num_logs > 1) {
          data_list[[i]]$time = as.numeric(data_list[[i]]$time)
          if (i > 1) {
            data_list[[i]]$time = data_list[[i]]$time + max(data_list[[i-1]]$time, na.rm=TRUE)
          }
        }
      }
      df = do.call('rbind', data_list)
      
      # convert columns from strings to numbers
      df = suppressWarnings(as.data.frame(lapply(df, as.numeric)))
      
      # drop rows in which our columns of interest are all blank
      if (length(ii) == length(pids)) {
        df = df[!(is.na(df$gear) & is.na(df$rpm) & is.na(df$speed) & is.na(df$pedal) & is.na(df$load)),]
      } else {
        df = df[!(is.na(df$rpm) & is.na(df$speed) & is.na(df$pedal) & is.na(df$load)),]
      }
    })
    
    # determine logged frequency of pedal and load
    freqs <- reactive({calc_freqs(log_df_0()[,c('time','pedal','load')])})
    output$freq_table <- renderDT(datatable(freqs(),
                                            options = list(dom='tB', ordering=FALSE, pageLength=2, buttons=FALSE)) %>%
                                            formatRound(columns=1, digits=1))
    output$freq_msg <- renderUI({
      if (anyNA(freqs())) {
        markdown("*Unable to determine logged frequency because data gaps have been interpolated.*")
      } else {
        markdown("*To increase logged frequency, adjust the Polling Interval in VCM Scanner, and/or reduce the total number of logged parameters.*")
      }
    })
    
    output$data_checks <- renderPrint({
      msg = ''
      max_load = max(log_df_0()$load, na.rm=TRUE)
      if ((max_load > 1.0) & (max_load < 50)) {
        msg = paste0(msg, 'WARNING: Load never exceeds 50; data collection might be insufficient. ')
      }
      
      min_pedal = min(log_df_0()$pedal, na.rm=TRUE)
      if (min_pedal > 6) msg = paste0(msg, 'WARNING: min(Pedal) > 0; values will be re-scaled. ')
      
      if (!('gear' %in% colnames(log_df_0()))) msg = paste0(msg, 'WARNING: Trans Gear data not detected; values will be heuristically deduced. ')
      
      if (msg == '') msg = 'Supplementary data checks passed.'
      
      return(noquote(msg))
    })
    
    log_df <- reactive({
      validate(need(min(log_df_0()$load, na.rm=TRUE) >= 0,
                    'ERROR: Negative load values detected. Check accuracy of CSV export.'))
      validate(need(max(log_df_0()$load, na.rm=TRUE) <= 100,
                    'ERROR: Load values exceed 100. Check accuracy of CSV export.'))
      validate(need(min(log_df_0()$pedal, na.rm=TRUE) >= 0,
                    'ERROR: Negative pedal values detected. Check accuracy of CSV export.'))
      validate(need(max(log_df_0()$pedal, na.rm=TRUE) <= 100,
                    'ERROR: Pedal values exceed 100. Check accuracy of CSV export.'))
      
      # interpolate gaps, scale values, and filter data
      df = interp_scale_filter(log_df_0(), input$log_units)
      validate(need(nrow(df) > 0,
                    'ERROR: Zero rows extracted. Most likely cause is faulty pedal data.'))
      
      # create pedal & speed bins and gear weights
      df = calc_bins_and_weights(df, input$log_units, dd_units(), pedal_breaks(), speed_bins())
    })
    
    # number of gears
    n_gear <- reactive({max(log_df()$gear)})
    
    # calculate avg gear and load by DD cell
    avg_by_dd_cell <- reactive({calc_avgs_by_dd_cell(log_df(), n_pedal(), speed_bins(), dd_units())})
    
    # plot RPM vs. Pedal by gear
    output$rpm_by_gear <- renderPlot({plot_rpm_vs_pedal(log_df()[,c('gear','pedal','rpm')])})
    
    # plot Avg. Gear by DD Cell
    output$gear_by_cell <- renderPlot({
      plot_avg_gear_by_cell(avg_by_dd_cell()$gear, n_gear(), speed_bins(), pedal_bins(), dd_units())
    })
    
    # create target profile dataframe and vector, scaled to minimum logged load
    profile_df <- reactive({
      df = ideal[which(ideal$x %in% pedal_bins()),]
      min_load = min(avg_by_dd_cell()$load, na.rm=TRUE)
      df[,1:5] = df[,1:5]*(100-min_load)/100 + min_load
      df[,1:5] = round(df[,1:5], 2)
      return(df)
    })
    
    profile_vec <- reactive({
      if (input$profile == 'Custom') {
        if (input$apply_custom > 0) {
          profile_vec = custom_profile()
        } else {
          profile_vec = 0
        }
      } else {
        i = as.integer(input$profile)
        profile_vec = profile_df()[,i]
      }
    })
    
    # process custom profile input
    output$custom_input <- renderUI({
      i = as.integer(input$custom_autofill)
      textAreaInput('custom_input', '1) Select auto-fill.  2) Edit values.  3) Click \'Apply Changes\' if checks pass.',
                    value=paste(round(profile_df()[,i],1), collapse="  "), width='100%', rows=1)
    })
    custom_realtime <- reactive({
      as.numeric(read.delim(text=gsub(',','',input$custom_input), sep='', header=FALSE))
    })
    output$custom_msg <- renderPrint({
      if (is.null(custom_realtime())) {
        noquote('ERROR: no input detected')
      } else {
        len = length(custom_realtime())
        if (len != n_pedal()) {
          noquote(paste('ERROR:',len,'values detected but', n_pedal(), 'required'))
        } else if (!min(custom_realtime() == sort(custom_realtime()))) {
          noquote('ERROR: values must be increasing')
        } else {
          noquote('Checks passed; proceed')
        }
      }
    })
    custom_profile <- eventReactive(input$apply_custom, {custom_realtime()}, ignoreNULL=FALSE)
    
    # create target profile matrix
    target_mat <- reactive({
      target_mat = matrix(data=rep(profile_vec(), times=n_speed()), nrow=n_pedal(), ncol=n_speed())
      if (dd_units() == 'RPM') {
        ii = which(speed_bins() <= 1500)
        target_mat[,ii] = target_mat[,ii]*profile_df()$mult
        ii = which(speed_bins() <= 1000)
        target_mat[,ii] = target_mat[,ii]*profile_df()$mult
      } else {
        target_mat[,1] = target_mat[,1]*profile_df()$mult^2
        target_mat[,2] = target_mat[,2]*profile_df()$mult
      }
      target_mat[target_mat > 100] = 100
      target_mat = round(target_mat, 2)
      rownames(target_mat) = paste0('p_', pedal_bins())
      colnames(target_mat) = paste0('s_', speed_bins())
      return(target_mat)
    })
    
    # plot target profiles
    output$profile_plot <- renderPlot({profile_plot(pedal_bins(), profile_df(), profile_vec())})
    
    # plot Load vs. Pedal by Speed
    output$load_by_speed <- renderPlot({
      # plot_load_by_speed(log_df(), avg_by_dd_cell()$load, target_mat(), speed_breaks(),
      #                    speed_bins(), pedal_bins(), n_gear(), dd_units())
      plot_load_by_speed(log_df(), load_mod(), target_mat(), speed_breaks(),
                         speed_bins(), pedal_bins(), n_gear(), dd_units())
    })
    
    # normalize logged load
    load_mod <- reactive({normalize_load(avg_by_dd_cell()$load, n_speed(), n_pedal())})
    
    # calculate new raw DD table
    dd_out <- reactive({calc_new_dd(dd_mat(), target_mat(), load_mod())})
    
    # smooth new DD table
    dd_out_smooth <- reactive({smooth_dd(dd_out())})
    
    # create max speed dropdown
    output$max_speed <- renderUI({
      selectInput('max_speed', 'Max speed to copy to the right',
                  choices=speed_bins(), selected=speed_bins()[n_speed()])
    })
    
    # scale negatives and round to one decimal point
    dd_out_final <- reactive({
      validate(need(!is.null(input$max_speed), 'Identifying speed bins'))
      finalize_dd(dd_out_smooth(), input$decel_mult, input$max_speed, pedal_bins(), speed_bins())
    })
    
    # calculate range of output DD table
    range_out <- reactive({range(dd_out_final())})
    
    # pedal and speed bin slider inputs
    output$pedal_slider <- renderUI({
      sliderInput('pedal_slider', 'Pedal bin index (rows)', min=1, max=n_pedal(), value=1, step=1)
    })
    output$speed_slider <- renderUI({
      sliderInput('speed_slider', 'Speed bin index (columns)', min=1, max=n_speed(), value=1, step=1)
    })
    
    # one-dimensional plots by pedal and speed
    output$pedal_1d <- renderPlot({
      plot_pedal_1d(dd_out_final(), input$pedal_slider,
                    pedal_bins(), speed_bins(), dd_units())
    })
    output$speed_1d <- renderPlot({
      plot_speed_1d(dd_out_final(), input$speed_slider,
                    speed_bins(), pedal_bins(), dd_units())
    })
    
    output$table_out = renderDT(
      datatable(dd_out_final(), extensions = 'Buttons',
                options = list(dom='tB', ordering=FALSE, pageLength=n_pedal(), buttons='csv')) %>%
        formatRound(columns=1:n_speed(), digits=1) %>%
        formatStyle(columns=1:n_speed(),
                    backgroundColor=styleInterval(seq(range_out()[1], range_out()[2], length.out=110),
                                                  colorRampPalette(c('red4','red','orange','yellow2','white',
                                                                     'green2','turquoise2','dodgerblue',
                                                                     'blue','purple2','purple2','magenta2'))(111)))
    )
    
    # Channels.xml file exporter
    output$channels <- downloadHandler(filename = "DriverDemand.Channels.xml",
                                       content = function(file){write.table(channels, file, quote=FALSE,
                                                                            row.names=FALSE, col.names=FALSE)}
    )
  }
)
