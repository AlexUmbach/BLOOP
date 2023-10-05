########################## SERVER LOGIC ############################
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)
  
  ## These generate NULL data to prevent the loading swirl from starting before press "start" on the analyses
  output$read_table_out <- renderDT(NULL)
  output$read_plot <- renderDT(NULL)
  output$b1_table_out <- renderDT(NULL)
  output$bubble_out <- renderDT(NULL)
  output$taxa_bar <- renderDT(NULL)
  output$bar_table_out <- renderDT(NULL)
  output$pcoa_plot_out <- renderDT(NULL)
  output$pcoa_table_out <- renderDT(NULL)
  output$ranked_table_out <- renderDT(NULL)
  output$ranked_plot_out <- renderDT(NULL)
  output$uni_pcoa_plot_out <- renderDT(NULL)
  
  
  # # Start buttons
  # read_start_pressed <- reactiveVal(FALSE)

  
  # ## These monitor the status of the various start buttons on each tab
  # observe({
  # bar_start_pressed <- reactiveVal(FALSE)
  # bubble_start_pressed <- reactiveVal(FALSE)
  # pcoa_start_pressed <- reactiveVal(FALSE)
  # ranked_start_pressed <- reactiveVal(FALSE)
  # })
  
  ## These pass on the metadata column names to all of the drop down menus
  observe({
    req(input$meta_file)
    
    ## Incorporate the meta file column names into every appropriate input
    meta_datafile <- meta_datafile()
    meta_colnames <- c(colnames(meta_datafile), "TaxaName")
    meta_colnames <- meta_colnames[meta_colnames != "SampleName"]
    
    ## Update the selections - read plot
    updateSelectInput(session, "read_sortby_axis", choices = meta_colnames)
    updateSelectInput(session, "read_meta_group", choices = meta_colnames)
    # updateSelectInput(session,"read_meta_key",choices = read_meta_list)
    updateSliderInput(session, "read_plot_out_w")
    # updateSelectInput(session,"read_colour",choices = colnames(meta_datafile()))
    updateSelectInput(session, "read_sortby_axis", choices = meta_colnames)
    updateSelectInput(session, "read_meta_group", choices = meta_colnames)
    updateSliderInput(session, "read_plot_out_w")

    ## Update selection - Bar plot
    updateSelectInput(session, "bar_sortby_xaxis", choices = meta_colnames)
    updateTextInput(session, "bar_plotheight")
    updateTextInput(session, "bar_plotwidth")

    ## Update selection - Bubble plot
    updateSelectInput(session, "b1_sort_param", choices = meta_colnames)
    updateSelectInput(session, "b1_color_param", choices = meta_colnames)
    updateSelectInput(session, "b1_meta_group", choices = meta_colnames)
    updateTextInput(session, "b1_bubble_width")
    updateTextInput(session, "b1_bubble_height")
    updateTabItems(session, "b1_meta_keyword")
    updateSelectInput(session, "b1_second_facet_meta", choices = meta_colnames)
    updateSelectInput(session, "b1_third_facet_meta", choices = meta_colnames)
    updateSelectInput(session, "b1_sort_axis", choices = meta_colnames)
    

    ## Update selection - Bray-Curtis PCoA plot
    updateSelectInput(session, "pcoa_fill_col", choices = meta_colnames)
    updateSelectInput(session, "pcoa_elips_col", choices = meta_colnames)
    updateSelectInput(session, "pcoa_shape", choices = meta_colnames)
    
    ## Update selection - Ranked
    updateSelectInput(session, "ranked_sort_param", choices = meta_colnames)
    updateSelectInput(session, "ranked_color_param", choices = meta_colnames)
    updateSelectInput(session, "ranked_meta_group", choices = meta_colnames)
    updateTextInput(session, "ranked_bubble_width")
    updateTextInput(session, "ranked_bubble_height")
    updateTabItems(session, "ranked_meta_keyword")
    updateSelectInput(session, "ranked_second_facet_meta", choices = meta_colnames)
    updateSelectInput(session, "ranked_sort_axis", choices = meta_colnames)
    updateCheckboxInput(session, "pcoa_sample_labels")
    
    
  })
  
  ## Update selection - UniFrac PCoA plot
  observe({
    req(input$uni_meta_file)
    
    meta_datafile <- uni_meta_datafile_og()
    meta_colnames <- c(colnames(meta_datafile), "TaxaName")
    meta_colnames <- meta_colnames[meta_colnames != "SampleName"]
    
    updateSelectInput(session, "uni_pcoa_fill_col", choices = meta_colnames)
    updateSelectInput(session, "uni_pcoa_elips_col", choices = meta_colnames)
    updateSelectInput(session, "uni_pcoa_shape", choices = meta_colnames)
  })
  
  
  
  ####  Upload tab ####
  {
    ## Main ASV table
    output$main_table <- renderDataTable({
      req(input$main_file)
      read.table(
        file = input$main_file$datapath,
        fill = TRUE,
        header = TRUE,
        sep = "\t"
      )
    })
    
    main_datafile_upload <- reactive({
      req(input$main_file)
      read.table(
        file = input$main_file$datapath,
        fill = TRUE,
        header = TRUE,
        sep = "\t"
      )
    })
    
    main_datafile_og <- reactive({
      main_datafile = main_datafile_upload()
      main_datafile[is.na(main_datafile)] <- 0
      main_datafile
    })
    
    ## Metadata table
    meta_datafile_og <- reactive({
      req(input$meta_file)
      read.table(
        file = input$meta_file$datapath,
        fill = TRUE,
        header = TRUE,
        sep = "\t"
      )
    })
    
    output$meta_table <- renderDataTable({
      req(input$meta_file)
      read.table(
        file = input$meta_file$datapath,
        fill = TRUE,
        header = TRUE,
        sep = "\t"
      )
    })
  
    
    ## Filter the ASV table based on the present samples in the metadata table
    main_datafile <- reactive({
      req(input$main_file)
      main_data_table <- main_datafile_og()
      meta_data_table <- meta_datafile()
      meta_names <-
        c(
          meta_data_table$SampleName,
          "Consensus.Lineage",
          "rowID",
          "Feature.ID",
          "ReprSequence"
        )
      main_data_table <-
        main_data_table[, names(main_data_table) %in% meta_names]
      main_data_table
    })
    
    ## Filter the metadata table based on present samples in ASV table
    meta_datafile <- reactive({
      meta_data_table <- meta_datafile_og()
      main_data_table <- main_datafile_og()
      meta_names <-
        c(
          meta_data_table$SampleName,
          "Consensus.Lineage",
          "rowID",
          "Feature.ID",
          "ReprSequence"
        )
      main_data_table <-
        main_data_table[, names(main_data_table) %in% meta_names]
      meta_data_table <-
        meta_data_table %>% filter(SampleName %in% colnames(main_data_table))
      # meta_data_table$TaxaName <- 1:nrow(meta_data_table)
      meta_data_table <-
        replace(meta_data_table, is.na(meta_data_table), "NA")
      meta_data_table
    })
    
    
    ## ASV contaminant table
    output$contam_table <- renderDataTable({
      req(input$contam_file)
      read.table(
        file = input$contam_file$datapath,
        fill = TRUE,
        header = TRUE,
        sep = "\t"
      )
    })
    
    contam_datafile <- reactive({
      req(input$contam_file)
      read.table(
        file = input$contam_file$datapath,
        fill = TRUE,
        header = TRUE,
        sep = "\t"
      )
    })
    
  }
  
  ## Show the original table that was uploaded
  output$proc_main <- renderDataTable({
    main_datafile <- main_datafile()
    output$proc_maintext <- renderText("This is your original data")
    
    # req(input$main_file)
    main_datafile
  })
  
  
  #### Main table (Processing tab) #### I SHOULD REMOVE THIS FOR SIMPLICITY
  
  data_tran_react <- reactive({
    req(input$main_file)
    data_tran <- main_datafile()
    
    ## Converting collapsed tables to ASV tables, and standardizing formatting, including adding rowID columns
    if (input$is_main_collapsed == TRUE) {
      data_tran$Consensus.Lineage <- data_tran$Feature.ID
      # data_tran <- within(data_tran, rm("Feature.ID"))
      data_tran$Feature.ID <- 1:nrow(data_tran)
      data_tran$rowID <- 1:nrow(data_tran)
    } else {
      if (input$is_main_collapsed == FALSE) {
        if ("rowID" %in% colnames(data_tran)) {
          data_tran
        } else {
          data_tran$rowID <- 1:nrow(data_tran)
        }
      }
    }
    data_tran
  })
  
  
  
  #### Transformed tables (processing tab) ####
  ## This needs to be changed to be only activated once, then again by any subsequent
  
  data_tran_contam_filt_react <- reactive({
    #data_tran = data_tran_react()
    
    # if(input$is_main_collapsed == "No"){
    if (input$contam_filter == "Remove") {
      contam_datafile <- contam_datafile()
      data_tran <- data_tran_react()
      rownames(data_tran) <- data_tran$Feature.ID
      # data_tran <- data_tran[ ! data_tran$Feature.ID %in% contam_datafile$Feature.ID,]
      data_tran[contam_datafile$Feature.ID, !names(data_tran) %in% c("Consensus.Lineage",
                                                                     "Feature.ID",
                                                                     "ReprSequence",
                                                                     "rowID")] = 0
      #rownames(data_tran) <- 1:nrow(data_tran)
    }
    
    ## There is an issue here I haven't sorted out
    if (input$contam_filter == "Analyze") {
      contam_datafile <- contam_datafile()
      data_tran <- data_tran_react()
      data_tran <- data_tran[data_tran$Feature.ID %in% contam_datafile$Feature.ID, ]
    }
    
    if (input$contam_filter == "No action") {
      data_tran <- data_tran_react()
    }
    # } else {
    #   data_tran
    
    data_tran
  })
  
  output$proc_new_data <- renderDataTable({
    data_tran <- data_tran_contam_filt_react()
    output$proc_new_text <- renderText("This is your new dataframe")
    data_tran
  })
  
  
  
  data_long_react <- reactive({
    data_tran <- data_tran_contam_filt_react()
    
    ## Need to set the labels here first, not elsewhere (like in other reactive elements)
    if (input$is_main_collapsed == TRUE) {
      labels <- data_tran$Consensus.Lineage
      labels <- paste(labels, data_tran$rowID, sep = "_")
      data_tran$Consensus.Lineage <-
        gsub(" ", "", data_tran$Consensus.Lineage)
    } else {
      labels <- data_tran$Consensus.Lineage
      labels <- paste(labels, data_tran$rowID, sep = "_")
      data_tran$Consensus.Lineage <-
        gsub(" ", "", data_tran$Consensus.Lineage)
    }
    
    ## Cleans up labels and removes uncultured/ambiguous taxa
    labels <- gsub("_[0-9]*$", "", labels)
    labels <- gsub(" ", "", labels)
    labels <- gsub("(;Ambiguous__taxa)", ";s__Ambiguous_taxa", labels)
    labels <- gsub("(;Ambiguous_taxa)", ";s__Ambiguous_taxa", labels)
    
    if (input$truncate_taxa == "Yes") {
      labels <- paste(";", sep = "", labels)
      labels <- gsub("(;\\s*Ambiguous_taxa)", "", labels)
      labels <- gsub("(uncultured.*)", "", labels)
      labels <- gsub("(__uncultured.*)", "", labels)
      labels <- gsub("(unidenti.*)", "", labels)
      labels <- gsub("(__unidenti.*)", "", labels)
      labels <- gsub("(;.__Ambiguous_taxa)", "", labels)
      labels <- gsub("(;._Ambiguous_taxa)", "", labels)
      labels <- gsub("(;s__$)", "", labels)
      labels <- gsub("(;g__$)", "", labels)
    }
    
    ## Remove taxa pre-fixes associated with SILVA classifier (old):
    if (input$remove_prefix == "Yes") {
      labels <- gsub("(D_.__)", "", labels)
      labels <- gsub(";$", "", labels)
    }
    labels <- gsub("(D_.__$)", "", labels)
    
    ## Remove taxa-prefixed associated with SILVA classifier (new):
    if (input$remove_prefix == "Yes") {
      labels <- gsub("(d__)", "", labels)
      labels <- gsub("(p__)", "", labels)
      labels <- gsub("(c__)", "", labels)
      labels <- gsub("(o__)", "", labels)
      labels <- gsub("(f__)", "", labels)
      labels <- gsub("(g__)", "", labels)
      labels <- gsub("(s__)", "", labels)
      labels <- gsub(" ", "", labels)
      labels <- gsub("(;metagenome$)", "", labels)
      labels <- gsub("(__.$)", "", labels)
      labels <- gsub("(;__)", "", labels)
      labels <- gsub("(;$)", "", labels)
      labels <-
        gsub("(;$)", "", labels) #This is not a duplicate -- leave it here
    }
    
    ## S## Retrieves the last taxonomy entry (i.e., genus, species):
    full_lineage <-
      as.data.frame(paste(data_tran$Consensus.Lineage, data_tran$rowID, sep = "_"))
    lineage_OTU <-
      as.data.frame(paste(gsub(".*;", "", labels), data_tran$rowID, sep = "_"))
    colnames(lineage_OTU) <- "TaxaName"
    colnames(full_lineage) <- "TaxaName"
    rownames(data_tran) <- full_lineage$TaxaName
    #print(lineage_OTU)
    
    ## Filter the table so that it will only have numerical data
    cols_to_filter <-
      c("OTU.ID",
        "Consensus.Lineage",
        "ReprSequence",
        "rowID",
        "Feature.ID")
    filtered_table <-
      data_tran[,!(names(data_tran) %in% cols_to_filter)]
    
    # Replace any NAs with zeros
    filtered_table[is.na(filtered_table)] <- 0
    
    ## Convert table into % abundance
    data_prop <- filtered_table / colSums(filtered_table)
    # data_prop <- prop.table(as.matrix(filtered_table), 2) * 100
    
    ## WORKING ##
    # data_prop <- prop.table(as.matrix(filtered_table), 2) * 100
    # data_prop_mat <- data_prop
    # data_prop <- as.data.frame(data_prop)
    
    ## Add full taxonomy into prop table and reassign row names to their truncated names:
    data_prop <- as.data.frame(data_prop)
    data_prop$Taxonomy <- rownames(data_prop)
    rownames(data_prop) <- lineage_OTU$TaxaName
    
    ## Convert to data frame and change row names to ASVs/Taxa:
    data_prop <- as.data.frame(data_prop)
    data_prop_taxa <- data_prop
    
    ## Add TaxaName column for sorting in later graphs. Also formats the Taxonomy names:
    data_prop$TaxaName <- lineage_OTU$TaxaName
    data_prop$Taxonomy <- gsub("(D_.__)", "", data_prop$Taxonomy)
    data_prop$Taxonomy <- gsub(";$", "", data_prop$Taxonomy)
    
    ## Convert to long form data frame
    data_long <-
      reshape2::melt(
        data_prop,
        id.vars = c("TaxaName", "Taxonomy"),
        variable.name = as.character("SampleName"),
        value.name = "Percentage"
      )
    data_long
  })
  
  output$proc_main_alt <- renderDataTable({
    data_long <- data_long_react()
    output$proc_alttext <- renderText("This is your processed data")
    data_long
  })
  
  
  #### New Bubble table generation ####
  data_unfiltered_table <- reactive({
    main_datafile <- main_datafile()
    data_tran <- data_tran_contam_filt_react()
    req(input$main_file)
    req(input$meta_file)
    
    if (input$is_main_collapsed == TRUE) {
      labels <- data_tran$Consensus.Lineage
      labels <- paste(labels, data_tran$Feature.ID, sep = "_")
    } else {
      labels <- data_tran$Consensus.Lineage
      labels <- paste(labels, data_tran$rowID, sep = "_")
    }
    
    ## Simple taxonomy corrections
    data_tran$Consensus.Lineage <-
      gsub(" ", "", data_tran$Consensus.Lineage)
    
    ## Cleans up labels and removes uncultured/ambiguous taxa
    labels <- gsub("_[0-9]*$", "", labels)
    labels <- gsub(" ", "", labels)
    labels <- gsub("(;Ambiguous__taxa)", ";s__Ambiguous_taxa", labels)
    labels <- gsub("(;Ambiguous_taxa)", ";s__Ambiguous_taxa", labels)
    
    if (input$truncate_taxa == "Yes") {
      labels <- paste(";", sep = "", labels)
      labels <- gsub("(;\\s*Ambiguous_taxa)", "", labels)
      labels <- gsub("(uncultured.*)", "", labels)
      labels <- gsub("(__uncultured.*)", "", labels)
      labels <- gsub("(unidenti.*)", "", labels)
      labels <- gsub("(__unidenti.*)", "", labels)
      labels <- gsub("(;.__Ambiguous_taxa)", "", labels)
      labels <- gsub("(;._Ambiguous_taxa)", "", labels)
      labels <- gsub("(;s__$)", "", labels)
      labels <- gsub("(;g__$)", "", labels)
    }
    
    ## Remove taxa pre-fixes associated with SILVA classifier (old):
    if (input$remove_prefix == "Yes") {
      labels <- gsub("(D_.__)", "", labels)
      labels <- gsub(";$", "", labels)
    }
    labels <- gsub("(D_.__$)", "", labels)
    
    ## Remove taxa-prefixed associated with SILVA classifier (new):
    if (input$remove_prefix == "Yes") {
      labels <- gsub("(d__)", "", labels)
      labels <- gsub("(p__)", "", labels)
      labels <- gsub("(c__)", "", labels)
      labels <- gsub("(o__)", "", labels)
      labels <- gsub("(f__)", "", labels)
      labels <- gsub("(g__)", "", labels)
      labels <- gsub("(s__)", "", labels)
      labels <- gsub(" ", "", labels)
      labels <- gsub("(;metagenome$)", "", labels)
      labels <- gsub("(__.$)", "", labels)
      labels <- gsub("(;__)", "", labels)
      labels <- gsub("(;$)", "", labels)
      labels <-
        gsub("(;$)", "", labels) #This is not a duplicate -- leave it here
    }
    
    ## ## Retrieves the last taxonomy entry (i.e., genus, species):
    full_lineage <-
      as.data.frame(paste(data_tran$Consensus.Lineage, data_tran$rowID, sep = "_"))
    lineage_OTU <-
      as.data.frame(paste(gsub(".*;", "", labels), data_tran$rowID, sep = "_"))
    colnames(lineage_OTU) <- "TaxaName"
    colnames(full_lineage) <- "TaxaName"
    rownames(data_tran) <- full_lineage$TaxaName
    
    ## Keep this unfiltered
    unfiltered_table <- data_tran
    unfiltered_table
  })
  
  
  ## This filters the table to prepare it for prop table functions. This must be a separate reactive so that the reprsequences can be maintained
  data_filtered_table <- reactive({
    unfiltered_table <- data_unfiltered_table()
    cols_to_filter <-
      c("OTU.ID",
        "rowID",
        "Consensus.Lineage",
        "ReprSequence",
        "rowID",
        "Feature.ID")
    filtered_table <-
      unfiltered_table[,!(names(unfiltered_table) %in% cols_to_filter)]
    
    filtered_table
  })
  
  data_filtered_table_abridged_names_re <- reactive({
    unfiltered_table <- data_unfiltered_table()
    
  })
  
  
  
  #### Total read plot ####
  
    # observeEvent
  
  giant_read_reactive <- reactive({
    req(input$read_start)
    # 
    # if (!read_start_pressed()){
    #   return(NULL)
    # }
    
    data_read_table <- reactive({
      filtered_table <- data_filtered_table()
      meta_data_table <- meta_datafile()
      
      data_read <- filtered_table
      data_read <- rbind(data_read, Total = colSums(data_read))
      data_read_total <- data_read["Total", ]
      data_read_total <-
        reshape2::melt(
          data_read_total,
          variable.name = as.character("SampleName"),
          value.name = "Total"
        )
      
      ## Add metadata columns
      data_read_total <-
        left_join(data_read_total,
                  meta_data_table,
                  by = "SampleName",
                  copy = FALSE)
      
      ## NOT WORKING BUT NEEDS TO
      # ## Include only specific sample groups within the plot/;
      # if (is.na(input$read_meta_group) == TRUE){
      #   warning("No metadata category selected. Script will continue without filtering by groups.")
      # } else {
      #   sample_hits <- grepl(pattern = input$read_meta_key, ignore.case = TRUE, x = (eval(parse(text=paste("data_read_total$",input$read_meta_group)))))
      #   data_read_total <- data_read_total[sample_hits,]
      #   warning("Metadata filtering selected.")
      # }
      
      ## Reorder x-axis to follow metadata category in data frame
      data_read_total$SampleName <-
        as.character(data_read_total$SampleName)
      data_read_total$SampleName <-
        factor(data_read_total$SampleName,
               levels = unique(data_read_total$SampleName))
      
      ## Make a list of unique metadata
      read_meta_list <- unique(data_read_total$input$read_meta_group)
      
      read_meta_list
      data_read_total
    })
    
    
    data_read_plot <- reactive({
      data_read_total <- data_read_table()
      
      
      if (input$box_select == "Bar") {
        read_plot <- ggplot(data = data_read_total,
                            aes(
                              x = SampleName,
                              y = Total,
                              width = input$read_width
                            ))
        
        read_plot <-
          read_plot + geom_bar(
            aes(fill = as.factor(eval(
              parse(text = paste(
                "data_read_total$", input$read_sortby_axis
              ))
            ))),
            colour = "black",
            size = input$read_border_bold,
            alpha = input$read_alpha,
            stat = "identity",
            ## this position_dodge preserves the size of the bar, so that you don't have different sized bars for identical sample names. position=position_dodge(preserve = "total")
            ## to make a stacked bar plot, also involved in pie charts
            position = "stack"
            
            
          )
        
        read_plot
        
        
      }
      
      if (input$box_select == "Box") {
        read_plot <- ggplot(data = data_read_total,
                           #aes(x=as.factor(input$read_sortby_axis), y=Total, width = input$read_width))
                           aes(
                             x = as.factor(input$read_sortby_axis),
                             y = Total,
                             width = 2
                           ))
        
        read_plot <- read_plot + geom_boxplot(
          aes(fill = as.factor(eval(
            parse(text = paste(
              "data_read_total$", input$read_sortby_axis
            ))
          ))),
          position = "dodge2",
          alpha = input$read_alpha,
          # size = input$read_width,
          size = input$read_border_bold,
          outlier.shape = NA
        )
        
        read_plot <- read_plot + stat_summary(
          fun = mean,
          geom = "point",
          shape = 15,
          size = 1,
          colour = "red"
        )
        
        read_plot <- read_plot + geom_jitter()
        
        read_plot
        
      }

      ## setting the graph so that it begins at the x-axis and there is no gap. Also sets the limits of the y-axis.
      read_plot <- read_plot + scale_y_continuous(expand = c(0, 0),
                                                  limit = (c(0, input$read_yaxis_limit)))

      ## Add faceting for sorting
      read_plot <- read_plot +
        facet_grid(
          ~ eval(parse(text = input$read_sortby_axis)),
          space = "free",
          scales = "free",
          switch = "both"
        )

      if (input$read_panel == "Yes") {
        read_plot <- read_plot + theme_bw() +
          theme(
            panel.grid = element_blank(),
            text = element_text(colour = "black"),
            #axis.line = element_line(colour = "black"),
            axis.line = element_blank(),
            axis.text = element_text(colour = "black", size = 12),
            axis.text.x = element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5,
              size = 12
            ),
            legend.text = element_text(face = "italic", size = 12),
            legend.title = element_text(size = 14),
            panel.spacing = unit(as.numeric(input$read_panel_spacing), "points"),
            #legend.position = "none",
            axis.title = element_text(size = 14, face = NULL),
            axis.text.y = element_text(size = 16),
            strip.text.x = element_text(size = 10, face = "bold"),
          )
      }
      
      if (input$read_panel == "No") {
        read_plot <- read_plot + theme_bw() +
          theme(
            panel.grid = element_blank(),
            text = element_text(colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.text = element_text(colour = "black", size = 12),
            axis.text.x = element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5,
              size = 12
            ),
            legend.text = element_text(face = "italic", size = 12),
            legend.title = element_text(size = 14),
            panel.border = element_blank(),
            panel.spacing = unit(as.numeric(input$read_panel_spacing), "points"),
            #legend.position = "none",
            axis.title = element_text(size = 14, face = NULL),
            axis.text.y = element_text(size = 16),
            strip.text.x = element_text(size = 10, face = "bold"),
          )
      }
      
      read_plot <- read_plot + labs(fill = "Sample category")
      read_plot <- read_plot + xlab("Samples")
      read_plot <- read_plot + ylab("Total reads following DADA2")
      
      read_plot
    })


    output$read_table_out <- renderDataTable({
      read_table <- data_read_table()
      read_table
    })
    
    
    
    ## You must define the input for width/height within a reactive context, then call it in the output
    read_plot_height = reactive(input$read_plot_out_h)
    read_plot_width = reactive(input$read_plot_out_w)

    
    output$read_plot = renderPlot({
      # if (!read_start_pressed()){
      #   return(NULL)
      # }
      # if (read_start_pressed())
      # input$read_start
      read_plot = data_read_plot()
      read_plot
      
    },
    width = read_plot_width,
    height = read_plot_height)
    })# End of giant reactive
  # End of if
  
  # #
  # observeEvent(input$read_start, {
  #   read_start_pressed(TRUE)
  # })
  
  

    
    
  output$read_download = downloadHandler(
    filename = "read_plot.pdf",
    contentType = ".pdf",
    content = function(read_file) {
      ggsave(
        read_file,
        plot = data_read_plot(),
        device = "pdf",
        height = as.numeric(input$read_plot_out_h),
        width = as.numeric(input$read_plot_out_w),
        units = "px",
        scale = 4
      )
    }
  )
  
  
  
  #### Bar Plot ####
    data_long_bar_filt_re <- reactive({
      filtered_table <- data_filtered_table()
      bar_data_prop <- data_filtered_table()
      meta_data_table <- meta_datafile()
      unfiltered_table <- data_unfiltered_table()
      
      ## Initial transformations:
      ## Produce a prop table and filter it to a specific threshold
      bar_data_prop <- prop.table(as.matrix(bar_data_prop), 2) * 100
      bar_data_prop <- as.data.frame(bar_data_prop)
      
      ## If the table contains any columns with zero reads, the column reports NA, and the script fails after this point.
      ## These columns must be removed first.
      bar_data_prop <- bar_data_prop[, colSums(is.na(bar_data_prop)) == 0]
      bar_data_prop <- bar_data_prop %>% filter_all(any_vars(. >= as.numeric(input$bar_cutoff)))
      
      ## Reassigns taxonomy to the filtered table
      bar_data_prop$Taxonomy <- rownames(bar_data_prop)
      bar_data_prop$TaxaName <- rownames(bar_data_prop)
      
      ## Collect the ASV IDs before mutating labels:
      bar_rowIDs <-
        as.data.frame(gsub(".*_", "", bar_data_prop$Taxonomy))
      colnames(bar_rowIDs) <- "rowIDs"
      
      ## Right now I have to redo the labels since I haven't figured out a way to exclude them from filter step. Until then:
      ## Cleans up labels and removes uncultured/ambiguous taxa
      bar_labels <- bar_data_prop$Taxonomy
      bar_labels <- gsub("_[0-9]*$", "", bar_labels, )
      bar_labels <-
        gsub("(;Ambiguous__taxa)", ";s__Ambiguous_taxa", bar_labels)
      bar_labels <-
        gsub("(;Ambiguous_taxa)", ";s__Ambiguous_taxa", bar_labels)
      bar_labels <- gsub(" ", "", bar_labels)
      
      if (input$truncate_taxa == "Yes") {
        bar_labels <- paste(";", sep = "", bar_labels)
        bar_labels <- gsub("(;\\s*Ambiguous_taxa)", "", bar_labels)
        bar_labels <- gsub("(uncultured.*)", "", bar_labels)
        bar_labels <- gsub("(__uncultured.*)", "", bar_labels)
        bar_labels <- gsub("(unidenti.*)", "", bar_labels)
        bar_labels <- gsub("(__unidenti.*)", "", bar_labels)
        bar_labels <- gsub("(;.__Ambiguous_taxa)", "", bar_labels)
        bar_labels <- gsub("(;._Ambiguous_taxa)", "", bar_labels)
        bar_labels <- gsub("(;s__$)", "", bar_labels)
        bar_labels <- gsub("(;g__$)", "", bar_labels)
      }
      
      ## Remove taxa pre-fixes associated with SILVA classifier (old):
      if (input$remove_prefix == "Yes") {
        bar_labels <- gsub("(D_.__)", "", bar_labels)
        bar_labels <- gsub(";$", "", bar_labels)
      }
      bar_labels <- gsub("(;D_.__$)", "", bar_labels)
      
      
      ## Remove taxa-prefixed associated with SILVA classifier (new):
      if (input$remove_prefix == "Yes") {
        bar_labels <- gsub("(d__)", "", bar_labels)
        bar_labels <- gsub("(p__)", "", bar_labels)
        bar_labels <- gsub("(c__)", "", bar_labels)
        bar_labels <- gsub("(o__)", "", bar_labels)
        bar_labels <- gsub("(f__)", "", bar_labels)
        bar_labels <- gsub("(g__)", "", bar_labels)
        bar_labels <- gsub("(s__)", "", bar_labels)
        bar_labels <- gsub(" ", "", bar_labels)
        bar_labels <- gsub("(;metagenome$)", "", bar_labels)
        bar_labels <- gsub("(__.$)", "", bar_labels)
        bar_labels <- gsub("(;__)", "", bar_labels)
        bar_labels <- gsub("(;$)", "", bar_labels)
        bar_labels <-
          gsub("(;$)", "", bar_labels) #This is not a duplicate -- leave it here
      }
      
      ## Further refining labels.
      bar_labels <- gsub(" ", "", bar_labels)
      bar_labels <- gsub("(__.$)", "", bar_labels)
      bar_labels <- gsub("(;$)", "", bar_labels)
      bar_labels <- gsub("(;__)", "", bar_labels)
      #bar_labels<-gsub("(_)","",bar_labels)
      bar_labels <- gsub(".*;", "", bar_labels)
      bar_labels <-
        paste(gsub(".*;$", "", bar_labels), bar_rowIDs$rowIDs, sep = "_")
      bar_labels <- as.data.frame(bar_labels, colnames("labels"))
      colnames(bar_labels) <- "labels"
      #bar_labels<-gsub("([:digit:]$)","_",bar_labels)
      
      
      ## Apply the labels to the new proportion table:
      bar_data_prop$TaxaName <- bar_labels$labels
      rownames(bar_data_prop) <- bar_data_prop$TaxaName

      ## Transform into long form:
      bar_data_long <-
        reshape2::melt(
          bar_data_prop,
          id.vars = c("TaxaName", "Taxonomy"),
          variable.name = as.character("SampleName"),
          value.name = "Percentage"
        )
      
      ## Add representative sequences back into the data by merging tables based on taxonomy. If collapsed, skips.
      unfiltered_table$Taxonomy <- rownames(unfiltered_table)
      if (input$is_main_collapsed == FALSE) {
        unfiltered_table <- unfiltered_table[, c("Taxonomy", "ReprSequence")]
      }
      bar_data_long <- left_join(bar_data_long,
                                unfiltered_table,
                                by = "Taxonomy",
                                copy = TRUE)
      
      ## Modify taxonomy names
      bar_data_long$Taxonomy <-
        gsub("(D_.__)", "", bar_data_long$Taxonomy)
      bar_data_long$Taxonomy <- gsub(";$", "", bar_data_long$Taxonomy)
      
      
      ## Filter above a threshold, append metadata, and round decimals -- I think this needs to be moved after the filtering
      data_long_bar <- bar_data_long
      data_long_bar <- dplyr::filter(data_long_bar, Percentage > 0)
      data_long_bar <-
        left_join(data_long_bar,
                  meta_data_table,
                  by = "SampleName",
                  copy = TRUE)

      # ## Include only specific taxa  within the plot
      # if (is.na(input$b1_tax_keyword) == TRUE){
      #   warning("No specific taxon selected. Script will continue without filtering by taxa.")
      # } else {
      #   taxon_hits <- grepl(pattern = paste(input$b1_tax_keyword), ignore.case = TRUE, x = data_long_bar$Taxonomy)
      #   data_long_bar <- data_long_bar[taxon_hits,]
      #   warning("Taxa filtering selected.")
      # }
      #
      # ## Include only specific sample groups within the plot/;
      # if (is.na(input$b1_meta_group) == TRUE){
      #   warning("No metadata category selected. Script will continue without filtering by groups.")
      # } else {
      #   sample_hits <- grepl(pattern = input$b1_meta_keyword, ignore.case = TRUE, x = (eval(parse(text=paste("data_long_bubble$",input$b1_meta_group)))))
      #   data_long_bubble <- data_long_bubble[sample_hits,]
      #   warning("Metadata filtering selected.")
      # }
      
      ## Attach full taxonomic lineage and separate into classifications, and fix labelling issues:
      data_long_bar$Sep <- data_long_bar$Taxonomy
      data_long_bar$Sep <- gsub("(d__)", "", data_long_bar$Sep)
      data_long_bar$Sep <- gsub("(p__)", "", data_long_bar$Sep)
      data_long_bar$Sep <- gsub("(c__)", "", data_long_bar$Sep)
      data_long_bar$Sep <- gsub("(o__)", "", data_long_bar$Sep)
      data_long_bar$Sep <- gsub("(f__)", "", data_long_bar$Sep)
      data_long_bar$Sep <- gsub("(g__)", "", data_long_bar$Sep)
      data_long_bar$Sep <- gsub("(s__)", "", data_long_bar$Sep)
      data_long_bar <-
        separate(
          data_long_bar,
          Sep,
          c(
            "Domain",
            "Phylum",
            "Class",
            "Order",
            "Family",
            "Genus",
            "Species"
          ),
          sep = ";",
          remove = TRUE,
          convert = FALSE
        )
      
      data_long_bar$Identifier <- 1:nrow(data_long_bar)
      # data_long_bar_filt <- dplyr::filter(data_long_bar, Percentage > as.numeric(input$bar_cutoff))
      data_long_bar_filt <- data_long_bar
      
      
      ## Include only specific taxa  within the plot
      if (is.na(input$taxon_subset) == TRUE) {
        warning("No specific taxon selected. Script will continue without filtering by taxa.")
      } else {
        taxon_hits <-
          grepl(
            pattern = input$taxon_subset,
            ignore.case = TRUE,
            x = data_long_bar_filt$Taxonomy
          )
        data_long_bar_taxa_filterd <-
          data_long_bar_filt[taxon_hits, ]
        warning("Taxa filtering selected. CONGRATULATIONS")
      }
      
      # ## Assigns all excluded taxa below the set threshold to category "ZZOther" and provides the appropriate relative abundance:
      bar_incl_samples <-
        aggregate(
          data_long_bar_filt$Percentage,
          by = list(data_long_bar_filt$SampleName),
          FUN = sum
        )
      bar_incl_samples <-
        cbind(bar_incl_samples, 100 - bar_incl_samples$x)
      colnames(bar_incl_samples) <-
        c("SampleName", "FiltSum", "Percentage")
      bar_incl_samples$TaxaName <- "ZZOther"
      bar_incl_samples$Taxonomy <- "ZZOther"
      bar_incl_samples <- select(bar_incl_samples, -c("FiltSum"))
      data_long_bar_filt <-
        bind_rows(data_long_bar_filt, bar_incl_samples)
      # data_long_bar_filt <- bind_rows(data_long_bar_filt,bar_ex_samples)
      
      
      barfilt <- colnames(meta_data_table)
      barfilt <- barfilt[barfilt != "SampleName"]
      data_long_bar_filt <- select(data_long_bar_filt,-barfilt)
      
      
      ## Modify the labels again to remove numbers from the legend:
      data_long_bar_filt$TaxaName <-
        gsub("_[0-9]*$", "", data_long_bar_filt$TaxaName)
      
      # ## Append the metadata
      data_long_bar_filt <-
        left_join(data_long_bar_filt,
                  meta_data_table,
                  by = "SampleName",
                  copy = FALSE)
      # data_long_bar_filt <- data_long_bar_filt[order(eval(parse(text=paste("data_long_bar_filt$",input$sort_para))),decreasing = FALSE),]
      
      ## Reorder x-axis to follow metadata category in data frame
      data_long_bar_filt$SampleName <-
        as.character(data_long_bar_filt$SampleName)
      data_long_bar_filt$SampleName <-
        factor(data_long_bar_filt$SampleName,
               levels = unique(data_long_bar_filt$SampleName))
      data_long_bar_filt
    })
    
    barplot_reactive <- reactive({
      data_long_bar_filt <- data_long_bar_filt_re()
      
      bar_plot <- ggplot(data = data_long_bar_filt,
                         aes(x = SampleName,
                             y = Percentage, width = 1))
      
      bar_plot <- bar_plot + geom_bar(
        aes(fill = TaxaName),
        colour = "black",
        size = 0.5,
        alpha = input$bar_alpha,
        stat = "identity",
        position = "stack"
      )
      
      ## Add faceting for sorting
      bar_plot <- bar_plot +
        facet_grid(
          ~ eval(parse(text = input$bar_sortby_xaxis)),
          space = "free",
          scales = "free",
          switch = "both"
        )

      if (input$bar_panel_border == "Yes") {
        bar_plot <- bar_plot + theme_bw() +
          theme(
            panel.grid = element_blank(),
            text = element_text(colour = "black"),
            # axis.line = element_line(colour = "black"),
            axis.line = element_blank(),
            axis.text = element_text(colour = "black", size = 12),
            axis.text.x = element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5,
              size = 12
            ),
            legend.text = element_text(face = "italic", size = 12),
            legend.title = element_text(size = 14),
            panel.spacing = unit(as.numeric(input$bar_panel_spacing), "points"),
            #legend.position = "none",
            axis.title = element_text(size = 10, face = NULL),
            axis.text.y = element_text(size = 16),
            strip.text.x = element_text(size = 10, face = "bold"),
          )
      }
      
      if (input$bar_panel_border == "No") {
        bar_plot <- bar_plot + theme_bw() +
          theme(
            panel.grid = element_blank(),
            text = element_text(colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.text = element_text(colour = "black", size = 12),
            axis.text.x = element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5,
              size = 12
            ),
            legend.text = element_text(face = "italic", size = 12),
            legend.title = element_text(size = 14),
            panel.border = element_blank(),
            panel.spacing = unit(as.numeric(input$bar_panel_spacing), "points"),
            #legend.position = "none",
            axis.title = element_text(size = 10, face = NULL),
            axis.text.y = element_text(size = 16),
            strip.text.x = element_text(size = 10, face = "bold"),
          )
      }
      
      ## setting the graph so that it begins at the x-axis and there is no gap. Also sets the limits of the y-axis.
      bar_plot <- bar_plot + scale_y_continuous(expand = c(0, 0))

      nb.cols <-
        length(unique(eval(parse(
          text = paste("data_long_bar_filt$", "TaxaName")
        ))))
      mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
      ## colour fill using Brewers
      bar_plot <- bar_plot + scale_fill_manual(values = mycolors)
      #bar_plot <- bar_plot + scale_fill_brewer(palette = "Paired")

      bar_plot = bar_plot + ylab(
        paste(
          "Proportion of affiliated taxa",
          " ",
          "at",
          " ",
          ">",
          eval(parse(text = input$bar_cutoff)),
          "%",
          " ",
          "relative",
          " ",
          "abundance",
          sep = ""
        )
      )
      bar_plot <- bar_plot + xlab("Samples")
      bar_plot <- bar_plot + labs(fill = "Taxonomy")
      
      #bar_plot<-bar_plot + ylab(input$bar_yaxis)
      bar_plot
    })
    
    
    output$bar_table_out <- renderDataTable({
      bar_table_out <- data_long_bar_filt_re()
      bar_table_out
    })
    
    ## You must define the input for width/height within a reactive context, then call it in the output.
    taxa_plot_width <- reactive(input$taxa_plot_out_w)
    taxa_plot_height <- reactive(input$taxa_plot_out_h)
    
    output$taxa_bar <- renderPlot({
      barplot <- barplot_reactive()
      barplot
    },
    width = taxa_plot_width,
    height = taxa_plot_height)
    

  output$bar_download <- downloadHandler(
    filename = "bar_plot.pdf",
    contentType = ".pdf",
    content = function(bar_file) {
      ggsave(
        bar_file,
        plot = barplot_reactive(),
        device = "pdf",
        height = as.numeric((input$taxa_plot_out_h)),
        width = as.numeric((input$taxa_plot_out_w)),
        units = "px",
        scale = 4
      )
    }
  )
  
  #### New bubble plot ####
    data_bubble_reactive_new <- reactive({
      data_tran <- data_tran_react()
      meta_data_table <- meta_datafile()
      unfiltered_table <- data_unfiltered_table()
      filtered_table <- data_filtered_table()

      # ## This removes samples from the ASV tables that don't have a corresponding metadata row: THIS IS WORKING
      meta_samplist <- meta_data_table$SampleName
      filtered_table <- filtered_table[, names(filtered_table) %in% meta_samplist]
      
      ## Initial transformations:
      ## Produce a prop table and filter it to a specific threshold
      B2_data_prop <- prop.table(as.matrix(filtered_table), 2) * 100
      B2_data_prop <- as.data.frame(B2_data_prop)
      
      ## If the table contains any columns with zero reads, the column reports NA, and the script fails after this point.
      ## These columns must be removed first.
      B2_data_prop <- B2_data_prop[, colSums(is.na(B2_data_prop)) == 0]
      
      ## This checks all columns for a value above a threshold and keeps all rows if found. However, this screws up downstream metadata filtering, so I need to figure out how to change this. During the metadata step, any samples without representation in the metadata table will be removed. If those removed samples contained reads above a threshold, the remaining samples will stay in the filtered table (and they should not).
      B2_data_prop <- B2_data_prop %>% filter_all(any_vars(. >= as.numeric(input$b1_ab_thresh)))
      
      ## Reassigns taxonomy to the filtered table
      B2_data_prop$Taxonomy <- rownames(B2_data_prop)
      B2_data_prop$TaxaName <- rownames(B2_data_prop)
      
      ## Collect the ASV IDs before mutating labels:
      B2_rowIDs <-
        as.data.frame(gsub(".*_", "", B2_data_prop$Taxonomy))
      colnames(B2_rowIDs) <- "rowIDs"
      
      ## Right now I have to redo the labels since I haven't figured out a way to exclude them from filter step. Until then:
      ## Cleans up labels and removes uncultured/ambiguous taxa
      B2_labels <- B2_data_prop$Taxonomy
      B2_labels <- gsub("_[0-9]*$", "", B2_labels, )
      B2_labels <-
        gsub("(;Ambiguous__taxa)", ";s__Ambiguous_taxa", B2_labels)
      B2_labels <-
        gsub("(;Ambiguous_taxa)", ";s__Ambiguous_taxa", B2_labels)
      B2_labels <- gsub(" ", "", B2_labels)
      
      if (input$truncate_taxa == "Yes") {
        B2_labels <- paste(";", sep = "", B2_labels)
        B2_labels <- gsub("(;\\s*Ambiguous_taxa)", "", B2_labels)
        B2_labels <- gsub("(uncultured.*)", "", B2_labels)
        B2_labels <- gsub("(__uncultured.*)", "", B2_labels)
        B2_labels <- gsub("(unidenti.*)", "", B2_labels)
        B2_labels <- gsub("(__unidenti.*)", "", B2_labels)
        B2_labels <- gsub("(;.__Ambiguous_taxa)", "", B2_labels)
        B2_labels <- gsub("(;._Ambiguous_taxa)", "", B2_labels)
        B2_labels <- gsub("(;s__$)", "", B2_labels)
        B2_labels <- gsub("(;g__$)", "", B2_labels)
      }
      
      ## Remove taxa pre-fixes associated with SILVA classifier (old):
      if (input$remove_prefix == "Yes") {
        B2_labels <- gsub("(D_.__)", "", B2_labels)
        B2_labels <- gsub(";$", "", B2_labels)
      }
      B2_labels <- gsub("(;D_.__$)", "", B2_labels)
      
      
      ## Remove taxa-prefixed associated with SILVA classifier (new):
      if (input$remove_prefix == "Yes") {
        B2_labels <- gsub("(d__)", "", B2_labels)
        B2_labels <- gsub("(p__)", "", B2_labels)
        B2_labels <- gsub("(c__)", "", B2_labels)
        B2_labels <- gsub("(o__)", "", B2_labels)
        B2_labels <- gsub("(f__)", "", B2_labels)
        B2_labels <- gsub("(g__)", "", B2_labels)
        B2_labels <- gsub("(s__)", "", B2_labels)
        B2_labels <- gsub(" ", "", B2_labels)
        B2_labels <- gsub("(;metagenome$)", "", B2_labels)
        B2_labels <- gsub("(__.$)", "", B2_labels)
        B2_labels <- gsub("(;__)", "", B2_labels)
        B2_labels <- gsub("(;$)", "", B2_labels)
        B2_labels <-
          gsub("(;$)", "", B2_labels) #This is not a duplicate -- leave it here
      }
      
      ## Further refining labels.
      B2_labels <- gsub(" ", "", B2_labels)
      B2_labels <- gsub("(__.$)", "", B2_labels)
      B2_labels <- gsub("(;$)", "", B2_labels)
      B2_labels <- gsub("(;__)", "", B2_labels)
      #B2_labels<-gsub("(_)","",B2_labels)
      B2_labels <- gsub(".*;", "", B2_labels)
      B2_labels <-
        paste(gsub(".*;$", "", B2_labels), B2_rowIDs$rowIDs, sep = "_")
      B2_labels <- as.data.frame(B2_labels, colnames("labels"))
      colnames(B2_labels) <- "labels"
      #B2_labels<-gsub("([:digit:]$)","_",B2_labels)
      
      
      ## Apply the labels to the new proportion table:
      B2_data_prop$TaxaName <- B2_labels$labels
      rownames(B2_data_prop) <- B2_data_prop$TaxaName
      
      
      ## Transform into long form:
      B2_data_long <-
        reshape2::melt(
          B2_data_prop,
          id.vars = c("TaxaName", "Taxonomy"),
          variable.name = as.character("SampleName"),
          value.name = "Percentage"
        )
      
      
      ## Add representative sequences back into the data by merging tables based on taxonomy. If collapsed, skips.
      unfiltered_table$Taxonomy <- rownames(unfiltered_table)
      if (input$is_main_collapsed == FALSE) {
        unfiltered_table <- unfiltered_table[, c("Taxonomy", "ReprSequence")]
      }
      B2_data_long <- left_join(B2_data_long,
                               unfiltered_table,
                               by = "Taxonomy",
                               copy = TRUE)
      
      
      ## Modify taxonomy names
      B2_data_long$Taxonomy <-
        gsub("(D_.__)", "", B2_data_long$Taxonomy)
      B2_data_long$Taxonomy <- gsub(";$", "", B2_data_long$Taxonomy)
      
      
      
      
      ## Filter above a threshold, append metadata, and round decimals -- I think this needs to be moved after the filtering
      
      data_long_bubble <- B2_data_long
      data_long_bubble <-
        dplyr::filter(data_long_bubble, Percentage > 0)
      data_long_bubble <-
        left_join(data_long_bubble,
                  meta_data_table,
                  by = "SampleName",
                  copy = TRUE)
      
      
      
      ## Round percentages to a given decimal places
      data_long_bubble$Percentage <-
        round(data_long_bubble$Percentage,
              digits = as.numeric(input$b1_num_dec))
      
      
      
      
      ## Include only specific taxa  within the plot
      if (is.na(input$b1_tax_keyword) == TRUE) {
        warning("No specific taxon selected. Script will continue without filtering by taxa.")
      } else {
        taxon_hits <-
          grepl(
            pattern = paste(input$b1_tax_keyword),
            ignore.case = TRUE,
            x = data_long_bubble$Taxonomy
          )
        data_long_bubble <- data_long_bubble[taxon_hits, ]
        warning("Taxa filtering selected.")
      }
      
   
      ## Attach full taxonomic lineage and separate into classifications, and fix labelling issues:
      data_long_bubble$Sep <- data_long_bubble$Taxonomy
      data_long_bubble$Sep <- gsub("(d__)", "", data_long_bubble$Sep)
      data_long_bubble$Sep <- gsub("(p__)", "", data_long_bubble$Sep)
      data_long_bubble$Sep <- gsub("(c__)", "", data_long_bubble$Sep)
      data_long_bubble$Sep <- gsub("(o__)", "", data_long_bubble$Sep)
      data_long_bubble$Sep <- gsub("(f__)", "", data_long_bubble$Sep)
      data_long_bubble$Sep <- gsub("(g__)", "", data_long_bubble$Sep)
      data_long_bubble$Sep <- gsub("(s__)", "", data_long_bubble$Sep)
      data_long_bubble <-
        separate(
          data_long_bubble,
          Sep,
          c(
            "Domain",
            "Phylum",
            "Class",
            "Order",
            "Family",
            "Genus",
            "Species"
          ),
          sep = ";",
          remove = TRUE,
          convert = FALSE
        )
      
      # Set the taxonomy as factors for later ordering. This doesn't do much, but if you remove it the colouring settings change
      if (isTRUE(input$b1_confirm_sort == "Yes")) {
        data_long_bubble <-
          data_long_bubble[with(data_long_bubble,
                                order(eval(parse(
                                  text = input$b1_tax_sort
                                )), TaxaName, decreasing = TRUE)), ]
        data_long_bubble$TaxaName <-
          as.character(data_long_bubble$TaxaName)
        data_long_bubble$TaxaName <-
          factor(data_long_bubble$TaxaName,
                 levels = unique(data_long_bubble$TaxaName))
        data_long_bubble
      }
      data_long_bubble
    })
    
    #### Old bubble plot ####
    data_bubble_reactive_old <- reactive({
      data_long <- data_long_react()
      meta_data_table <- meta_datafile()
      
      
      ## Filter above a threshold, append metadata, and round decimals
      data_long_bubble <- data_long
      data_long_bubble <-
        dplyr::filter(data_long, Percentage > as.numeric(input$b1_ab_thresh))
      data_long_bubble <-
        left_join(data_long_bubble,
                  meta_data_table,
                  by = "SampleName",
                  copy = TRUE)
      data_long_bubble$Percentage <-
        round(data_long_bubble$Percentage,
              digits = as.numeric(input$b1_num_dec))
      
      
      ## Include only specific taxa  within the plot
      if (is.na(input$b1_tax_keyword) == TRUE) {
        warning("No specific taxon selected. Script will continue without filtering by taxa.")
      } else {
        taxon_hits <-
          grepl(
            pattern = input$b1_tax_keyword,
            ignore.case = TRUE,
            x = data_long_bubble$Taxonomy
          )
        data_long_bubble <- data_long_bubble[taxon_hits, ]
        warning("CONGRATULATIONS! Taxa filtering selected.")
      }
      
      ## Include only specific sample groups within the plot/;
      if (is.na(input$b1_meta_group) == TRUE) {
        warning(
          "No metadata category selected. Script will continue without filtering by groups."
        )
      } else {
        sample_hits <-
          grepl(
            pattern = input$b1_meta_keyword,
            ignore.case = TRUE,
            x = (eval(parse(
              text = paste("data_long_bubble$", input$b1_meta_group)
            )))
          )
        data_long_bubble <- data_long_bubble[sample_hits, ]
        warning("Metadata filtering selected.")
      }
      
      
      ## Attach full taxonomic lineage and separate into classifications, and fix labelling issues:
      data_long_bubble$Sep <- data_long_bubble$Taxonomy
      data_long_bubble$Sep <- gsub("(d__)", "", data_long_bubble$Sep)
      data_long_bubble$Sep <- gsub("(p__)", "", data_long_bubble$Sep)
      data_long_bubble$Sep <- gsub("(c__)", "", data_long_bubble$Sep)
      data_long_bubble$Sep <- gsub("(o__)", "", data_long_bubble$Sep)
      data_long_bubble$Sep <- gsub("(f__)", "", data_long_bubble$Sep)
      data_long_bubble$Sep <- gsub("(g__)", "", data_long_bubble$Sep)
      data_long_bubble$Sep <- gsub("(s__)", "", data_long_bubble$Sep)
      data_long_bubble <-
        separate(
          data_long_bubble,
          Sep,
          c(
            "Domain",
            "Phylum",
            "Class",
            "Order",
            "Family",
            "Genus",
            "Species"
          ),
          sep = ";",
          remove = TRUE,
          convert = FALSE
        )
      
      data_long_bubble
    })
    
    
    #### Generate the bubble plot ####
    observeEvent(input$bubble_start, {
    bubble_plot_re <- reactive({
      if (input$b1_new_old == "Old") {
        data_long_bubble <- data_bubble_reactive_old()
      } else {
        data_long_bubble <- data_bubble_reactive_new()
      }
      
      ## Plot the Data:
      bubble_plot <- ggplot(
        data_long_bubble,
        aes(
          x = reorder(SampleName, eval(parse(
            text = input$b1_sort_axis
          ))),

          if (input$b1_confirm_sort == "Yes") {
            y = TaxaName
          } else {
            y = reorder(TaxaName, desc(TaxaName))
          },
          fill = as.factor(eval(parse(
            text = input$b1_color_param
          ))),
          color = as.factor(eval(parse(
            text = input$b1_color_param
          ))),
          size = Percentage
        ),
        colsep = c(1:100),
        rowsep = (1:100),
        sepwidth = c(5, 1)
      ) +
        guides(fill = FALSE, colour = FALSE)
      
      ## Sorting the y-axis and faceting options. I need to overhaul this to use three levels of nested faceting
      ## So for some reason you need to use the * operators instead of the + for faceting. Not sure why this changed. 

      
      # Adding second faceting
      if (input$b1_second_facet == TRUE) {
        bubble_plot <- bubble_plot +
          
          # First must check the facet options. Not sure how else to do this without running through all possible cases:
          if (input$b1_confirm_sort == "Yes") {
            if ((input$b1_facet_side_x == "Top") &
                (input$b1_facet_side_y == "Right")) {
              facet_nested(
                eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = input$b1_sort_param)) +
                  eval(parse(text = input$b1_second_facet_meta)),
                space = "free",
                scales = "free"
              )}
            
           else if ((input$b1_facet_side_x == "Top") &
                (input$b1_facet_side_y == "Left")) {
              facet_nested(
                eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = input$b1_sort_param)) +
                  eval(parse(text = input$b1_second_facet_meta)),
                space = "free",
                scales = "free",
                switch = "y"
              )}
            
          else if ((input$b1_facet_side_x == "Bottom") &
                (input$b1_facet_side_y == "Left")) {
              facet_nested(
                eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = input$b1_sort_param)) +
                  eval(parse(text = input$b1_second_facet_meta)),
                space = "free",
                scales = "free",
                switch = "both"
              )}
            
           else if ((input$b1_facet_side_x == "Bottom") &
                (input$b1_facet_side_y == "Right")) {
              facet_nested(
                eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = input$b1_sort_param)) +
                  eval(parse(text = input$b1_second_facet_meta)),
                space = "free",
                scales = "free",
                switch = "x"
              )}
          }
        
        # Now if you're not sorting the y-axis based on taxonomy:
       else if (input$b1_confirm_sort == "No") {
          if (input$b1_facet_side_x == "Top") {
            facet_nested(
              ~ eval(parse(text = input$b1_sort_param)) +
                eval(parse(text = input$b1_second_facet_meta)),
              space = "free",
              scales = "free"
            )
          }
          
         else if (input$b1_facet_side_x == "Bottom") {
            facet_nested(
              ~ eval(parse(text = input$b1_sort_param)) +
                eval(parse(text = input$b1_second_facet_meta)),
              space = "free",
              scales = "free",
              switch = "x"
            )
          }
        }
      }# This is the end of the second-level faceting
      
      
      # If no additional faceting is chosen:
      if (input$b1_second_facet == FALSE) {
        bubble_plot <- bubble_plot +
          
          # First must check the facet options. Not sure how else to do this without running through all possible cases:
          if (input$b1_confirm_sort == "Yes") {
            if ((input$b1_facet_side_x == "Top") &
                (input$b1_facet_side_y == "Right")) {
              facet_nested(
                eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = input$b1_sort_param)),
                space = "free",
                scales = "free"
              )}
            
           else if ((input$b1_facet_side_x == "Top") &
                (input$b1_facet_side_y == "Left")) {
              facet_nested(
                eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = input$b1_sort_param)),
                space = "free",
                scales = "free",
                switch = "y"
              )}
            
           else if ((input$b1_facet_side_x == "Bottom") &
                (input$b1_facet_side_y == "Left")) {
              facet_nested(
                eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = input$b1_sort_param)),
                space = "free",
                scales = "free",
                switch = "both"
              )}
            
           else if ((input$b1_facet_side_x == "Bottom") &
                (input$b1_facet_side_y == "Right")) {
              facet_nested(
                eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = input$b1_sort_param)),
                space = "free",
                scales = "free",
                switch = "x"
              )}
          }
        
        # Now if you're not sorting the y-axis based on taxonomy:
       else if (input$b1_confirm_sort == "No") {
          if (input$b1_facet_side_x == "Top") {
            facet_nested(
              ~ eval(parse(text = input$b1_sort_param)),
              space = "free",
              scales = "free"
            )
          }
          
        else if (input$b1_facet_side_x == "Bottom") {
            facet_nested(
              ~ eval(parse(text = input$b1_sort_param)),
              space = "free",
              scales = "free",
              switch = "x"
            )
          }
        }
      }# This is the end of default faceting
      
      
      
      # Third level faceting. This code must come after all others. Not sure why. But if it comes first it doesn't remain dynamic
      if (input$b1_third_facet == TRUE) {
        bubble_plot <- bubble_plot +
          
          # First must check the facet options. Not sure how else to do this without running through all possible cases:
          if (input$b1_confirm_sort == "Yes") {
            if ((input$b1_facet_side_x == "Top") &
                (input$b1_facet_side_y == "Right")) {
              facet_nested(
                eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = input$b1_sort_param)) *
                  eval(parse(text = input$b1_second_facet_meta)) * 
                  eval(parse(text = input$b1_third_facet_meta)),
                space = "free",
                scales = "free"
              )}
            
            else if ((input$b1_facet_side_x == "Top") &
                     (input$b1_facet_side_y == "Left")) {
              facet_nested(
                eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = input$b1_sort_param)) *
                  eval(parse(text = input$b1_second_facet_meta)) * 
                  eval(parse(text = input$b1_third_facet_meta)),
                space = "free",
                scales = "free",
                switch = "y"
              )}
            
            else if ((input$b1_facet_side_x == "Bottom") &
                     (input$b1_facet_side_y == "Left")) {
              facet_nested(
                eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = input$b1_sort_param)) *
                  eval(parse(text = input$b1_second_facet_meta)) * 
                  eval(parse(text = input$b1_third_facet_meta)),
                space = "free",
                scales = "free",
                switch = "both"
              )}
            else if ((input$b1_facet_side_x == "Bottom") &
                     (input$b1_facet_side_y == "Right")) {
              facet_nested(
                eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = input$b1_sort_param)) *
                  eval(parse(text = input$b1_second_facet_meta)) * 
                  eval(parse(text = input$b1_third_facet_meta)),
                space = "free",
                scales = "free",
                switch = "x"
              )}
          }
        
        # Now if you're not sorting the y-axis based on taxonomy:
        else if (input$b1_confirm_sort == "No") {
          if (input$b1_facet_side_x == "Top") {
            facet_nested(
              ~ eval(parse(text = input$b1_sort_param)) *
                eval(parse(text = input$b1_second_facet_meta)) * 
                eval(parse(text = input$b1_third_facet_meta)),
              space = "free",
              scales = "free"
            )
          }
          
          else if (input$b1_facet_side_x == "Bottom") {
            facet_nested(
              ~ eval(parse(text = input$b1_sort_param)) *
                eval(parse(text = input$b1_second_facet_meta)) * 
                eval(parse(text = input$b1_third_facet_meta)),
              space = "free",
              scales = "free",
              switch = "x"
            )
          }
        }
      }# This is the end of the third-level faceting
      

      ## Add the bubbles and percentage labels to the plot:
      
      if (input$b1_incl_percent == "Yes") {
        bubble_plot <- bubble_plot +
          geom_point(shape = 21, alpha = 0.8) +
          geom_text(aes(label = Percentage),
                    colour = "black",
                    size = 3.0) +
          theme(plot.title = element_text(hjust = 0.5)) +
          scale_size_area(max_size = 15) +
          ggtitle("") + xlab("") +
          scale_size_area(max_size = 15) +
          ylab(
            paste(
              "Taxonomic affiliation",
              " ",
              "at",
              " ",
              ">",
              eval(parse(text = input$b1_ab_thresh)),
              "%",
              " ",
              "relative",
              " ",
              "abundance",
              sep = ""
            )
          )
      } else {
        bubble_plot <- bubble_plot +
          geom_point(shape = 21, alpha = 0.8) +
          ggtitle("") + xlab("") +
          scale_size_area(max_size = 15) +
          ylab(
            paste(
              "Taxonomic affiliation",
              " ",
              "at",
              " ",
              ">",
              eval(parse(text = input$b1_ab_thresh)),
              "%",
              " ",
              "relative",
              " ",
              "abundance",
              sep = ""
            )
          )
      }
      
      
      ## Modify the general theme, including panel borders
      if (input$b1_panel_border == "Yes") {
        bubble_plot <- bubble_plot +
          theme_bw() + theme(
            axis.text = element_text(colour = "black", size = 10),
            axis.line = element_blank(),
            #strip.background.y = element_rect(fill = "white"),
            panel.spacing = unit(as.numeric(input$b1_panel_spacing), "points"),
            #panel.grid = element_line(colour = "grey"),
            #axis.line.y = element_line(colour="black",size=0.5),
            #panel.border = element_blank(),
            #text = element_text(size=10),
            axis.text.x = element_text(
              angle = 90,
              vjust = 0.5,
              hjust = 1
            )
            #legend.position = "none")
          )
      }
      
      ## modify the general theme, removing panel borders
      if (input$b1_panel_border == "No") {
        bubble_plot <- bubble_plot +
          theme_bw() + theme(
            axis.text = element_text(colour = "black", size = 10),
            #strip.background.y = element_rect(fill = "white"),
            panel.spacing = unit(as.numeric(input$b1_panel_spacing), "points"),
            #panel.grid = element_line(colour = "grey"),
            #axis.line.y = element_line(colour="black",size=0.5),
            panel.border = element_blank(),
            #text = element_text(size=10),
            axis.text.x = element_text(
              angle = 90,
              vjust = 0.5,
              hjust = 1
            )
            #legend.position = "none")
          )
      }
      
      bubble_plot
      
    })
    
    output$b1_table_out <- renderDataTable({
      if (input$b1_new_old == "New") {
        bubble_table <- data_bubble_reactive_new()
      } else {
        bubble_table <- data_bubble_reactive_old()
      }
    })
    
    b1_plot_height <- reactive(input$b1_plot_out_h)
    b1_plot_width <- reactive(input$b1_plot_out_w)
    
    output$bubble_out <- renderPlot({
      bubble_plot <- bubble_plot_re()
      bubble_plot
    },
    width = b1_plot_width,
    height = b1_plot_height)
    
    output$b1_bubble_download <- downloadHandler(
      filename = "bubble_plot.pdf",
      contentType = ".pdf",
      content = function(bubble_file) {
        ggsave(
          bubble_file,
          plot = bubble_plot_re(),
          device = "pdf",
          height = as.numeric(input$b1_plot_out_h),
          width = as.numeric(input$b1_plot_out_w),
          units = "px",
          scale = 4
        )
      }
    )
    
  })
  
  output$b1_data_table <- downloadHandler(
    filename = "bubble_data_table.csv",
    content = function(bubble_table) {
      write.csv(data_bubble_reactive_new(), bubble_table)
    }
  )
  
  #### Bray Curtis Triplot ####
  
  # Call in the main files
    pcoa_data_main_re <- reactive({
      meta_data_table <- meta_datafile()
      main_data_table <- data_tran_react()
      
      # # Filter for samples present in the metadata file -- Not sure if this is needed
      # meta_names <- c(meta_data_table$SampleName, "Consensus.Lineage", "rowID", "Feature.ID", "ReprSequence")
      # main_data_table <- main_data_table[, names(main_data_table) %in% meta_names]
      # meta_data_table <- meta_data_table %>% filter(SampleName %in% colnames(main_data_table))
      
      ## Simple taxonomy corrections
      main_data_table$Consensus.Lineage <-
        gsub(" ", "", main_data_table$Consensus.Lineage)
      labels <- main_data_table$Consensus.Lineage
      
      ## Cleans up labels and removes uncultured/ambiguous taxa
      labels <- gsub("_[0-9]*$", "", labels)
      labels <- gsub(" ", "", labels)
      labels <- gsub("(;Ambiguous__taxa)", ";s__Ambiguous_taxa", labels)
      labels <- gsub("(;Ambiguous_taxa)", ";s__Ambiguous_taxa", labels)
      
      ## ## Retrieves the last taxonomy entry (i.e., genus, species):
      full_lineage <-
        data.frame(
          TaxaName = paste(
            main_data_table$Consensus.Lineage,
            main_data_table$rowID,
            sep = "_"
          )
        )
      last_lineage <-
        data.frame(TaxaName = paste(gsub(".*;", "", labels), main_data_table$rowID, sep =
                                      "_"))
      
      ## Add the last resolve taxon to the dataframe
      main_data_table$last_taxon <- last_lineage$TaxaName
      rownames(main_data_table) <- last_lineage$TaxaName
      #print(lineage_OTU)
      
      # Create a table containing only non-numeric data
      sample_info_data <- main_data_table[, colnames(main_data_table) %in% c("Consensus.Lineage",
                                                                            "Feature.ID",
                                                                            "rowID",
                                                                            "ReprSequence",
                                                                            "last_taxon")]
      main_data_table
      main_data_table
    })
    
    
    srs_react <- reactive({
      ## Set an option to select SRS, rarefy, or none (default SRS)
      main_data_table <- pcoa_data_main_re()
      meta_data_table <- meta_datafile()
      # Set the seed for reproducibility (optional)
      srs_depth <- input$pcoa_srs_depth
      srs_table <- main_data_table[, !colnames(main_data_table) %in% c("Consensus.Lineage",
                                                                      "Feature.ID",
                                                                      "rowID",
                                                                      "ReprSequence",
                                                                      "last_taxon")]
      srs_table <- SRS(srs_table, Cmin = srs_depth, seed = 123)
      rownames(srs_table) <- main_data_table$last_taxon
      srs_table
    })
    
    srs_proportion_react <- reactive({
      srs_table <- srs_react()
      meta_data_table <- meta_datafile()
      
      # Create a proportion table
      proportion_table <- srs_table / colSums(srs_table)
      
      # # Combine the proportion table back with the non-numeric data
      # proportion_table <- cbind(proportion_table,sample_info_data)
      
      # Remove empty rows
      proportion_table <-
        proportion_table[rowSums(proportion_table) > 0,]
      
      # Transpose table
      proportion_table <- t(proportion_table)
      proportion_table
      
    })
    
    bc_pcoa_react <- reactive({
      proportion_table <- srs_proportion_react()
      meta_data_table <- meta_datafile()
      
      # Generate Bray Curtis
      bray_curtis <-
        as.matrix(vegdist(proportion_table, method = "bray"))
      
      # Perform PCoA using the APE package
      pcoa_result <- pcoa(as.dist(bray_curtis))
      pcoa_result
      
    })
    
    pcoa_coords_react <- reactive({
      pcoa_result <- bc_pcoa_react()
      
      # Extract PCoA coordinates
      pcoa_coords <- pcoa_result$vectors[, 1:2]
      # Create a dataframe with PCoA coordinates and row names
      pcoa_df <-
        data.frame(
          PCoA1 = pcoa_coords[, 1],
          PCoA2 = pcoa_coords[, 2],
          row.names = row.names(pcoa_coords)
        )
      pcoa_df
    })
    
    pcoa_eigen_react <- reactive({
      pcoa_result <- bc_pcoa_react()
      
      # Extract relative eigenvalues
      eigenvalues <- pcoa_result$values$Relative_eig
      
      # Extract the first two eigenvalues
      Axis1 <- eigenvalues[1] * 100
      Axis2 <- eigenvalues[2] * 100
      eigen_df <- data.frame(Axis1 = Axis1, Axis2 = Axis2)
      eigen_df
    })
    
    pcoa_envfit_react <- reactive({
      pcoa_result <- bc_pcoa_react()
      meta_data_table <- meta_datafile()
      srs_table <- srs_react()
      
      # Collect the column names after SRS rarefaction
      pcoa_filt_colnames <- colnames(srs_table)
      
      # Filter the metadata table
      meta_data_table <-
        meta_data_table %>% filter(SampleName %in% pcoa_filt_colnames)
      meta_data_table
      
      # Filter the main table
      
      

      ## Process for all the environment variables for triplot and separate the vectors and R2 ##
      # pcoa_test = cmdscale(pcoa_srs_diss, k=3, eig = TRUE)
      # This must be corrected so that any samples removed in the SRS correction are removed from the metadata table!
      pcoa_envfit <- envfit(pcoa_result$vectors, meta_data_table, perm = 10000)
      
      ## Scales the arrow vectors so they aren't huge
      pcoa_envfit_df <- as.data.frame(pcoa_envfit$vectors$arrows * sqrt(pcoa_envfit$vectors$r))
      pcoa_envfit_df <- cbind(pcoa_envfit_df, pcoa_envfit$vectors$r)
      pcoa_envfit_df <- cbind(pcoa_envfit_df, pcoa_envfit$vectors$pvals)
      colnames(pcoa_envfit_df) <- c("axis1", "axis2", "R", "pvalue")
      
      # # Filter below a specified threshold (p-value?). Need to look more into what these axis values represent.
      # pcoa_envfit_df = filter(pcoa_envfit_df, pcoa_envfit_df$R < 0.5)
      pcoa_envfit_df_filt <- filter(pcoa_envfit_df,
                                   pcoa_envfit_df$pvalue < input$pcoa_env_thresh)
      pcoa_envfit_df_filt
      
    })
    
    pcoa_plot_taxa_fit_react <- reactive({
      pcoa_srs <- srs_proportion_react()
      pcoa_srs_plot <- bc_pcoa_react()
      
      # Generate the weighted average scores
      taxon_weighted_scores <-
        wascores(pcoa_srs_plot$vectors[, 1:3], pcoa_srs)
      
      taxon_weighted_scores[is.na(taxon_weighted_scores)] <- 0 # remove NA values
      
      # Calculate normalized, total abundance of each taxa
      taxa_total_abundance <-
        sum(pcoa_srs) #total number of asvs in the table
      taxa_count <-
        apply(pcoa_srs, 2, sum) #total number of each asv across all samples
      normalized_taxa_count <-
        as.data.frame(taxa_count / taxa_total_abundance)
      colnames(normalized_taxa_count) <- "abundance"
      
      taxon_weighted_scores <-
        as.data.frame(cbind(
          taxon_weighted_scores,
          normalized_taxa_count$abundance
        )) #Append the abundance information
      colnames(taxon_weighted_scores) <-
        c("Axis1", "Axis2", "Axis3", "Abundance") # Change column names
      # taxon_weighted_scores$abundance <- normalized_taxa_count # Append the counts to the weighted average scores
      # taxon_weighted_scores <- subset(taxon_weighted_scores, taxon_weighted_scores$Abundance %in% taxon_weighted_scores$Abundance[taxon_weighted_scores$Abundance > abund_thresh])
      taxon_weighted_scores <-
        filter(
          taxon_weighted_scores,
          taxon_weighted_scores$Abundance > input$pcoa_taxa_thresh / 100
        ) # Filter taxonomy abundance based on a threshold
      taxon_weighted_scores
      
      
    })
    
    pcoa_plot_react <- reactive({
      ## I need to modify this so if the dataframe is empty it doesn't include the environmental fit data
      pcoa_df <- pcoa_coords_react()
      meta_data_table <- meta_datafile()
      pcoa_envfit_df_filt <- pcoa_envfit_react()
      eigen_df <- pcoa_eigen_react()
      taxon_weighted_scores <- pcoa_plot_taxa_fit_react()
      
      # Insert a column of the sample names
      pcoa_df$SampleName <- rownames(pcoa_df)
      # Merge metadata with PCoA dataframe based on row names
      merged_df <-
        left_join(pcoa_df, meta_data_table, by = "SampleName")
      # Change all metadata into categorical data
      merged_df <-
        merged_df %>% mutate_if(!names(.) %in% c("PCoA1", "PCoA2"), factor)
      
      # Define the available shapes and colors
      available_shapes <- c(21, 22, 23, 24, 14, 13:1)
      available_colors <- 2:27
      available_fill <- 2:27
      
      if (input$shape_choice == TRUE){
      # Plot PCoA using ggplot2 with shape and color based on metadata
      pcoa_plot <- ggplot(merged_df, aes(x = PCoA1, y = PCoA2)) +
        # geom_point(size = 4, aes(shape = eval(parse(text = )), fill = get(input$pcoa_fill_col))) +
      
        geom_point(size = input$pcoa_size_select, aes(
          # shape = eval(parse(text = paste("merged_df$",input$pcoa_shape))),
          # colour = eval(parse(text = paste("merged_df$",input$pcoa_fill_col))),
          # fill = eval(parse(text = paste("merged_df$",input$pcoa_fill_col))))) +
          shape = get(input$pcoa_shape),
          colour = get(input$pcoa_fill_col),
          fill = get(input$pcoa_fill_col)
        )) +
        # (eval(parse(text=paste("data_long_bubble$",input$b1_meta_group)))))
        # geom_point(size = 4, aes(shape = get(input$pcoa_shape), fill = get(input$pcoa_fill_col))) +
        labs(
          x = paste(
            "Axis1 variance (",
            round(eigen_df$Axis1, 1),
            "%",
            ")",
            sep = ""
          ),
          y = paste(
            "Axis2 variance (",
            round(eigen_df$Axis2, 1),
            "%",
            ")",
            sep = ""
          ),
          fill = input$pcoa_fill_col,
          colour = input$pcoa_fill_col,
          shape = input$pcoa_shape
        ) +
        # scale_shape_manual(values = available_shapes, name = paste("Data",eval(parse(text = input$pcoa_shape)),sep = "")) +
        # scale_fill_manual(values = available_fill, name =  paste("Data2",eval(parse(text = input$pcoa_fill_col)),sep = "")) +
        # scale_color_manual(values = available_colors, name = "colour", guide = "none")
        scale_shape_manual(values = available_shapes) +
        scale_fill_manual(values = available_fill) +
        scale_colour_manual(values = available_fill)
      # scale_colour_manual(values = available_colors, name = "colour", guide = "none")
        }
      
      if (input$shape_choice == FALSE){
        
        # Collect the names of the metadata category selected
        
        pcoa_colour_name <- input$pcoa_fill_col
        
        
        pcoa_plot <- ggplot(merged_df, aes(x = PCoA1, y = PCoA2)) +
          # geom_point(size = 4, aes(shape = eval(parse(text = )), fill = get(input$pcoa_fill_col))) +
              geom_point(size = input$pcoa_size_select, aes(
              # shape = eval(parse(text = paste("merged_df$",input$pcoa_shape))),
              # colour = eval(parse(text = paste("merged_df$",input$pcoa_fill_col))),
              # fill = eval(parse(text = paste("merged_df$",input$pcoa_fill_col))))) +
              colour = get(input$pcoa_fill_col),
              fill = get(input$pcoa_fill_col)
            )) +
              # (eval(parse(text=paste("data_long_bubble$",input$b1_meta_group)))))
              # geom_point(size = 4, aes(shape = get(input$pcoa_shape), fill = get(input$pcoa_fill_col))) +
              labs(
                x = paste(
                  "Axis1 variance (",
                  round(eigen_df$Axis1, 1),
                  "%",
                  ")",
                  sep = ""
                ),
                y = paste(
                  "Axis2 variance (",
                  round(eigen_df$Axis2, 1),
                  "%",
                  ")",
                  sep = ""
                ),
                # figuring out labeling took way too long, but you don't actually need to do anything more than use the input. 
                fill = input$pcoa_fill_col,
                colour = input$pcoa_fill_col
              ) +
              # scale_shape_manual(values = available_shapes, name = paste("Data",eval(parse(text = input$pcoa_shape)),sep = "")) +
              # scale_fill_manual(values = available_fill, name =  paste("Data2",eval(parse(text = input$pcoa_fill_col)),sep = "")) +
              # scale_color_manual(values = available_colors, name = "colour", guide = "none")
              scale_fill_manual(values = available_fill) +
              scale_colour_manual(values = available_fill)
      }
      
      
      
      if (dim(pcoa_envfit_df_filt)[1] != 0) {
        ## Add triplot info and labels: ## Add if statement here
        pcoa_plot <- pcoa_plot + geom_segment(
          data = pcoa_envfit_df_filt,
          aes(
            x = 0,
            y = 0,
            xend = pcoa_envfit_df_filt$axis1,
            yend = pcoa_envfit_df_filt$axis2,
          ),
          show.legend = FALSE,
          arrow = arrow(ends = "last")
        ) +
          geom_label(
            data = pcoa_envfit_df_filt,
            aes(
              label = rownames(pcoa_envfit_df_filt),
              x = pcoa_envfit_df_filt$axis1 / 2,
              y = pcoa_envfit_df_filt$axis2 / 2
            ),
            size = 4
          )
      }
      
      
      # If sample labels are selected:
      if (input$pcoa_sample_labels == TRUE){
        pcoa_plot <- pcoa_plot +
          geom_text(aes(label = SampleName))
      }
      
      
      # Do you want to include an ellipsis around your data points?:
      if (input$pcoa_elips == TRUE) {
        pcoa_plot <-
          pcoa_plot + stat_ellipse(aes(color = get(input$pcoa_fill_col)),
                                   show.legend = FALSE)
      }
      
      
      
      # Taxon points
      
      if (dim(taxon_weighted_scores)[1] != 0) {
        pcoa_plot = pcoa_plot + geom_point(
          data = taxon_weighted_scores,
          aes(Axis1, Axis2, size = round(Abundance *
                                           100, digits = 0)),
          inherit.aes = FALSE,
          shape = 21,
          fill = NA,
          colour = "black",
          show.legend = TRUE
        ) +
          labs(size = "Abundance")
      }
      
      if (dim(taxon_weighted_scores)[1] != 0) {
        # Add taxon annotation
        pcoa_plot <- pcoa_plot + geom_text(
          data = taxon_weighted_scores,
          aes(Axis1, Axis2, label = rownames(taxon_weighted_scores)),
          inherit.aes = FALSE,
          size = 4
        )
      }
      
      
      
      # Customize plot aesthetics
      pcoa_plot <- pcoa_plot +
        theme(
          panel.grid = element_blank(),
          text = element_text(colour = "black"),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(colour = "black", size = 12),
          axis.text.x = element_text(
            angle = 90,
            hjust = 1,
            vjust = 0.5,
            size = 14,
            face = "plain"
          ),
          axis.text.y.left = element_text(size = 14, face = "plain"),
          legend.text = element_text(face = "italic", size = 16),
          legend.title = element_text(size = 16),
          axis.title = element_text(size = 16, face = NULL),
          axis.text.y = element_text(size = 14),
          strip.text.x = element_text(size = 10, face = "bold"),
          panel.spacing = unit(0, "lines"),
          panel.border = element_rect(
            colour = "black",
            size = 1,
            fill = NA
          ),
          axis.ticks = element_line(colour = "black"),
          axis.ticks.y = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black")
        )
      
      # Plot PCoA
      pcoa_plot
    })
    
    
    ## Adjusting the PCoA image and saving
    ## You must define the input for width/height within a reactive context, then call it in the output.
    pcoa_plot_width <- reactive(input$pcoa_plot_outw)
    pcoa_plot_height <- reactive(input$pcoa_plot_outh)
    
    output$pcoa_plot_out <- renderPlot({
      pcoa_plot <- pcoa_plot_react()
      pcoa_plot
    },
    width = pcoa_plot_width,
    height = pcoa_plot_height)
    
  output$pcoa_download <- downloadHandler(
    filename = "pcoa_plot.pdf",
    contentType = ".pdf",
    content = function(pcoa_file) {
      ggsave(
        pcoa_file,
        plot = pcoa_plot_react(),
        device = "pdf",
        height = as.numeric(input$pcoa_plot_outh),
        width = as.numeric(input$pcoa_plot_outw),
        units = "px",
        scale = 4
      )
    }
  )
  
  
  
  
  
  #### Unifrac PCoA Triplot ####
  
  # Right now, this is limited to using the distance matrix output from QIIME2, which means the uses must calculate run diversity-core-phylogentetics
  # and then export the distance matrix. It also cannot include taxonomy because of the nature of the files in the QIIME2 environment. We'll see if this can be
  # fixed in the future. Perhaps a workaround. 
  
  

  ## We start with an input upload of the distance matrix and the metadata. It is best to keep this separate from the metadata table used throughout BLOOP
  ## Main ASV table
  # output$main_table <- renderDataTable({
  #   req(input$unifrac_file)
  #   read.table(
  #     file = input$unifrac_file$datapath,
  #     fill = TRUE,
  #     header = TRUE,
  #     sep = "\t"
  #   )
  # })
  
  unifrac_datafile_upload <- reactive({
    req(input$unifrac_file)
    read.table(
      file = input$unifrac_file$datapath,
      fill = TRUE,
      header = TRUE,
      sep = "\t"
    )
  })
  
  # main_datafile_og <- reactive({
  #   main_datafile = main_datafile_upload()
  #   main_datafile[is.na(main_datafile)] <- 0
  #   main_datafile
  # })
  
  ## Metadata table
  uni_meta_datafile_og <- reactive({
    req(input$uni_meta_file)
    read.table(
      file = input$uni_meta_file$datapath,
      fill = TRUE,
      header = TRUE,
      sep = "\t"
    )
  })
  
  

  
  
#  observeEvent(input$uni_pcoa_start, {
  unifrac_distance_re <- reactive({
    main_data_table <- unifrac_datafile_upload()
    meta_data_table <- uni_meta_datafile_og()
    
    names(main_data_table)[1] = "SampleName"
    rownames(main_data_table) = main_data_table$SampleName
    meta_names <-
      c(
        meta_data_table$SampleName,
        "Consensus.Lineage",
        "rowID",
        "Feature.ID",
        "ReprSequence"
      )
    
    
    ## Remove columns and rows of samples not present in metadata -- when there is more data than metadata
    main_data_table = main_data_table[,names(main_data_table) %in% meta_names]
    main_data_table = main_data_table %>% filter(rownames(main_data_table) %in% meta_names)
    main_data_table
  })
  
  uni_meta_datafile_re <- reactive({
    meta_data_table <- uni_meta_datafile_og()
    main_data_table <- unifrac_distance_re()
    
    ## Filter for samples only present in the metadata file:
    main_colnames = colnames(main_data_table)
    main_rownames = rownames(main_data_table)
    meta_names = meta_data_table$SampleName
    
    ## Remove metadata when there is more metadata than data:
    meta_data_table = meta_data_table %>% filter(SampleName %in% main_colnames)
    meta_data_table
  })
  
  
  uni_pcoa_react <- reactive({
    pcoa_srs_diss <- unifrac_distance_re()
    ##Generate the PCOA plot using the APE package
    pcoa_result <- ape::pcoa(pcoa_srs_diss, correction = "cailliez", )
    pcoa_result
    
  })
  
  uni_pcoa_coords_react <- reactive({
    pcoa_result <- uni_pcoa_react()
    
    # Extract PCoA coordinates
    pcoa_coords <- pcoa_result$vectors[, 1:2]
    # Create a dataframe with PCoA coordinates and row names
    pcoa_df <-
      data.frame(
        PCoA1 = pcoa_coords[, 1],
        PCoA2 = pcoa_coords[, 2],
        row.names = row.names(pcoa_coords)
      )
    pcoa_df
  })
  
  uni_pcoa_eigen_react <- reactive({
    pcoa_result <- uni_pcoa_react()
    
    # Extract relative eigenvalues
    eigenvalues <- pcoa_result$values$Rel_corr_eig
    
    # Extract the first two eigenvalues
    Axis1 <- eigenvalues[1] * 100
    Axis2 <- eigenvalues[2] * 100
    eigen_df <- data.frame(Axis1 = Axis1, Axis2 = Axis2)
    eigen_df
  })
  
  uni_pcoa_envfit_react <- reactive({
    pcoa_result <- uni_pcoa_react()
    meta_data_table <- uni_meta_datafile_re()

        ## Process for all the environment variables for triplot and separate the vectors and R2 ##
    # pcoa_test = cmdscale(pcoa_srs_diss, k=3, eig = TRUE)
    # This must be corrected so that any samples removed in the SRS correction are removed from the metadata table!
    pcoa_envfit <- envfit(pcoa_result$vectors, meta_data_table, perm = 10000)
    
    ## Scales the arrow vectors so they aren't huge
    pcoa_envfit_df <- as.data.frame(pcoa_envfit$vectors$arrows * sqrt(pcoa_envfit$vectors$r))
    pcoa_envfit_df <- cbind(pcoa_envfit_df, pcoa_envfit$vectors$r)
    pcoa_envfit_df <- cbind(pcoa_envfit_df, pcoa_envfit$vectors$pvals)
    colnames(pcoa_envfit_df) <- c("axis1", "axis2", "R", "pvalue")
    
    # # Filter below a specified threshold (p-value?). Need to look more into what these axis values represent.
    # pcoa_envfit_df = filter(pcoa_envfit_df, pcoa_envfit_df$R < 0.5)
    pcoa_envfit_df_filt <- filter(pcoa_envfit_df,
                                  pcoa_envfit_df$pvalue < input$uni_env_thresh)
    pcoa_envfit_df_filt
    
  })
  
  # pcoa_plot_taxa_fit_react <- reactive({
  #   pcoa_srs <- srs_proportion_react()
  #   pcoa_srs_plot <- bc_pcoa_react()
  #   
  #   # Generate the weighted average scores
  #   taxon_weighted_scores <-
  #     wascores(pcoa_srs_plot$vectors[, 1:3], pcoa_srs)
  #   
  #   taxon_weighted_scores[is.na(taxon_weighted_scores)] <- 0 # remove NA values
  #   
  #   # Calculate normalized, total abundance of each taxa
  #   taxa_total_abundance <-
  #     sum(pcoa_srs) #total number of asvs in the table
  #   taxa_count <-
  #     apply(pcoa_srs, 2, sum) #total number of each asv across all samples
  #   normalized_taxa_count <-
  #     as.data.frame(taxa_count / taxa_total_abundance)
  #   colnames(normalized_taxa_count) <- "abundance"
  #   
  #   taxon_weighted_scores <-
  #     as.data.frame(cbind(
  #       taxon_weighted_scores,
  #       normalized_taxa_count$abundance
  #     )) #Append the abundance information
  #   colnames(taxon_weighted_scores) <-
  #     c("Axis1", "Axis2", "Axis3", "Abundance") # Change column names
  #   # taxon_weighted_scores$abundance <- normalized_taxa_count # Append the counts to the weighted average scores
  #   # taxon_weighted_scores <- subset(taxon_weighted_scores, taxon_weighted_scores$Abundance %in% taxon_weighted_scores$Abundance[taxon_weighted_scores$Abundance > abund_thresh])
  #   taxon_weighted_scores <-
  #     filter(
  #       taxon_weighted_scores,
  #       taxon_weighted_scores$Abundance > input$pcoa_taxa_thresh / 100
  #     ) # Filter taxonomy abundance based on a threshold
  #   taxon_weighted_scores
  #   
  #   
  # })
  
 observeEvent(input$uni_pcoa_start, {
  uni_pcoa_plot_react <- reactive({
    ## I need to modify this so if the dataframe is empty it doesn't include the environmental fit data
    
    pcoa_df <- uni_pcoa_coords_react()
    meta_data_table <- uni_meta_datafile_re()
    pcoa_envfit_df_filt <- uni_pcoa_envfit_react()
    eigen_df <- uni_pcoa_eigen_react()
    # taxon_weighted_scores <- pcoa_plot_taxa_fit_react()
    
    # Insert a column of the sample names
    pcoa_df$SampleName <- rownames(pcoa_df)
    # Merge metadata with PCoA dataframe based on row names
    merged_df <-
      left_join(pcoa_df, meta_data_table, by = "SampleName")
    # Change all metadata into categorical data
    merged_df <-
      merged_df %>% mutate_if(!names(.) %in% c("PCoA1", "PCoA2"), factor)
    
    # Define the available shapes and colors
    available_shapes <- c(21, 22, 23, 24, 14, 13:1)
    available_colors <- 2:27
    available_fill <- 2:27
    
    
    
    if (input$uni_shape_choice == TRUE){
      # Plot PCoA using ggplot2 with shape and color based on metadata
      pcoa_plot <- ggplot(merged_df, aes(x = PCoA1, y = PCoA2)) +
        # geom_point(size = 4, aes(shape = eval(parse(text = )), fill = get(input$pcoa_fill_col))) +
        
        geom_point(size = input$uni_pcoa_size_select, aes(
          # shape = eval(parse(text = paste("merged_df$",input$pcoa_shape))),
          # colour = eval(parse(text = paste("merged_df$",input$pcoa_fill_col))),
          # fill = eval(parse(text = paste("merged_df$",input$pcoa_fill_col))))) +
          shape = get(input$uni_pcoa_shape),
          colour = get(input$uni_pcoa_fill_col),
          fill = get(input$uni_pcoa_fill_col)
        )) +
        # (eval(parse(text=paste("data_long_bubble$",input$b1_meta_group)))))
        # geom_point(size = 4, aes(shape = get(input$pcoa_shape), fill = get(input$pcoa_fill_col))) +
        labs(
          x = paste(
            "Axis1 variance (",
            round(eigen_df$Axis1, 1),
            "%",
            ")",
            sep = ""
          ),
          y = paste(
            "Axis2 variance (",
            round(eigen_df$Axis2, 1),
            "%",
            ")",
            sep = ""
          ),
          fill = input$uni_pcoa_fill_col,
          colour = input$uni_pcoa_fill_col,
          shape = input$uni_pcoa_shape
        ) +
        # scale_shape_manual(values = available_shapes, name = paste("Data",eval(parse(text = input$pcoa_shape)),sep = "")) +
        # scale_fill_manual(values = available_fill, name =  paste("Data2",eval(parse(text = input$pcoa_fill_col)),sep = "")) +
        # scale_color_manual(values = available_colors, name = "colour", guide = "none")
        scale_shape_manual(values = available_shapes) +
        scale_fill_manual(values = available_fill) +
        scale_colour_manual(values = available_fill)
      # scale_colour_manual(values = available_colors, name = "colour", guide = "none")
    }
    
    if (input$uni_shape_choice == FALSE){
      pcoa_plot <- ggplot(merged_df, aes(x = PCoA1, y = PCoA2)) +
        # geom_point(size = 4, aes(shape = eval(parse(text = )), fill = get(input$pcoa_fill_col))) +
        geom_point(size = input$uni_pcoa_size_select, aes(
          # shape = eval(parse(text = paste("merged_df$",input$pcoa_shape))),
          # colour = eval(parse(text = paste("merged_df$",input$pcoa_fill_col))),
          # fill = eval(parse(text = paste("merged_df$",input$pcoa_fill_col))))) +
          colour = get(input$uni_pcoa_fill_col),
          fill = get(input$uni_pcoa_fill_col)
        )) +
        # (eval(parse(text=paste("data_long_bubble$",input$b1_meta_group)))))
        # geom_point(size = 4, aes(shape = get(input$pcoa_shape), fill = get(input$pcoa_fill_col))) +
        labs(
          x = paste(
            "Axis1 variance (",
            round(eigen_df$Axis1, 1),
            "%",
            ")",
            sep = ""
          ),
          y = paste(
            "Axis2 variance (",
            round(eigen_df$Axis2, 1),
            "%",
            ")",
            sep = ""
          ),
          fill = input$uni_pcoa_fill_col,
          colour = input$uni_pcoa_fill_col
        ) +
        # scale_shape_manual(values = available_shapes, name = paste("Data",eval(parse(text = input$pcoa_shape)),sep = "")) +
        # scale_fill_manual(values = available_fill, name =  paste("Data2",eval(parse(text = input$pcoa_fill_col)),sep = "")) +
        # scale_color_manual(values = available_colors, name = "colour", guide = "none")
        scale_fill_manual(values = available_fill) +
        scale_colour_manual(values = available_fill)
    }
    
    
    
    if (dim(pcoa_envfit_df_filt)[1] != 0) {
      ## Add triplot info and labels: ## Add if statement here
      pcoa_plot <- pcoa_plot + geom_segment(
        data = pcoa_envfit_df_filt,
        aes(
          x = 0,
          y = 0,
          xend = pcoa_envfit_df_filt$axis1,
          yend = pcoa_envfit_df_filt$axis2,
        ),
        show.legend = FALSE,
        arrow = arrow(ends = "last")
      ) +
        geom_label(
          data = pcoa_envfit_df_filt,
          aes(
            label = rownames(pcoa_envfit_df_filt),
            x = pcoa_envfit_df_filt$axis1 / 2,
            y = pcoa_envfit_df_filt$axis2 / 2
          ),
          size = 4
        )
    }
    
    
    # If sample labels are selected:
    if (input$pcoa_sample_labels == TRUE){
      pcoa_plot <- pcoa_plot +
        geom_text(aes(label = SampleName))
    }
    
    # Do you want to include an ellipsis around your data points?:
    if (input$uni_pcoa_elips == TRUE) {
      pcoa_plot <-
        pcoa_plot + stat_ellipse(aes(color = get(input$uni_pcoa_fill_col)),
                                 show.legend = FALSE)
    }

    
    
    # # Taxon points
    # 
    # if (dim(taxon_weighted_scores)[1] != 0) {
    #   pcoa_plot = pcoa_plot + geom_point(
    #     data = taxon_weighted_scores,
    #     aes(Axis1, Axis2, size = round(Abundance *
    #                                      100, digits = 0)),
    #     inherit.aes = FALSE,
    #     shape = 21,
    #     fill = NA,
    #     colour = "black",
    #     show.legend = TRUE
    #   ) +
    #     labs(size = "Abundance")
    # }
    # 
    # if (dim(taxon_weighted_scores)[1] != 0) {
    #   # Add taxon annotation
    #   pcoa_plot <- pcoa_plot + geom_text(
    #     data = taxon_weighted_scores,
    #     aes(Axis1, Axis2, label = rownames(taxon_weighted_scores)),
    #     inherit.aes = FALSE,
    #     size = 4
    #   )
    # }
    # 
    
    
    # Customize plot aesthetics
    pcoa_plot <- pcoa_plot +
      theme(
        panel.grid = element_blank(),
        text = element_text(colour = "black"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 14,
          face = "plain"
        ),
        axis.text.y.left = element_text(size = 14, face = "plain"),
        legend.text = element_text(face = "italic", size = 16),
        legend.title = element_text(size = 16),
        axis.title = element_text(size = 16, face = NULL),
        axis.text.y = element_text(size = 14),
        strip.text.x = element_text(size = 10, face = "bold"),
        panel.spacing = unit(0, "lines"),
        panel.border = element_rect(
          colour = "black",
          size = 1,
          fill = NA
        ),
        axis.ticks = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black")
      )
    
    # Plot PCoA
    pcoa_plot
  })
  

  
  
  
  ## Adjusting the PCoA image and saving
  ## You must define the input for width/height within a reactive context, then call it in the output.
  uni_pcoa_plot_width <- reactive(input$uni_pcoa_plot_outw)
  uni_pcoa_plot_height <- reactive(input$uni_pcoa_plot_outh)
  
  output$uni_pcoa_plot_out <- renderPlot({
    pcoa_plot <- uni_pcoa_plot_react()
    pcoa_plot
  },
  width = uni_pcoa_plot_width,
  height = uni_pcoa_plot_height)
  
 })
  
  output$uni_pcoa_download <- downloadHandler(
    filename = "uni_pcoa_plot.pdf",
    contentType = ".pdf",
    content = function(pcoa_file) {
      ggsave(
        pcoa_file,
        plot = uni_pcoa_plot_react(),
        device = "pdf",
        height = as.numeric(input$uni_pcoa_plot_outh),
        width = as.numeric(input$uni_pcoa_plot_outw),
        units = "px",
        scale = 4
      )
    }
  )

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #### Ranked abundance plot ####
  # For this plot, I think I need to generate all of the data again, including the taxonomy labels. I think I can use the prop table or filtered table from previously
  
  ranked_abundance_table_re <- reactive({
    meta_data_table <- meta_datafile()
    unfiltered_table <- data_unfiltered_table()
    filtered_table <- data_filtered_table()
    
    
    # # ## This removes samples from the ASV tables that don't have a corresponding metadata row: THIS IS WORKING
    # meta_samplist = meta_data_table$SampleName
    # filtered_table = filtered_table[,names(filtered_table) %in% meta_samplist]
    
    ## Initial transformations:
    ## Produce a prop table and filter it to a specific threshold
    
    # Set dataframe
    
    # Function to find highest values representing 90% of all values in a column
    find_top_values <- function(column) {
      sorted_column <-
        sort(column, decreasing = TRUE)  # Sort column in descending order
      threshold <-
        sum(column) * input$ranked_threshold  # Calculate the threshold representing 90% of all values
      cumsum_values <-
        cumsum(sorted_column)  # Cumulative sum of sorted values
      top_values <-
        sorted_column[cumsum_values <= threshold]  # Filter values below the threshold
      return(top_values)
    }
    
    # Create a new dataframe to store the top values
    top_values_df <- filtered_table
    
    # Find top values representing 90% of all values in each column and populate the new dataframe
    for (col_idx in 1:ncol(filtered_table)) {
      col_name <- names(filtered_table)[col_idx]
      col_values <- filtered_table[[col_name]]
      top_values <- find_top_values(col_values)
      top_values_df[[col_name]] <-
        ifelse(col_values %in% top_values, col_values, 0)
    }
    filtered_table <- top_values_df
    
    # # Tally the number of counts >0 in each column and add it as a new row
    # tallied_values <- colSums(top_values_df > 0)
    # df_with_sums_alt <- rbind(top_values_df, tallied_values)
    # # Add a row with column sums of the new table
    # df_with_sums_alt <- rbind(df_with_sums_alt, colSums(top_values_df))
    # # Add a row with column sums of the original table
    # df_with_sums_alt <- rbind(df_with_sums_alt, colSums(df))
    # # Assign row names to the last two rows
    # row.names(df_with_sums_alt)[nrow(df_with_sums_alt) - 2] <- "Number of groups"
    # row.names(df_with_sums_alt)[nrow(df_with_sums_alt) - 1] <- "New total"
    # row.names(df_with_sums_alt)[nrow(df_with_sums_alt)] <- "Original total"
    #
    filtered_table
  })
  
  ranked_abundance_bubble_re = reactive({
    filtered_table = ranked_abundance_table_re()
    meta_data_table = meta_datafile()
    unfiltered_table = data_unfiltered_table()
    
    
    B2_data_prop <- prop.table(as.matrix(filtered_table), 2) * 100
    B2_data_prop <- as.data.frame(B2_data_prop)
    
    ## If the table contains any columns with zero reads, the column reports NA, and the script fails after this point.
    ## These columns must be removed first.
    B2_data_prop = B2_data_prop[, colSums(is.na(B2_data_prop)) == 0]
    
    ## This checks all columns for a value above a threshold and keeps all rows if found. However, this screws up downstream metadata filtering, so I need to figure out how to change this. During the metadata step, any samples without representation in the metadata table will be removed. If those removed samples contained reads above a threshold, the remaining samples will stay in the filtered table (and they should not).
    
    ## For ranked abundance, this should remain commented. We don't want to layer on thresholds (i.e., >1% of the >90% of all reads)
    # B2_data_prop = B2_data_prop %>% filter_all(any_vars(.>= as.numeric(input$b1_ab_thresh)))
    
    
    ## Reassigns taxonomy to the filtered table
    B2_data_prop$Taxonomy <- rownames(B2_data_prop)
    B2_data_prop$TaxaName <- rownames(B2_data_prop)
    
    ## Collect the ASV IDs before mutating labels:
    B2_rowIDs <- as.data.frame(gsub(".*_", "", B2_data_prop$Taxonomy))
    colnames(B2_rowIDs) <- "rowIDs"
    
    ## Right now I have to redo the labels since I haven't figured out a way to exclude them from filter step. Until then:
    ## Cleans up labels and removes uncultured/ambiguous taxa
    B2_labels <- B2_data_prop$Taxonomy
    B2_labels <- gsub("_[0-9]*$", "", B2_labels, )
    B2_labels <-
      gsub("(;Ambiguous__taxa)", ";s__Ambiguous_taxa", B2_labels)
    B2_labels <-
      gsub("(;Ambiguous_taxa)", ";s__Ambiguous_taxa", B2_labels)
    B2_labels <- gsub(" ", "", B2_labels)
    
    if (input$truncate_taxa == "Yes") {
      B2_labels <- paste(";", sep = "", B2_labels)
      B2_labels <- gsub("(;\\s*Ambiguous_taxa)", "", B2_labels)
      B2_labels <- gsub("(uncultured.*)", "", B2_labels)
      B2_labels <- gsub("(__uncultured.*)", "", B2_labels)
      B2_labels <- gsub("(unidenti.*)", "", B2_labels)
      B2_labels <- gsub("(__unidenti.*)", "", B2_labels)
      B2_labels <- gsub("(;.__Ambiguous_taxa)", "", B2_labels)
      B2_labels <- gsub("(;._Ambiguous_taxa)", "", B2_labels)
      B2_labels <- gsub("(;s__$)", "", B2_labels)
      B2_labels <- gsub("(;g__$)", "", B2_labels)
    }
    
    ## Remove taxa pre-fixes associated with SILVA classifier (old):
    if (input$remove_prefix == "Yes") {
      B2_labels <- gsub("(D_.__)", "", B2_labels)
      B2_labels <- gsub(";$", "", B2_labels)
    }
    B2_labels <- gsub("(;D_.__$)", "", B2_labels)
    
    
    ## Remove taxa-prefixed associated with SILVA classifier (new):
    if (input$remove_prefix == "Yes") {
      B2_labels <- gsub("(d__)", "", B2_labels)
      B2_labels <- gsub("(p__)", "", B2_labels)
      B2_labels <- gsub("(c__)", "", B2_labels)
      B2_labels <- gsub("(o__)", "", B2_labels)
      B2_labels <- gsub("(f__)", "", B2_labels)
      B2_labels <- gsub("(g__)", "", B2_labels)
      B2_labels <- gsub("(s__)", "", B2_labels)
      B2_labels <- gsub(" ", "", B2_labels)
      B2_labels <- gsub("(;metagenome$)", "", B2_labels)
      B2_labels <- gsub("(__.$)", "", B2_labels)
      B2_labels <- gsub("(;__)", "", B2_labels)
      B2_labels <- gsub("(;$)", "", B2_labels)
      B2_labels <-
        gsub("(;$)", "", B2_labels) #This is not a duplicate -- leave it here
    }
    
    ## Further refining labels.
    B2_labels <- gsub(" ", "", B2_labels)
    B2_labels <- gsub("(__.$)", "", B2_labels)
    B2_labels <- gsub("(;$)", "", B2_labels)
    B2_labels <- gsub("(;__)", "", B2_labels)
    #B2_labels<-gsub("(_)","",B2_labels)
    B2_labels <- gsub(".*;", "", B2_labels)
    B2_labels <-
      paste(gsub(".*;$", "", B2_labels), B2_rowIDs$rowIDs, sep = "_")
    B2_labels <- as.data.frame(B2_labels, colnames("labels"))
    colnames(B2_labels) <- "labels"
    #B2_labels<-gsub("([:digit:]$)","_",B2_labels)
    
    
    ## Apply the labels to the new proportion table:
    B2_data_prop$TaxaName <- B2_labels$labels
    rownames(B2_data_prop) <- B2_data_prop$TaxaName
    
    
    # data_tran$rowID = 1:nrow(data_tran)
    # data_tran$Taxonomy = paste(data_tran$Consensus.Lineage,data_tran$rowID, sep)
    # colnames(data_tran) = c("Taxonomy","ReprSequence")
    
    
    # data_tran$Taxonomy = as.character(data_tran$Taxonomy)
    # data_tran$ReprSequence = as.character(data_tran$ReprSequence)
    
    # B2_data_prop$Taxonomy = as.character(B2_data_prop$Taxonomy)
    
    
    # B2_data_prop$Taxonomy = as.factor(B2_data_prop$Taxonomy)
    
    ## Transform into long form:
    B2_data_long <-
      reshape2::melt(
        B2_data_prop,
        id.vars = c("TaxaName", "Taxonomy"),
        variable.name = as.character("SampleName"),
        value.name = "Percentage"
      )
    
    
    ## Add representative sequences back into the data by merging tables based on taxonomy. If collapsed, skips.
    unfiltered_table$Taxonomy = rownames(unfiltered_table)
    if (input$is_main_collapsed == FALSE) {
      unfiltered_table = unfiltered_table[, c("Taxonomy", "ReprSequence")]
    }
    B2_data_long = left_join(B2_data_long,
                             unfiltered_table,
                             by = "Taxonomy",
                             copy = TRUE)
    
    
    ## Modify taxonomy names
    B2_data_long$Taxonomy <- gsub("(D_.__)", "", B2_data_long$Taxonomy)
    B2_data_long$Taxonomy <- gsub(";$", "", B2_data_long$Taxonomy)
    
    
    
    
    ## Filter above a threshold, append metadata, and round decimals -- I think this needs to be moved after the filtering
    
    data_long_bubble <- B2_data_long
    data_long_bubble <-
      dplyr::filter(data_long_bubble, Percentage > 0)
    data_long_bubble <-
      left_join(data_long_bubble,
                meta_data_table,
                by = "SampleName",
                copy = TRUE)
    
    
    
    ## Round percentages to a given decimal places
    data_long_bubble$Percentage <-
      round(data_long_bubble$Percentage, digits = as.numeric(input$b1_num_dec))
    
    
    
    
    ## Include only specific taxa  within the plot
    if (is.na(input$ranked_tax_keyword) == TRUE) {
      warning("No specific taxon selected. Script will continue without filtering by taxa.")
    } else {
      taxon_hits <-
        grepl(
          pattern = paste(input$ranked_tax_keyword),
          ignore.case = TRUE,
          x = data_long_bubble$Taxonomy
        )
      data_long_bubble <- data_long_bubble[taxon_hits, ]
      warning("Taxa filtering selected.")
    }
    
    ## Include only specific sample groups within the plot -- This has been moved to the beginning so it works properly.
    # if (is.na(input$b1_meta_group) == TRUE){
    #   warning("No metadata category selected. Script will continue without filtering by groups.")
    # } else {
    #   sample_hits <- grepl(pattern = input$b1_meta_keyword, ignore.case = TRUE, x = (eval(parse(text=paste("data_long_bubble$",input$b1_meta_group)))))
    #   data_long_bubble <- data_long_bubble[sample_hits,]
    #   warning("Metadata filtering selected.")
    # }
    #
    ## Attach full taxonomic lineage and separate into classifications, and fix labelling issues:
    data_long_bubble$Sep = data_long_bubble$Taxonomy
    data_long_bubble$Sep <- gsub("(d__)", "", data_long_bubble$Sep)
    data_long_bubble$Sep <- gsub("(p__)", "", data_long_bubble$Sep)
    data_long_bubble$Sep <- gsub("(c__)", "", data_long_bubble$Sep)
    data_long_bubble$Sep <- gsub("(o__)", "", data_long_bubble$Sep)
    data_long_bubble$Sep <- gsub("(f__)", "", data_long_bubble$Sep)
    data_long_bubble$Sep <- gsub("(g__)", "", data_long_bubble$Sep)
    data_long_bubble$Sep <- gsub("(s__)", "", data_long_bubble$Sep)
    data_long_bubble <-
      separate(
        data_long_bubble,
        Sep,
        c(
          "Domain",
          "Phylum",
          "Class",
          "Order",
          "Family",
          "Genus",
          "Species"
        ),
        sep = ";",
        remove = TRUE,
        convert = FALSE
      )
    
    # Set the taxonomy as factors for later ordering. This doesn't do much, but if you remove it the colouring settings change
    if (isTRUE(input$b1_confirm_sort == "Yes")) {
      data_long_bubble <-
        data_long_bubble[with(data_long_bubble,
                              order(eval(parse(
                                text = input$b1_tax_sort
                              )), TaxaName, decreasing = TRUE)), ]
      data_long_bubble$TaxaName <-
        as.character(data_long_bubble$TaxaName)
      data_long_bubble$TaxaName <-
        factor(data_long_bubble$TaxaName,
               levels = unique(data_long_bubble$TaxaName))
      data_long_bubble
    }
    data_long_bubble
  })
  
  ranked_abundance_bubble_plot_re = reactive({
    data_long_bubble <- ranked_abundance_bubble_re()
    
    ## Plot the Data:
    bubble_plot <- ggplot(
      data_long_bubble,
      aes(
        x = reorder(SampleName, eval(parse(
          text = input$ranked_sort_axis
        ))),
        
        # aes(x = reorder_within(x = SampleName, by = eval(parse(text=input$b1_sort_axis)),within = eval(parse(text=input$b1_sort_param))),
        
        # x = if(!is.na(input$b1_sort_param)){
        #   x = reorder_within(x = SampleName, by = eval(parse(text=input$b1_sort_axis)),within = eval(parse(text=input$b1_sort_param)))
        # } else {
        #   x = SampleName
        # },
        
        if (input$ranked_confirm_sort == "Yes") {
          y = TaxaName
        } else {
          y = reorder(TaxaName, desc(TaxaName))
        },
        fill = as.factor(get(input$ranked_color_param)),
        color = as.factor(get(input$ranked_color_param)),
        size = Percentage
      ),
      colsep = c(1:100),
      rowsep = (1:100),
      sepwidth = c(5, 1)
    ) + guides(fill = 'none', colour = 'none')
    
    #     if(input$ranked_confirm_sort == "Yes"){
    #       y = TaxaName
    #     } else {y = reorder(TaxaName,desc(TaxaName))},
    #     fill = as.factor(eval(parse(text=input$ranked_color_param))),
    #     color = as.factor(eval(parse(text=input$ranked_color_param))), size = Percentage),
    # colsep=c(1:100), rowsep=(1:100), sepwidth=c(5,1))
    #
    ## Sorting the y-axis and faceting options
    
    
    
    if (input$ranked_second_facet == "No") {
      bubble_plot = bubble_plot +
        
        if (input$ranked_confirm_sort == "Yes") {
          if ((input$ranked_facet_side_x == "Top") &
              (input$ranked_facet_side_y == "Right")) {
            facet_nested(eval(parse(text = input$ranked_tax_sort)) ~ eval(parse(text =
                                                                                  input$ranked_sort_param)),
                         space = "free",
                         scales = "free")
          } else {
            if ((input$ranked_facet_side_x == "Bottom") &
                (input$ranked_facet_side_y == "Left")) {
              facet_nested(
                eval(parse(text = input$ranked_tax_sort)) ~ eval(parse(text = input$ranked_sort_param)),
                space = "free",
                scales = "free",
                switch = "both"
              )
            } else {
              if ((input$ranked_facet_side_x == "Bottom") &
                  (input$ranked_facet_side_y == "Right")) {
                facet_nested(
                  eval(parse(text = input$ranked_tax_sort)) ~ eval(parse(text = input$ranked_sort_param)),
                  space = "free",
                  scales = "free",
                  switch = "x"
                )
              } else {
                facet_nested(
                  eval(parse(text = input$ranked_tax_sort)) ~ eval(parse(text = input$ranked_sort_param)),
                  space = "free",
                  scales = "free",
                  switch = "y"
                )
              }
            }
          }
        } else {
          if (input$ranked_facet_side_x == "Top") {
            facet_nested( ~ eval(parse(text = input$ranked_sort_param)), space = "free", scales =
                            "free")
          } else {
            facet_nested(
              ~ eval(parse(text = input$ranked_sort_param)),
              space = "free",
              scales = "free",
              switch = "x"
            )
          }
        }
    }
    
    
    
    if (input$ranked_second_facet == "Yes") {
      bubble_plot = bubble_plot +
        
        
        if (input$ranked_confirm_sort == "Yes") {
          if ((input$ranked_facet_side_x == "Top") &
              (input$ranked_facet_side_y == "Right")) {
            facet_nested(
              eval(parse(text = input$ranked_tax_sort)) ~ eval(parse(text = input$ranked_sort_param)) +
                eval(parse(
                  text = input$ranked_second_facet_meta
                )),
              space = "free",
              scales = "free"
            )
          } else {
            if ((input$ranked_facet_side_x == "Bottom") &
                (input$ranked_facet_side_y == "Left")) {
              facet_nested(
                eval(parse(text = input$ranked_tax_sort)) ~ eval(parse(text = input$ranked_sort_param)) +
                  eval(parse(
                    text = input$ranked_second_facet_meta
                  )),
                space = "free",
                scales = "free",
                switch = "both"
              )
            } else {
              if ((input$ranked_facet_side_x == "Bottom") &
                  (input$ranked_facet_side_y == "Right")) {
                facet_nested(
                  eval(parse(text = input$ranked_tax_sort)) ~ eval(parse(text = input$ranked_sort_param)) +
                    eval(parse(
                      text = input$ranked_second_facet_meta
                    )),
                  space = "free",
                  scales = "free",
                  switch = "x"
                )
              } else {
                facet_nested(
                  eval(parse(text = input$ranked_tax_sort)) ~ eval(parse(text = input$ranked_sort_param)) +
                    eval(parse(
                      text = input$ranked_second_facet_meta
                    )),
                  space = "free",
                  scales = "free",
                  switch = "y"
                )
              }
            }
          }
        } else {
          if (input$ranked_facet_side_x == "Top") {
            facet_nested( ~ eval(parse(text = input$ranked_sort_param)) + eval(parse(
              text = input$ranked_second_facet_meta
            )),
            space = "free",
            scales = "free")
          } else {
            facet_nested(
              ~ eval(parse(text = input$ranked_sort_param)) + eval(parse(
                text = input$ranked_second_facet_meta
              )),
              space = "free",
              scales = "free",
              switch = "x"
            )
          }
        }
    }
    
    ## Add the bubbles and percentage labels to the plot:
    
    if (input$ranked_incl_percent == "Yes") {
      bubble_plot <- bubble_plot +
        geom_point(shape = 21, alpha = 0.8) +
        geom_text(aes(label = Percentage),
                  colour = "black",
                  size = 3.0) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_size_area(max_size = 15) +
        ggtitle("") + xlab("") +
        scale_size_area(max_size = 15) +
        ylab(
          paste(
            "Taxonomic affiliation",
            " ",
            "at",
            " ",
            ">",
            eval(parse(text = input$b1_ab_thresh)),
            "%",
            " ",
            "relative",
            " ",
            "abundance",
            sep = ""
          )
        )
    } else {
      bubble_plot <- bubble_plot +
        geom_point(shape = 21, alpha = 0.8) +
        ggtitle("") + xlab("") +
        scale_size_area(max_size = 15) +
        ylab(
          paste(
            "Taxonomic affiliation",
            " ",
            "at",
            " ",
            ">",
            eval(parse(text = input$b1_ab_thresh)),
            "%",
            " ",
            "relative",
            " ",
            "abundance",
            sep = ""
          )
        )
    }
    
    ## Modify the general theme, including panel borders
    if (input$ranked_panel_border == "Yes") {
      bubble_plot = bubble_plot +
        theme_bw() + theme(
          axis.text = element_text(colour = "black", size = 10),
          axis.line = element_blank(),
          #strip.background.y = element_rect(fill = "white"),
          panel.spacing = unit(as.numeric(input$ranked_panel_spacing), "points"),
          #panel.grid = element_line(colour = "grey"),
          #axis.line.y = element_line(colour="black",size=0.5),
          #panel.border = element_blank(),
          #text = element_text(size=10),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          )
          #legend.position = "none")
        )
    }
    
    ## modify the general theme, removing panel borders
    if (input$ranked_panel_border == "No") {
      bubble_plot = bubble_plot +
        theme_bw() + theme(
          axis.text = element_text(colour = "black", size = 10),
          #strip.background.y = element_rect(fill = "white"),
          panel.spacing = unit(as.numeric(input$ranked_panel_spacing), "points"),
          #panel.grid = element_line(colour = "grey"),
          #axis.line.y = element_line(colour="black",size=0.5),
          panel.border = element_blank(),
          #text = element_text(size=10),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          )
          #legend.position = "none")
        )
    }
    
    #nb.cols <- length(unique(bubble_colour))
    #mycolors <- colorRampPalette(brewer.pal(8, bar_fill_col))(nb.cols)
    ## colour fill using Brewers
    #bubble_plot<-bubble_plot + scale_fill_manual(values = mycolors)
    bubble_plot
    
  })
  
  output$ranked_table_out = renderDataTable({
    table = ranked_abundance_table_re()
    table
  })
  
  ranked_plot_height = reactive(input$ranked_plot_out_h)
  ranked_plot_width = reactive(input$ranked_plot_out_w)
  
  output$ranked_plot_out = renderPlot({
    plot = ranked_abundance_bubble_plot_re()
    plot
  },
  width = ranked_plot_width,
  height = ranked_plot_height)
  
  output$ranked_bubble_download = downloadHandler(
    filename = "ranked_plot.pdf",
    contentType = ".pdf",
    content = function(ranked_file) {
      ggsave(
        ranked_file,
        plot = ranked_abundance_bubble_plot_re(),
        device = "pdf",
        height = as.numeric(input$ranked_plot_out_h),
        width = as.numeric(input$ranked_plot_out_w),
        units = "px",
        scale = 4
      )
    }
  )
  
  
  output$ranked_data_table = downloadHandler(
    filename = "ranked_data_table.csv",
    content = function(ranked_table) {
      write.csv(ranked_abundance_table_re(), ranked_table)
    }
  )
  
  
  #### FAQ PAGE ####
  ## This outputs a pdf file to the browser.
  output$helppdf = renderUI({
    tags$iframe(src = "UA_2022_biblio.pdf", style = "height: 800px; width: 100%")
  })
  
}

## The server launch ## KEEP THIS COMMENTED (unless you need to uncomment it?). In the past, this had to be uncommented to run in RStudio but would crash ShinyServer.
#shinyApp(ui = ui, server = server)