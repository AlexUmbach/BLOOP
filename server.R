########################## SERVER LOGIC ############################
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024 ^ 2) ## This sets the size limit for AOViz, 100 mb right now
  
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
  

  # ## These pass on the metadata column names to all of the drop down menus. They might be causing issues with my start buttons.
  # observe({
  #   req(input$meta_file)
  #   
  #   ## Incorporate the meta file column names into every appropriate input
  #   meta_datafile <- meta_datafile_og()
  #   meta_colnames <- c(colnames(meta_datafile), "TaxaName")
  #   meta_colnames <- meta_colnames[meta_colnames != "SampleName"]
  #   
  #   updateCheckboxInput(session, "remove_low_reads")
  #   
  #   
  #   
  #   ## Update Selection - UniPCoA plot
  #   
  #   
  # })
  
  
  
  
  ######  Upload tab ######
  
  ## Test data upload. To accomplish this, I think I need to duplicate my code for the scenario in which the test button is pressed. 
  # Function to load test data from app www/ directory
  
  ### Test data ###
  ## Main ASV table
  observeEvent(input$test_data,{
    
    main_datafile_upload <- reactive({
      read.table(
        file = "test_ASV.txt",
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
    
    output$main_table <- renderDataTable({
      read.table(
        file = "test_ASV.txt",
        fill = TRUE,
        header = TRUE,
        sep = "\t"
      )
    })
    
    ## Metadata table
    meta_datafile_og <- reactive({
      read.table(
        file = "test_metadata.txt",
        fill = TRUE,
        header = TRUE,
        sep = "\t"
      )
    })
    
    output$meta_table <- renderDataTable({
      read.table(
        file = "test_metadata.txt",
        fill = TRUE,
        header = TRUE,
        sep = "\t"
      )
    })
    
    ## Filter the ASV table based on the present samples in the metadata table
    main_datafile <- reactive({
      # req(input$main_file)
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
      read.table(
        file = "test_contam.txt",
        fill = TRUE,
        header = TRUE,
        sep = "\t"
      )
    })
    
    contam_datafile <- reactive({
      read.table(
        file = "test_contam.txt",
        fill = TRUE,
        header = TRUE,
        sep = "\t"
      )
    })
    
    ## Show the original table that was uploaded
    output$proc_main <- renderDataTable({
      main_datafile <- main_datafile()
      output$proc_maintext <- renderText("This is your original data")
      # req(input$main_file)
      main_datafile
    })
  })
  
  
  
  
  
  ## Main ASV table
  {
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
    
    output$proc_main <- renderDataTable({
      main_datafile <- main_datafile()
      output$proc_maintext <- renderText("This is your original data")
      # req(input$main_file)
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
      # req(input$main_file)
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
    #req(input$main_file)
    data_tran <- main_datafile()
    
    ## Converting collapsed tables to ASV tables, and standardizing formatting, including adding rowID columns
    if (input$is_main_collapsed == TRUE) {
      data_tran$Consensus.Lineage <- data_tran$Feature.ID
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
  
  
  
  ##### Transformed tables (processing tab) #####
  ## This needs to be changed to be only activated once, then again by any subsequent
  
  data_tran_contam_filt_react <- reactive({
    
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
  
  
  data_labels_react <- reactive({
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
    
    ## Retrieves the last taxonomy entry (i.e., genus, species) and preserves the link between featureID and taxonomy:
    feature_taxonomy <- as.data.frame(data_tran$Consensus.Lineage, data_tran$Feature.ID)
    full_lineage <-
      as.data.frame(paste(data_tran$Consensus.Lineage, data_tran$rowID, sep = "_"))
    lineage_OTU <-
      as.data.frame(paste(gsub(".*;", "", labels), data_tran$rowID, sep = "_"))
    colnames(lineage_OTU) <- "TaxaName"
    colnames(full_lineage) <- "TaxaName"
    rownames(data_tran) <- full_lineage$TaxaName
    
    # This final table to track taxonomy and feature IDs. This way I can always map back data based on unique taxon IDs
    feature_taxonomy_labels <- cbind(feature_taxonomy, labels, lineage_OTU)
    feature_taxonomy_labels
  })
  
  data_long_react <- reactive({
    data_tran <- data_tran_contam_filt_react()
    feature_taxonomy_labels <- data_labels_react()
    
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
    
    if (input$remove_low_reads == TRUE){
      filtered_table[filtered_table < input$read_threshold] <- 0
      filtered_table
    }
    
    ## Convert table into % abundance
    data_prop <- filtered_table / colSums(filtered_table)
    
    ## Add full taxonomy into prop table and reassign row names to their truncated names:
    data_prop <- as.data.frame(data_prop)
    data_prop$Taxonomy <- rownames(data_prop)
    rownames(data_prop) <- feature_taxonomy_labels$TaxaName
    
    ## Convert to data frame and change row names to ASVs/Taxa:
    data_prop <- as.data.frame(data_prop)
    data_prop_taxa <- data_prop
    
    ## Add TaxaName column for sorting in later graphs. Also formats the Taxonomy names:
    data_prop$TaxaName <- feature_taxonomy_labels$TaxaName
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
  
  
  
  
  ################# New Bubble table generation ###################
  data_unfiltered_table <- reactive({
    main_datafile <- main_datafile()
    data_tran <- data_tran_contam_filt_react()
    
    # req(input$main_file)
    # req(input$meta_file)
    
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
    
    if (input$remove_low_reads == TRUE){
      filtered_table[filtered_table < input$read_threshold] <- 0
      filtered_table
    }
    filtered_table
  })
  
  data_filtered_table_abridged_names_re <- reactive({
    unfiltered_table <- data_unfiltered_table()
  })
  
  ############ Total read plot  ############ 
  
  ## Read table metadata options
  observeEvent(input$meta_file,{
    req(input$meta_file)
    ## Incorporate the meta file column names into every appropriate input
    meta_datafile <- meta_datafile_og()
    meta_colnames <- colnames(meta_datafile)
    # meta_colnames <- c(colnames(meta_datafile), "TaxaName")
    meta_colnames <- meta_colnames[meta_colnames != "SampleName"]
    ## Update the selections - read plot
    # isolate({
    updateSelectInput(session, "read_sortby_axis", choices = sort(meta_colnames))
    updateSelectInput(session, "read_meta_group", choices = sort(meta_colnames))
    # updateSelectInput(session,"read_meta_key",choices = read_meta_list)
    updateSliderInput(session, "read_plot_out_w")
    # updateSelectInput(session,"read_colour",choices = colnames(meta_datafile()))
    updateSelectInput(session, "read_sortby_axis", choices = sort(meta_colnames))
    updateSelectInput(session, "read_meta_group", choices = sort(meta_colnames))
    updateSliderInput(session, "read_plot_out_w")
    # })
  })
  
  
  #Transform the raw ASV data into counts per sample
  data_read_table <- reactive({
    req(input$read_start)
    # if (read_start()) {
    # isolate({
    
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
    # })
  })
  
  
  #Generate the read plot
  data_read_plot <- reactive({
    req(input$read_start)
    # if (read_start()) {
    # isolate({
    
    data_read_total <- data_read_table()
    
    if (isolate(input$box_select) == "Bar") {
      read_plot <- ggplot(data = data_read_total,
                          aes(
                            x = SampleName,
                            y = Total,
                            width = isolate(input$read_width)
                          ))
      
      read_plot <-
        read_plot + geom_bar(
          # aes(fill = as.factor(
          #   eval(
          #   parse(text = paste(
          #     "data_read_total$", isolate(input$read_sortby_axis)
          #   ))))
          # ),
          fill = "#D3D3D3",
          colour = "black",
          size = 0.4,
          alpha = 0.8,
          stat = "identity",
          position = "stack"
        )
      
      read_plot
    }
    
    if (isolate(input$box_select) == "Box") {
      read_plot <- ggplot(data = data_read_total,
                          #aes(x=as.factor(input$read_sortby_axis), y=Total, width = input$read_width))
                          aes(
                            x = as.factor(isolate(input$read_sortby_axis)),
                            y = Total,
                            width = 2
                          ))
      
      read_plot <- read_plot + geom_boxplot(
        # aes(fill = as.factor(eval(
        #   parse(text = paste(
        #     "data_read_total$", isolate(input$read_sortby_axis)
        #   ))))),
        fill = "#D3D3D3",
        position = "dodge2",
        alpha = 0.8,
        # size = input$read_width,
        size = 0.4,
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
                                                limit = (c(0, isolate(input$read_yaxis_limit))))
    
    ## Add faceting for sorting
    read_plot <- read_plot +
      facet_grid(
        ~ eval(parse(text = isolate(input$read_sortby_axis))),
        space = "free",
        scales = "free",
        switch = "both"
      )
    
    if (isolate(input$read_panel) == "Yes") {
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
          panel.spacing = isolate(unit(as.numeric(input$read_panel_spacing), "points")),
          #legend.position = "none",
          axis.title = element_text(size = 14, face = NULL),
          axis.text.y = element_text(size = 16),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "white"),
        )
    }
    
    if (isolate(input$read_panel) == "No") {
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
          panel.spacing = isolate(unit(as.numeric(input$read_panel_spacing), "points")),
          #legend.position = "none",
          axis.title = element_text(size = 14, face = NULL),
          axis.text.y = element_text(size = 16),
          strip.background = element_rect(fill = "white"),
          strip.text.x = element_text(size = 10, face = "bold")
        )
    }
    
    read_plot <- read_plot + labs(fill = "Sample category")
    read_plot <- read_plot + xlab("Samples")
    read_plot <- read_plot + ylab("Total reads following DADA2")
    
    read_plot
  })
  
  output$read_table_out <- renderDataTable({
    req(input$read_start)
    read_table <- data_read_table()
    read_table
  })
  
  ## You must define the input for width/height within a reactive context, then call it in the output
  read_plot_height = reactive(input$read_plot_out_h)
  read_plot_width = reactive(input$read_plot_out_w)
  
  output$read_plot = renderPlot({
    read_plot = data_read_plot()
    read_plot
    
  },
  width = read_plot_width,
  height = read_plot_height)
  
  # Download output for read plot
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
  
  # Download the table for the read plot
  output$read_table_download <- downloadHandler(
    filename = "read_table.csv",
    content = function(bubble_table) {
      write.csv(data_read_table(), bubble_table)
    })
  
  
  #### Taxonomic Bar Plot ####
  
  ## Update selection - Bar plot
  observeEvent(input$meta_file,{
    req(input$meta_file)
    ## Incorporate the meta file column names into every appropriate input
    meta_datafile <- meta_datafile_og()
    meta_colnames <- colnames(meta_datafile)
    # meta_colnames <- c(colnames(meta_datafile), "TaxaName")
    meta_colnames <- meta_colnames[meta_colnames != "SampleName"]
    ## Update the selections - read plot
    # isolate({
    
    updateSelectInput(session, "bar_sortby_xaxis", choices = sort(meta_colnames))
    updateSelectInput(session, "bar_taxon_level")
    updateTextInput(session, "bar_plotheight")
    updateTextInput(session, "bar_plotwidth")
  })
  
  #
  data_long_bar_filt_re <- reactive({
    filtered_table <- data_filtered_table()
    bar_data_prop <- data_filtered_table()
    meta_data_table <- meta_datafile()
    unfiltered_table <- data_unfiltered_table()
    req(input$bar_start)
    
    ## Initial transformations:
    ## Produce a prop table and filter it to a specific threshold
    bar_data_prop <- prop.table(as.matrix(bar_data_prop), 2) * 100
    bar_data_prop <- as.data.frame(bar_data_prop)
    
    ## If the table contains any columns with zero reads, the column reports NA, and the script fails after this point.
    ## These columns must be removed first.
    bar_data_prop <- bar_data_prop[, colSums(is.na(bar_data_prop)) == 0]
    bar_data_prop <- bar_data_prop %>% filter_all(any_vars(. >= as.numeric(isolate(input$bar_cutoff))))
    
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
    
    ## Sort the table by taxonomic level, then combine the proportions for each sample with a matching taxonomy
    data_long_bar_filt <- data_long_bar_filt %>% group_by(SampleName, eval(parse(text=paste("data_long_bar_filt$",isolate(input$bar_taxon_level))))) %>% summarize(Percentage = sum(Percentage))
    
    

    
    
    
    ## Rename the columns, join with metadata
    colnames(data_long_bar_filt) <- c("SampleName","Taxonomy", "Percentage")
    data_long_bar_filt <- subset(data_long_bar_filt, select = c("SampleName", "Taxonomy", "Percentage"))
    
    ## I need something here to remove the IDs from phyla/class/order whatever so that they are all treated as one. This is an issue for taxa that are not resolved. 
    data_long_bar_filt$Taxonomy <- gsub("_.*","",data_long_bar_filt$Taxonomy)
    
    
    data_long_bar_filt <- left_join(data_long_bar_filt, meta_data_table, by = "SampleName")
    
    ## Include only specific taxa within the plot
    if (is.na(isolate(input$taxon_subset)) == TRUE) {
      warning("No specific taxon selected. Script will continue without filtering by taxa.")
    } else {
      taxon_hits <-
        grepl(
          pattern = isolate(input$taxon_subset),
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
    # bar_incl_samples$TaxaName <- "ZZOther"
    bar_incl_samples$Taxonomy <- "ZZOther"
    
    
    bar_incl_samples <- select(bar_incl_samples, -c("FiltSum"))
    data_long_bar_filt <-
      bind_rows(data_long_bar_filt, bar_incl_samples)
    # data_long_bar_filt <- bind_rows(data_long_bar_filt,bar_ex_samples)
    
    
    barfilt <- colnames(meta_data_table)
    barfilt <- barfilt[barfilt != "SampleName"]
    data_long_bar_filt <- select(data_long_bar_filt,-barfilt)
    
    
    # ## Append the metadata
    data_long_bar_filt <-
      left_join(data_long_bar_filt,
                meta_data_table,
                by = "SampleName",
                copy = FALSE)
    
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
                           y = Percentage, width = 1,
                           fill = Taxonomy
                       ))
    
    bar_plot <- bar_plot + geom_bar(
      # aes(fill = input$bar_taxon_level),
      colour = "black",
      size = 0.5,
      alpha = isolate(input$bar_alpha),
      stat = "identity",
      position = "stack"
    )
    
    if (input$bar_rename_check == TRUE){
      bar_plot <- bar_plot + scale_x_discrete(breaks = data_long_bar_filt$SampleName, labels = data_long_bar_filt$SampleShort)
    }
    
    ## Add faceting for sorting
    bar_plot <- bar_plot +
      facet_grid(
        ~ eval(parse(text = isolate(input$bar_sortby_xaxis))),
        space = "free",
        scales = "free",
        switch = "both"
      )
    
    if (isolate(input$bar_panel_border == "Yes")) {
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
          panel.spacing = unit(as.numeric(isolate(input$bar_panel_spacing)), "points"),
          #legend.position = "none",
          axis.title = element_text(size = 10, face = NULL),
          axis.text.y = element_text(size = 16),
          strip.background = element_rect(fill = "white"),
          strip.text.x = element_text(size = 10, face = "bold"),
        )
    }
    
    if (isolate(input$bar_panel_border) == "No") {
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
          panel.spacing = unit(as.numeric(isolate(input$bar_panel_spacing)), "points"),
          #legend.position = "none",
          axis.title = element_text(size = 10, face = NULL),
          axis.text.y = element_text(size = 16),
          strip.background = element_rect(fill = "white"),
          strip.text.x = element_text(size = 10, face = "bold"),
        )
    }
    
    ## setting the graph so that it begins at the x-axis and there is no gap. Also sets the limits of the y-axis.
    bar_plot <- bar_plot + scale_y_continuous(expand = c(0, 0))
    
    nb.cols <-
      length(unique(eval(parse(
        text = paste("data_long_bar_filt$", "Taxonomy")
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
        eval(parse(text = isolate(input$bar_cutoff))),
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
  
  # Download the table for the bar plot
  output$bar_table_download <- downloadHandler(
    filename = "bar_table.csv",
    content = function(bar_table) {
      write.csv(data_long_bar_filt_re(), bar_table)
    }
  )
  
  ##################### Bubble plot - data transform #########################
  
  ## Update selection - Bubble plot
  observeEvent(input$meta_file,{
    req(input$meta_file)
    
    ## Incorporate the meta file column names into every appropriate input
    meta_datafile <- meta_datafile_og()
    meta_colnames <- colnames(meta_datafile)
    meta_colnames <- c(colnames(meta_datafile), "TaxaName")
    meta_colnames <- meta_colnames[meta_colnames != "SampleName"]
    updateSelectInput(session, "b1_sort_param", choices = sort(meta_colnames))
    updateSelectInput(session, "b1_color_param", choices = sort(meta_colnames))
    updateSelectInput(session, "b1_meta_group", choices = sort(meta_colnames))
    updateTextInput(session, "b1_bubble_width")
    updateTextInput(session, "b1_bubble_height")
    updateTabItems(session, "b1_meta_keyword")
    updateSelectInput(session, "b1_second_facet_meta", choices = sort(meta_colnames))
    updateSelectInput(session, "b1_third_facet_meta", choices = sort(meta_colnames))
    updateSelectInput(session, "b1_sort_axis", choices = sort(meta_colnames))
    updateSelectInput(session, "b1_rename_x", choices = sort(meta_colnames))
  })
  
  
  #Create the bubble plot proportion table, filter based on abundance
  data_bubble_reactive_new <- reactive({
    data_tran <- data_tran_react()
    meta_data_table <- meta_datafile()
    unfiltered_table <- data_unfiltered_table()
    filtered_table <- data_filtered_table()
    req(input$bubble_start)
    
    # ## This removes samples from the ASV tables that don't have a corresponding metadata row: THIS IS WORKING
    meta_samplist <- meta_data_table$SampleName
    filtered_table <- filtered_table[, names(filtered_table) %in% meta_samplist]
    
    ## Initial transformations:
    ## Produce a prop table
    B2_data_prop <- prop.table(as.matrix(filtered_table), 2) * 100
    B2_data_prop <- as.data.frame(B2_data_prop)
    
    ## If the table contains any columns with zero reads, the column reports NA, and the script fails after this point.
    ## These columns must be removed first. This should be exceedingly rare and likely represent failed samples or controls
    B2_data_prop <- B2_data_prop[, colSums(is.na(B2_data_prop)) == 0]
    
    ## This checks each column for values > than the selected threshold. If found, all values in the row are kept, regardless of threshold.
    B2_data_prop <- B2_data_prop %>% filter_all(any_vars(. >= as.numeric(isolate(input$b1_ab_thresh))))
    
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
    B2_data_prop
  })
  
  ## Transform into long form:
  data_data_long_re <- reactive({
    B2_data_prop <- data_bubble_reactive_new()
    meta_data_table <- meta_datafile()
    unfiltered_table <- data_unfiltered_table()
    filtered_table <- data_filtered_table()
    B2_data_long <-
      reshape2::melt(
        B2_data_prop,
        id.vars = c("TaxaName", "Taxonomy"),
        variable.name = as.character("SampleName"),
        value.name = "Percentage"
      )
    
    # Collect all samples
    b1_all_samples <- data.frame(SampleName = colnames(filtered_table))
    
    
    ## Add rep seqs back into data
    # Create a column "Taxonomy" from the row names
    unfiltered_table$Taxonomy <- rownames(unfiltered_table)
    
    
    # Create a filtered table for joining

    if (input$is_main_collapsed == FALSE) {
      unfiltered_selected <- unfiltered_table %>% 
        select(Taxonomy, ReprSequence)
      unfiltered_table <- unfiltered_table[, c("Taxonomy", "ReprSequence")]
    } 
    else if (input$is_main_collapsed == TRUE) {
      unfiltered_selected <- unfiltered_table %>% 
        select(Taxonomy)
    }
    
    B2_data_long <- B2_data_long %>%
      left_join(unfiltered_selected, by = "Taxonomy", copy = TRUE)
    
    # B2_data_long <- left_join(B2_data_long,
    #                           unfiltered_table,
    #                           by = "Taxonomy",
    #                           copy = TRUE)
    
    ## Modify taxonomy names
    B2_data_long$Taxonomy <-
      gsub("(D_.__)", "", B2_data_long$Taxonomy)
    B2_data_long$Taxonomy <- gsub(";$", "", B2_data_long$Taxonomy)
    
    
    
    ## Remove any zero-values from the data so that they aren't plotted. But why are there zeros here in the first place? Because the initial filtering step keeps all column/rows where at least one sample is above a specific threshold, keeping zero-values. This might be useful for keeping the samples I need. 
    
    data_long_bubble <- B2_data_long
    
    data_long_bubble <-
      dplyr::filter(data_long_bubble, Percentage > 0)
    
    ## This option allows a user to keep all samples within the plot regardless of taxon presence (i.e., a sample on the X-axis with no bubbles in the plot). This is achieved sloppily by adding in a fake taxon that is plotted and can be removed during post-processing. This fake taxon is added indepdenent of any abundance calculations and so does not affect proportions ore relative abundances or legend scaling. It is added at the abundance threshold set in the plot (e.g., 3%, 5%)
    
    ## Add in a fake taxonomy to show all samples
    if(input$b1_fake_taxon == TRUE){
      if(input$is_main_collapsed == FALSE){
      fake_taxon_df <- data.frame(SampleName = unique(data_long_bubble$SampleName), Percentage = 1, TaxaName = "AAA_FAKE_TAXON_999999", Taxonomy = "d_AAAFAKETERIA;p_AAA_FAKE_TAXON_999999", ReprSequence = "AAAA")
      }
      else if (input$is_main_collapsed == TRUE){
        fake_taxon_df <- data.frame(SampleName = unique(data_long_bubble$SampleName), Percentage = 1, TaxaName = "AAA_FAKE_TAXON_999999", Taxonomy = "d_AAAFAKETERIA;p_AAA_FAKE_TAXON_999999")
      }
      data_long_bubble <- rbind(data_long_bubble, fake_taxon_df)
    }
    
      data_long_bubble <-
        left_join(data_long_bubble,
                  meta_data_table,
                  by = "SampleName",
                  copy = TRUE)

    ## Round percentages to a given decimal places
    data_long_bubble$Percentage <-
      round(data_long_bubble$Percentage,
            digits = as.numeric(isolate(input$b1_num_dec)))
    
    
    ## Selecting specific taxa
    if (!is.na(isolate(input$b1_tax_keyword))) {
      # Split the input$b1_tax_keyword into a list of keywords using ","
      tax_keywords <- strsplit(isolate(input$b1_tax_keyword), ",")[[1]]
      if (length(tax_keywords) > 0){
      # Create a pattern by pasting the keywords with "|" for OR condition
      pattern <- paste(tax_keywords, collapse = "|")
      
      # Perform pattern matching using grepl
      taxon_hits <- grepl(
        pattern = pattern,
        ignore.case = TRUE,
        x = data_long_bubble$Taxonomy
      )
      
      # Subset the data_long_bubble dataset based on taxon_hits
      data_long_bubble <- data_long_bubble[taxon_hits, ]
      
      # Display a warning message indicating taxa filtering is selected
      warning("Taxa filtering selected.")
      }
    }
    
    ## Filtering specific taxa
    if (!is.na(isolate(input$b1_tax_remove))){
      # Split the input$b1_tax_keyword into a list of keywords using ","
      tax_rem_keywords <- strsplit(isolate(input$b1_tax_remove), ",")[[1]]
      if (length(tax_rem_keywords) > 0){
      
      # Create a pattern by pasting the keywords with "|" for OR condition
      pattern <- paste(tax_rem_keywords, collapse = "|")

      # Perform pattern matching using grepl
      taxon_hits <- grepl(
        pattern = pattern,
        ignore.case = TRUE,
        x = data_long_bubble$Taxonomy
      )

      # Subset the data_long_bubble dataset based on taxon_hits
      data_long_bubble <- data_long_bubble[!taxon_hits, ]

      # Display a warning message indicating taxa filtering is selected
      warning("Taxa filtering selected.")
      }
    }
    
    ## Filtering samples by a metadata category
    if (is.na(isolate(input$b1_meta_group))) {
      warning("No metadata category selected. Script will continue without filtering by groups.")
    } else {
      # Split the isolate(input$b1_meta_keyword) into a list of keywords using ","
      meta_keywords <- strsplit(isolate(input$b1_meta_keyword), ",")[[1]]
      
      # Create a pattern by pasting the keywords with "|" for OR condition
      pattern <- paste(meta_keywords, collapse = "|")
      
      # Use any() to check if any keyword matches in the metadata group
      sample_hits <- sapply(data_long_bubble[, isolate(input$b1_meta_group)], function(group_value) {
        any(grepl(pattern, group_value, ignore.case = TRUE))
      })
      
      # Subset the data_long_bubble dataset based on sample_hits
      data_long_bubble <- data_long_bubble[sample_hits, ]
      
      # Display a warning message indicating metadata filtering is selected
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
    
    # Set the taxonomy as factors for later ordering. This doesn't do much, but if you remove it the colouring settings change
      data_long_bubble <-
        data_long_bubble[with(data_long_bubble,
                              order(eval(parse(
                                text = isolate(input$b1_tax_sort)
                              )), TaxaName, decreasing = TRUE)), ]
      data_long_bubble$TaxaName <-
        as.character(data_long_bubble$TaxaName)
      data_long_bubble$TaxaName <-
        factor(data_long_bubble$TaxaName,
               levels = unique(data_long_bubble$TaxaName))
      data_long_bubble

  })
  
  bubble_plot_re <- reactive({
    data_long_bubble <- data_data_long_re()
    
    #### Main Bubble plot: ####
    bubble_plot <- ggplot(
      data_long_bubble,
      aes(
        x = reorder(SampleName, eval(parse(
          text = isolate(input$b1_sort_axis)
        ))),
          y = TaxaName,
        fill = as.factor(eval(parse(
          text = isolate(input$b1_color_param)
        ))),
        
        if(input$b1_factor_data == TRUE){
          color = as.factor(eval(parse(
            text = isolate(input$b1_color_param)
          )))
        },
        
        if(input$b1_factor_data == FALSE){
          color = eval(parse(
            text = isolate(input$b1_color_param)
          ))
          },
        size = Percentage
      ),
      colsep = c(1:100),
      rowsep = (1:100),
      sepwidth = c(5, 1)
    ) +
      guides(fill = "none", colour = "none")+
      labs(size = "Relative abundance")
    
    if (input$b1_rename_x_check == TRUE){
      bubble_plot <- bubble_plot + scale_x_discrete(breaks = data_long_bubble$SampleName, labels = data_long_bubble$SampleShort)
    }
    
    ## Sorting the y-axis and faceting options. I need to overhaul this to use three levels of nested faceting
    ## So for some reason you need to use the * operators instead of the + for faceting. Not sure why this changed. 
    
    
    # Adding second faceting
    if (isolate(input$b1_second_facet) == TRUE) {
      bubble_plot <- bubble_plot +
        
        # First must check the facet options. Not sure how else to do this without running through all possible cases:
          if ((input$b1_facet_side_x == "Top") &
              (input$b1_facet_side_y == "Right")) {
            facet_nested(
              eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = isolate(input$b1_sort_param))) +
                eval(parse(text = isolate(input$b1_second_facet_meta))),
              space = "free",
              scales = "free"
            )}
          
          else if ((input$b1_facet_side_x == "Top") &
                   (input$b1_facet_side_y == "Left")) {
            facet_nested(
              eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = isolate(input$b1_sort_param))) +
                eval(parse(text = isolate(input$b1_second_facet_meta))),
              space = "free",
              scales = "free",
              switch = "y"
            )}
          
          else if ((input$b1_facet_side_x == "Bottom") &
                   (input$b1_facet_side_y == "Left")) {
            facet_nested(
              eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = isolate(input$b1_sort_param))) +
                eval(parse(text = isolate(input$b1_second_facet_meta))),
              space = "free",
              scales = "free",
              switch = "both"
            )}
          
          else if ((input$b1_facet_side_x == "Bottom") &
                   (input$b1_facet_side_y == "Right")) {
            facet_nested(
              eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = isolate(input$b1_sort_param))) +
                eval(parse(text = isolate(input$b1_second_facet_meta))),
              space = "free",
              scales = "free",
              switch = "x"
            )}
    }
      
    #   # Now if you're not sorting the y-axis based on taxonomy:
    #   else if (input$b1_confirm_sort == "No") {
    #     if (input$b1_facet_side_x == "Top") {
    #       facet_nested(
    #         ~ eval(parse(text = isolate(input$b1_sort_param))) +
    #           eval(parse(text = isolate(input$b1_second_facet_meta))),
    #         space = "free",
    #         scales = "free"
    #       )
    #     }
    #     
    #     else if (input$b1_facet_side_x == "Bottom") {
    #       facet_nested(
    #         ~ eval(parse(text = isolate(input$b1_sort_param))) +
    #           eval(parse(text = isolate(input$b1_second_facet_meta))),
    #         space = "free",
    #         scales = "free",
    #         switch = "x"
    #       )
    #     }
    #   }
    # }# This is the end of the second-level faceting
    
    
    # If no additional faceting is chosen:
    if (isolate(input$b1_second_facet) == FALSE) {
      bubble_plot <- bubble_plot +
        
        # First must check the facet options. Not sure how else to do this without running through all possible cases:

          if ((input$b1_facet_side_x == "Top") &
              (input$b1_facet_side_y == "Right")) {
            facet_nested(
              eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = isolate(input$b1_sort_param))),
              space = "free",
              scales = "free"
            )}
          
          else if ((input$b1_facet_side_x == "Top") &
                   (input$b1_facet_side_y == "Left")) {
            facet_nested(
              eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = isolate(input$b1_sort_param))),
              space = "free",
              scales = "free",
              switch = "y"
            )}
          
          else if ((input$b1_facet_side_x == "Bottom") &
                   (input$b1_facet_side_y == "Left")) {
            facet_nested(
              eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = isolate(input$b1_sort_param))),
              space = "free",
              scales = "free",
              switch = "both"
            )}
          
          else if ((input$b1_facet_side_x == "Bottom") &
                   (input$b1_facet_side_y == "Right")) {
            facet_nested(
              eval(parse(text = input$b1_tax_sort)) ~ eval(parse(text = isolate(input$b1_sort_param))),
              space = "free",
              scales = "free",
              switch = "x"
            )}
    }
      
    #   # Now if you're not sorting the y-axis based on taxonomy:
    #   else if (input$b1_confirm_sort == "No") {
    #     if (input$b1_facet_side_x == "Top") {
    #       facet_nested(
    #         ~ eval(parse(text = isolate(input$b1_sort_param))),
    #         space = "free",
    #         scales = "free"
    #       )
    #     }
    #     
    #     else if (input$b1_facet_side_x == "Bottom") {
    #       facet_nested(
    #         ~ eval(parse(text = isolate(input$b1_sort_param))),
    #         space = "free",
    #         scales = "free",
    #         switch = "x"
    #       )
    #     }
    #   }
    # }# This is the end of default faceting
    
    
    
    # Third level faceting. This code must come after all others. Not sure why. But if it comes first it doesn't remain dynamic
    if (isolate(input$b1_third_facet) == TRUE) {
      bubble_plot <- bubble_plot +
        
        # First must check the facet options. Not sure how else to do this without running through all possible cases:
          if ((input$b1_facet_side_x == "Top") &
              (input$b1_facet_side_y == "Right")) {
            facet_nested(
              eval(parse(text = isolate(input$b1_tax_sort))) ~ eval(parse(text = isolate(input$b1_sort_param))) *
                eval(parse(text = isolate(input$b1_second_facet_meta))) * 
                eval(parse(text = isolate(input$b1_third_facet_meta))),
              space = "free",
              scales = "free"
            )}
          
          else if ((input$b1_facet_side_x == "Top") &
                   (input$b1_facet_side_y == "Left")) {
            facet_nested(
              eval(parse(text = isolate(input$b1_tax_sort))) ~ eval(parse(text = isolate(input$b1_sort_param))) *
                eval(parse(text = isolate(input$b1_second_facet_meta))) * 
                eval(parse(text = isolate(input$b1_third_facet_meta))),
              space = "free",
              scales = "free",
              switch = "y"
            )}
          
          else if ((input$b1_facet_side_x == "Bottom") &
                   (input$b1_facet_side_y == "Left")) {
            facet_nested(
              eval(parse(text = isolate(input$b1_tax_sort))) ~ eval(parse(text = isolate(input$b1_sort_param))) *
                eval(parse(text = isolate(input$b1_second_facet_meta))) * 
                eval(parse(text = isolate(input$b1_third_facet_meta))),
              space = "free",
              scales = "free",
              switch = "both"
            )}
          else if ((input$b1_facet_side_x == "Bottom") &
                   (input$b1_facet_side_y == "Right")) {
            facet_nested(
              eval(parse(text = isolate(input$b1_tax_sort))) ~ eval(parse(text = isolate(input$b1_sort_param))) *
                eval(parse(text = isolate(input$b1_second_facet_meta))) * 
                eval(parse(text = isolate(input$b1_third_facet_meta))),
              space = "free",
              scales = "free",
              switch = "x"
            )}
    }
      
    #   # Now if you're not sorting the y-axis based on taxonomy:
    #   else if (input$b1_confirm_sort == "No") {
    #     if (input$b1_facet_side_x == "Top") {
    #       facet_nested(
    #         ~ eval(parse(text = isolate(input$b1_sort_param))) *
    #           eval(parse(text = isolate(input$b1_second_facet_meta))) * 
    #           eval(parse(text = isolate(input$b1_third_facet_meta))),
    #         space = "free",
    #         scales = "free"
    #       )
    #     }
    #     
    #     else if (input$b1_facet_side_x == "Bottom") {
    #       facet_nested(
    #         ~ eval(parse(text = isolate(input$b1_sort_param))) *
    #           eval(parse(text = isolate(input$b1_second_facet_meta))) * 
    #           eval(parse(text = isolate(input$b1_third_facet_meta))),
    #         space = "free",
    #         scales = "free",
    #         switch = "x"
    #       )
    #     }
    #   }
    # }# This is the end of the third-level faceting
    
    
    ## Add the bubbles and percentage labels to the plot:
    
    if (isolate(input$b1_incl_percent) == "Yes") {
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
            eval(parse(text = isolate(input$b1_ab_thresh))),
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
            eval(parse(text = isolate(input$b1_ab_thresh))),
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
          strip.background.y = element_rect(fill = "white"),
          strip.background.x = element_rect(fill = "white"),
          panel.spacing = unit(as.numeric(input$b1_panel_spacing), "points"),
          legend.position = "bottom",
          #panel.grid = element_line(colour = "grey"),
          #axis.line.y = element_line(colour="black",size=0.5),
          #panel.border = element_blank(),
          #text = element_text(size=10),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          ),
          plot.margin=unit(c(-0.30,0,0,0), "null")
          
          #legend.position = "none")
        )
    }
    
    ## modify the general theme, removing panel borders
    if (input$b1_panel_border == "No") {
      bubble_plot <- bubble_plot +
        theme_bw() + theme(
          axis.text = element_text(colour = "black", size = 10),
          strip.background.y = element_rect(fill = "white"),
          strip.background.x = element_rect(fill = "white"),
          panel.spacing = unit(as.numeric(input$b1_panel_spacing), "points"),
          legend.position = "bottom",
          #panel.grid = element_line(colour = "grey"),
          #axis.line.y = element_line(colour="black",size=0.5),
          panel.border = element_blank(),
          #text = element_text(size=10),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          ),
          plot.margin=unit(c(-0.30,0,0,0), "null")
          #legend.position = "none")
        )
    }
    bubble_plot ## This is the end of the base plot
  })
  
  #### Bubble plot - sample reads ####
  ## This should be made into its own function
  
  bubble_read_plot_re <- reactive({
    filtered_table <- data_filtered_table()
    meta_data_table <- meta_datafile()
    data_long_bubble <- data_data_long_re()
    req(input$bubble_start)
    
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
    
    ## Reorder x-axis to follow metadata category in data frame
    data_read_total$SampleName <-
      as.character(data_read_total$SampleName)
    data_read_total$SampleName <-
      factor(data_read_total$SampleName,
             levels = unique(data_read_total$SampleName))
    
    ## Make a list of unique metadata
    read_meta_list <- unique(isolate(input$b1_sort_param))
    
    ## Now filter for metadata, if selected
    if (is.na(isolate(input$b1_meta_group))) {
      warning("No metadata category selected. Script will continue without filtering by groups.")
    } else {
      # Split the input$b1_meta_keyword into a list of keywords using ","
      meta_keywords <- strsplit(isolate(input$b1_meta_keyword), ",")[[1]]
      
      # Create a pattern by pasting the keywords with "|" for OR condition
      pattern <- paste(meta_keywords, collapse = "|")
      
      # Use any() to check if any keyword matches in the metadata group
      sample_hits <- sapply(data_read_total[, isolate(input$b1_meta_group)], function(group_value) {
        any(grepl(pattern, group_value, ignore.case = TRUE))
      })
      
      # Subset the data_long_bubble dataset based on sample_hits
      data_read_total <- data_read_total[sample_hits, ]
      
      # Display a warning message indicating metadata filtering is selected
      warning("Metadata filtering selected.")
    }
    
    # if (is.na(isolate(input$b1_meta_group)) == TRUE) {
    #   warning(
    #     "No metadata category selected. Script will continue without filtering by groups."
    #   )
    # } else {
    #   sample_hits <-
    #     grepl(
    #       pattern = isolate(input$b1_meta_keyword),
    #       ignore.case = TRUE,
    #       x = (eval(parse(
    #         text = paste("data_read_total$", isolate(input$b1_meta_group))
    #       )))
    #     )
    #   data_read_total <- data_read_total[sample_hits, ]
    #   warning("Metadata filtering selected.")
    # }
    
    read_plot <- ggplot(data = data_read_total,
                        aes(
                          x = SampleName,
                          y = Total,
                          width = 0.9
                        ))
    
    read_plot <-
      read_plot + geom_bar(
        aes(fill = "grey"),
        colour = "black",
        size = 0.5,
        alpha = 0.8,
        stat = "identity",
        ## this position_dodge preserves the size of the bar, so that you don't have different sized bars for identical sample names. position=position_dodge(preserve = "total")
        ## to make a stacked bar plot, also involved in pie charts
        position = "stack"
      ) + scale_fill_manual(values = "grey")
    
    read_plot
    
    
    ## setting the graph so that it begins at the x-axis and there is no gap. Also sets the limits of the y-axis.
    # read_plot <- read_plot + scale_y_continuous(expand = c(0, 0),
    #                                             limit = (c(0, max(data_read_total$Total + 10000))))
    # read_plot <- read_plot + scale_y_log10()
    
    ## Add faceting for sorting, requires to work alongside the multiple faceting options in the bubble plot. 
    # If second-level faceting:
    if (isolate(input$b1_second_facet) == TRUE){
      read_plot <- read_plot +
        facet_grid(~ eval(parse(text = isolate(input$b1_sort_param))) + eval(parse(text = isolate(input$b1_second_facet_meta))),
                   space = "free",
                   scales = "free",
                   switch = "both"
        )
    }
    
    # Default faceting - single level
    if (isolate(input$b1_second_facet) == FALSE){
      read_plot <- read_plot +
        facet_grid(
          ~ eval(parse(text = isolate(input$b1_sort_param))),
          space = "free",
          scales = "free",
          switch = "both"
        )
    }
    
    if (isolate(input$b1_third_facet) == TRUE){
      read_plot <- read_plot +
        facet_grid( ~ eval(parse(text = isolate(input$b1_sort_param))) * eval(parse(text = isolate(input$b1_second_facet_meta))) * eval(parse(text = isolate(input$b1_third_facet_meta))),
                    space = "free",
                    scales = "free",
                    switch = "both"
        )
    }
    
    #If third-level faceting:
    
    read_plot <- read_plot + theme_bw() +
      theme(
        panel.grid = element_blank(),
        text = element_text(colour = "black"),
        #axis.line = element_line(colour = "black"),
        axis.line = element_blank(),
        axis.text = element_text(colour = "black", size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(face = "italic", size = 12),
        legend.title = element_text(size = 10),
        panel.spacing = unit(as.numeric(input$b1_panel_spacing), "points"),
        legend.position = "none",
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"),
        axis.title = element_text(size = 10, face = NULL),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_blank(),
        plot.margin=unit(c(-0.30,0,0,0), "null")
      )
    
    
    # read_plot <- read_plot + labs(fill = "Sample category")
    # read_plot <- read_plot + xlab("Samples")
    read_plot <- read_plot + ylab("Reads")
    
    read_plot 
  }) #End of the read bubble read plot
  
  #### Bubble plot - taxa proportions ####
  bubble_taxa_plot_re <- reactive({
    req(input$bubble_start)
    if (isolate(input$b1_include_taxa) == TRUE){
      # row sums for taxonomy read plot
      B2_data_prop <- data_bubble_reactive_new()
      data_long_bubble <- data_data_long_re()
      filtered_table <- data_filtered_table()
      data_read <- filtered_table
      
      taxonomy_read_data <- data_read
      taxonomy_read_data$Total <- rowSums(taxonomy_read_data)
      
      # Total number of reads in taxonomy read data
      taxonomy_read_total <- sum(taxonomy_read_data$Total)
      
      #Assign taxonomy to last column, remove total column
      taxonomy_read_data$Taxonomy <- rownames(taxonomy_read_data)
      # taxonomy_read_data <- subset(taxonomy_read_data, select = -Total)
      
      #B2_prop table used as a template to put read count data back in. Use the rownames to do this.
      taxonomy_read_filtered <- subset(B2_data_prop, select = - TaxaName)
      
      # Add RowNames column for replacing data
      taxonomy_read_filtered$TaxaName <- rownames(taxonomy_read_filtered)
      # taxonomy_read_data$RowNames <- rownames(taxonomy_read_data)
      
      #Now delete all data in the final file except for the row names, and left join with the read data file
      taxonomy_read_filtered <- subset(taxonomy_read_filtered, select = Taxonomy)
      taxonomy_read_filtered$TaxaName <- rownames(taxonomy_read_filtered)
      taxonomy_read_filtered <- left_join(taxonomy_read_filtered,taxonomy_read_data, by = "Taxonomy")
      
      #Set row names
      rownames(taxonomy_read_filtered) <- taxonomy_read_filtered$Taxonomy
      # taxonomy_read_final <- subset(taxonomy_read_final, select = -RowNames)
      
      #Now parse the taxonomy and add a total and proportion
      taxonomy_read_filtered$Sep <- taxonomy_read_filtered$Taxonomy
      taxonomy_read_filtered$Sep <- gsub("(d__)", "", taxonomy_read_filtered$Sep)
      taxonomy_read_filtered$Sep <- gsub("(p__)", "", taxonomy_read_filtered$Sep)
      taxonomy_read_filtered$Sep <- gsub("(c__)", "", taxonomy_read_filtered$Sep)
      taxonomy_read_filtered$Sep <- gsub("(o__)", "", taxonomy_read_filtered$Sep)
      taxonomy_read_filtered$Sep <- gsub("(f__)", "", taxonomy_read_filtered$Sep)
      taxonomy_read_filtered$Sep <- gsub("(g__)", "", taxonomy_read_filtered$Sep)
      taxonomy_read_filtered$Sep <- gsub("(s__)", "", taxonomy_read_filtered$Sep)
      taxonomy_read_filtered <-
        separate(
          taxonomy_read_filtered,
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
      
      
      # Set your chosen taxonomic level and parse the data, then set the column names
      taxonomy_read_final = taxonomy_read_filtered %>% distinct(taxonomy_read_filtered$TaxaName,
                                                                taxonomy_read_filtered$Total,
                                                                eval(parse(text = paste("taxonomy_read_filtered$",isolate(input$b1_tax_sort)))))
      colnames(taxonomy_read_final) = c("TaxaName","Total","taxonomic_level")
      
      ## Generate a proportion of reads by dividing the read number in each row by the total number of reads.
      taxonomy_read_final$Prop = taxonomy_read_final$Total / taxonomy_read_total * 100
      
      ## Filter out specific taxonomy
      # Set rownames into a new column
      taxonomy_read_final$full_lineage <- rownames(taxonomy_read_final)
      
      
      ## Selecting specific taxa
      if (!is.na(isolate(input$b1_tax_keyword))) {
        # Split the input$b1_tax_keyword into a list of keywords using ","
        tax_keywords <- strsplit(isolate(input$b1_tax_keyword), ",")[[1]]
        if (length(tax_keywords) > 0){
          # Create a pattern by pasting the keywords with "|" for OR condition
          pattern <- paste(tax_keywords, collapse = "|")
          
          # Perform pattern matching using grepl
          taxon_hits <- grepl(
            pattern = pattern,
            ignore.case = TRUE,
            x = data_long_bubble$Taxonomy
          )
          
          # Subset the data_long_bubble dataset based on taxon_hits
          data_long_bubble <- data_long_bubble[taxon_hits, ]
          
          # Display a warning message indicating taxa filtering is selected
          warning("Taxa filtering selected.")
        }
      }
      
      ## Filtering specific taxa
      if (!is.na(isolate(input$b1_tax_remove))){
        # Split the input$b1_tax_keyword into a list of keywords using ","
        tax_rem_keywords <- strsplit(isolate(input$b1_tax_remove), ",")[[1]]
        if (length(tax_rem_keywords) > 0){
          
          # Create a pattern by pasting the keywords with "|" for OR condition
          pattern <- paste(tax_rem_keywords, collapse = "|")
          
          # Perform pattern matching using grepl
          taxon_hits <- grepl(
            pattern = pattern,
            ignore.case = TRUE,
            x = data_long_bubble$Taxonomy
          )
          
          # Subset the data_long_bubble dataset based on taxon_hits
          data_long_bubble <- data_long_bubble[!taxon_hits, ]
          
          # Display a warning message indicating taxa filtering is selected
          warning("Taxa filtering selected.")
        }
      }
      
      
      
      # ## New taxa filtering
      # if (is.na(isolate(input$b1_tax_keyword)) == TRUE) {
      #   warning("No specific taxon selected. Script will continue without filtering by taxa.")
      # } else {
      #   # Split the input$b1_tax_keyword into a list of keywords using ","
      #   tax_keywords <- strsplit(isolate(input$b1_tax_keyword), ",")[[1]]
      #   
      #   # Create a pattern by pasting the keywords with "|" for OR condition
      #   pattern <- paste(tax_keywords, collapse = "|")
      #   
      #   # Perform pattern matching using grepl
      #   taxon_hits <- grepl(
      #     pattern = pattern,
      #     ignore.case = TRUE,
      #     x = taxonomy_read_final$full_lineage
      #   )
      #   
      #   # Subset the data_long_bubble dataset based on taxon_hits
      #   taxonomy_read_final <- taxonomy_read_final[taxon_hits, ]
      #   
      #   # Display a warning message indicating taxa filtering is selected
      #   warning("Taxa filtering selected.")
      # }
      
      taxonomy_read_final <- taxonomy_read_final[taxonomy_read_final$TaxaName %in% data_long_bubble$TaxaName, ,drop = FALSE]
      
      # ## New sample group filtering. What this actually needs to do is check which taxa remain in the filtered data_long_bubble table after
      # earlier metadata filtering, then only include proportion data for the TaxaName that remain. 
      
      
      
      
      # if (is.na(input$b1_tax_keyword) == TRUE) {
      #   warning("No specific taxon selected. Script will continue without filtering by taxa.")
      # } else {
      #   taxon_hits <-
      #     grepl(
      #       pattern = input$b1_tax_keyword,
      #       ignore.case = TRUE,
      #       x = taxonomy_read_final$full_lineage
      #     )
      #   taxonomy_read_final <- taxonomy_read_final[taxon_hits, ]
      #   warning("CONGRATULATIONS! Taxa filtering selected.")
      # }
      
      ## The faceting does not properly align the proportions; they are reversed. So you have to reorder them manually.
      taxa_readplot = ggplot(data = taxonomy_read_final, aes(x =  reorder(TaxaName, desc(TaxaName)), y = Prop))
      
      ## Define the geom_bar work space and modify the visuals. Modify the position variable to change between a standard and stacked bar plot.
      taxa_readplot <- taxa_readplot + geom_bar(aes(),
                                                position = position_dodge2(),
                                                #position = position_dodge2(preserve = "single"),
                                                #position = position_stack(reverse = TRUE),
                                                stat="identity",
                                                colour="black",
                                                size=0.6,
                                                alpha=0.7,
                                                width = 0.9,
      ) + scale_fill_manual(values = "grey")
      
      # Flip the bar chart so it is aligned vertically
      taxa_readplot <- taxa_readplot + coord_flip()
      
      
      # taxa_readplot <- taxa_readplot + facet_nested(eval(parse(text = isolate(input$b1_tax_sort)))~., scales = "free", space = "free")
      taxa_readplot <- taxa_readplot + facet_nested(taxonomic_level~., scales = "free", space = "free")
      
      
      ## Modify the general visual theme (i.e., background, legends, axis titles).
      taxa_readplot <- taxa_readplot + theme_bw() + theme(
        panel.grid = element_blank(),
        text = element_text(colour = "black"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black",size=0),
        axis.line.x.bottom = element_line(size=-0),
        axis.text = element_text(colour = "black",size=12),
        axis.text.x = element_text(angle = 60, hjust =1.4, vjust=1.2,size=12,face = "plain"),
        legend.text = element_text(face = "plain",size = 16),
        legend.title = element_text(size=16),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(size=10,face="bold"),
        strip.background = element_rect(fill = "white", color = "black"),
        panel.spacing = unit(as.numeric(input$b1_panel_spacing), "points"),
        panel.border = element_rect(colour="black",size=0,fill=NA),
        axis.ticks = element_line(colour = "black"),
        axis.ticks.y = element_blank(),
        #panel.grid.major.y = element_line(colour = "black"),
        axis.line.y = element_line(colour="black"),
        strip.text.y = element_blank(),
        plot.margin=unit(c(0,0,0,0), "null")
      )
      
      taxa_readplot
    }
  })
  
  
  #### Bubble plot - final patchwork ####
  
  bubble_combined_plot_re <- reactive({
    req(input$bubble_start)
    bubble_plot <- bubble_plot_re()

    # If both the read plot and taxa proportion bar plots are selected:
    if(isolate(input$b1_include_read) == TRUE && isolate(input$b1_include_taxa)){
      read_plot <- bubble_read_plot_re()
      taxa_readplot <- bubble_taxa_plot_re()
      layout <- "
                AA##
                BBCC
                    "
      bubble_plot <- read_plot / bubble_plot + 
        taxa_readplot + 
        plot_layout(design = layout, heights = c(0.25,1,0.10), widths = c(0.25,1,0.01))
      bubble_plot
    }
    
    # If only the read plot is selected
    else if(isolate(input$b1_include_read) == TRUE){
      read_plot <- bubble_read_plot_re()
      bubble_plot <- read_plot + 
        bubble_plot + 
        plot_layout(ncol = 1, heights = c(0.1,1))
      bubble_plot
    }
    
    # If only the taxa proportion plot is selected
    else if(isolate(input$b1_include_taxa) == TRUE){
      taxa_readplot <- bubble_taxa_plot_re()
      bubble_plot <- bubble_plot + 
        taxa_readplot + 
        plot_layout(nrow = 1, heights = c(0.10,1))
      bubble_plot
    } else
    bubble_plot
  })
  
  
  output$b1_table_out <- renderDataTable({
    bubble_table <- data_bubble_reactive_new()
  })
  
  b1_plot_height <- reactive(input$b1_plot_out_h)
  b1_plot_width <- reactive(input$b1_plot_out_w)
  
  output$bubble_out <- renderPlot({
    bubble_plot <- bubble_combined_plot_re()
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
        plot = bubble_combined_plot_re(),
        device = "pdf",
        height = as.numeric(input$b1_plot_out_h),
        width = as.numeric(input$b1_plot_out_w),
        units = "px",
        scale = 4
      )
    }
  )
  
  output$b1_data_table <- downloadHandler(
    filename = "bubble_data_table.csv",
    content = function(bubble_table) {
      write.csv(data_bubble_reactive_new(), bubble_table)
    }
  )
  
  #### Bray Curtis Triplot ####
  
  ## Update selection - Bray-Curtis PCoA plot
  observeEvent(input$meta_file, {
    req(input$meta_file)
    meta_datafile <- meta_datafile()
    meta_colnames <- c(colnames(meta_datafile), "TaxaName")
    meta_colnames <- meta_colnames[meta_colnames != "SampleName"]
    updateSelectInput(session, "pcoa_fill_col", choices = sort(meta_colnames))
    updateSelectInput(session, "pcoa_elips_col", choices = sort(meta_colnames))
    updateSelectInput(session, "pcoa_shape", choices = sort(meta_colnames))
    updateSelectInput(session, "pcoa_pallet_selection")
    updateSelectInput(session, "pcoa_gradient")
  })
  
  #Create an SRS table  
  bc_srs_table_re <- reactive({
    req(input$pcoa_start)
    ## Set an option to select SRS, rarefy, or none (default SRS)
    main_data_table <- main_datafile()
    meta_data_table <- meta_datafile()
    feature_taxonomy_labels <- data_labels_react()
    # Set the seed for reproducibility (optional)
    srs_depth <- isolate(input$pcoa_srs_depth)
    srs_table <- main_data_table[, !colnames(main_data_table) %in% c("Consensus.Lineage",
                                                                     "Feature.ID",
                                                                     "rowID",
                                                                     "ReprSequence",
                                                                     "last_taxon")]
    srs_table <- SRS(srs_table, Cmin = srs_depth, seed = 123)
    rownames(srs_table) <- rownames(feature_taxonomy_labels)
    srs_table
  })
  
  #Create a proportion table
  bc_srs_prop_re <- reactive({
    req(input$pcoa_start)
    srs_table <- bc_srs_table_re()
    meta_data_table <- meta_datafile()
    
    proportion_table <- srs_table / colSums(srs_table)
    proportion_table
  })
  
  #Generate the PCoA table
  bc_pcoa_react <- reactive({
    req(input$pcoa_start)
    proportion_table <- bc_srs_prop_re()
    meta_data_table <- meta_datafile()
    
    t_proportion_table <- t(proportion_table)
    bray_curtis <-
      as.matrix(vegdist(t_proportion_table, method = "bray"))
    
    # Perform PCoA using the APE package
    pcoa_result <- pcoa(as.dist(bray_curtis))
    pcoa_result
    
  })
  
  #Isolate the axis1 and axis2 coordinates
  pcoa_coords_react <- reactive({
    req(input$pcoa_start)
    pcoa_result <- bc_pcoa_react()
    
    pcoa_coords <- pcoa_result$vectors[, 1:2]
    pcoa_df <-
      data.frame(
        PCoA1 = pcoa_coords[, 1],
        PCoA2 = pcoa_coords[, 2],
        row.names = row.names(pcoa_coords)
      )
    pcoa_df
  })
  
  #Isolate the relative corrected eigen values
  pcoa_eigen_react <- reactive({
    req(input$pcoa_start)
    pcoa_result <- bc_pcoa_react()
    
    # Extract relative eigenvalues
    eigenvalues <- pcoa_result$values[3]
    
    # Extract the first two eigenvalues
    Axis1 <- eigenvalues[1,] * 100
    Axis2 <- eigenvalues[2,] * 100
    eigen_df <- data.frame(Axis1 = Axis1, Axis2 = Axis2)
    eigen_df
  })
  
  #Filter the metadata for missing samples after SRS rarefaction
  bc_pcoa_metafilt <- reactive({
    req(input$pcoa_start)
    pcoa_result <- bc_pcoa_react()
    meta_data_table <- meta_datafile()
    srs_table <- bc_srs_table_re()
    
    # Collect the column names after SRS rarefaction
    pcoa_filt_colnames <- colnames(srs_table)
    
    # Filter the metadata table
    meta_data_table <-
      meta_data_table %>% filter(SampleName %in% pcoa_filt_colnames)
    meta_data_table
    meta_data_table
  })
  
  #Fit the environment variables to the PCoA coordinates
  bc_pcoa_envfit_re <- reactive({
    req(input$pcoa_start)
    pcoa_result <- bc_pcoa_react()
    meta_data_table <- bc_pcoa_metafilt()
    srs_table <- bc_srs_prop_re()
    
    #conver to a dataframe
    pcoa_vectors <- as.data.frame(pcoa_result$vectors)
    pcoa_envfit <- envfit(pcoa_vectors, meta_data_table, perm = 10000)
    
    ## Scales the arrow vectors so they aren't huge
    pcoa_envfit_df <- as.data.frame(pcoa_envfit$vectors$arrows * sqrt(pcoa_envfit$vectors$r))
    pcoa_envfit_df <- cbind(pcoa_envfit_df, pcoa_envfit$vectors$r)
    pcoa_envfit_df <- cbind(pcoa_envfit_df, pcoa_envfit$vectors$pvals)
    colnames(pcoa_envfit_df) <- c("axis1", "axis2","R2", "pvalue")
    pcoa_envfit_df$R2_rounded <- round(pcoa_envfit_df$R2, 5)
    pcoa_envfit_df$pvalue <- round(pcoa_envfit_df$pvalue, 5)
    pcoa_envfit_df
  })
  
  #Filter the data below a given R or p-value threshold
  pcoa_envfit_df_filt_re <- reactive({
    req(input$pcoa_start)
    pcoa_envfit_df <- bc_pcoa_envfit_re()
    pcoa_envfit_df_filt <- filter(pcoa_envfit_df,
                                  pcoa_envfit_df$pvalue < (input$pcoa_env_thresh) &
                                    pcoa_envfit_df$R2 > (input$pcoa_env_R_thresh))
    pcoa_envfit_df_filt
  })
  
  
  #Fit taxonomy abundances to PCoA data
  bc_taxonomy_scores <- reactive({
    req(input$pcoa_start)
    meta_data_table <- bc_pcoa_metafilt()
    feature_taxonomy_labels <- data_labels_react()
    pcoa_srs_prop <- bc_srs_prop_re()
    pcoa_result <- bc_pcoa_react()
    
    t_pcoa_srs_prop <- t(pcoa_srs_prop)
    
    #Use the wascores function to calculated weighted average scores 
    taxon_weighted_scores <-
      wascores(pcoa_result$vectors[, 1:3], t_pcoa_srs_prop)
    taxon_weighted_scores[is.na(taxon_weighted_scores)] <- 0 # remove NA values
    
    # Calculate normalized, total abundance of each taxa
    taxa_total_abundance <-
      sum(t_pcoa_srs_prop) #total number of asvs in the table
    taxa_count <-
      apply(t_pcoa_srs_prop, 2, sum) #total number of each asv across all samples
    normalized_taxa_count <-
      as.data.frame(taxa_count / taxa_total_abundance)
    colnames(normalized_taxa_count) <- "abundance"
    
    taxon_weighted_scores <- as.data.frame(cbind(
      taxon_weighted_scores,
      normalized_taxa_count$abundance
    )) #Append the abundance information
    colnames(taxon_weighted_scores) <- c("Axis1", "Axis2", "Axis3", "Abundance") 
    taxon_weighted_scores <- filter(
      taxon_weighted_scores,
      taxon_weighted_scores$Abundance > (input$pcoa_taxa_thresh) / 100
    ) # Filter taxonomy abundance based on a threshold
    taxon_weighted_scores$FeatureID <- rownames(taxon_weighted_scores)
    feature_taxonomy_labels$FeatureID <- rownames(feature_taxonomy_labels)
    
    ## Add taxaname based on featureID:
    taxon_weighted_scores <- merge(taxon_weighted_scores, feature_taxonomy_labels, by = "FeatureID")
    taxon_weighted_scores
    
  })
  
  #Generate the PCoA plot visual
  bc_pcoa_plot_react <- reactive({
    req(input$pcoa_start)
    pcoa_df <- pcoa_coords_react()
    meta_data_table <- bc_pcoa_metafilt()
    pcoa_envfit_df_filt <- pcoa_envfit_df_filt_re()
    eigen_df <- pcoa_eigen_react()
    taxon_weighted_scores <- bc_taxonomy_scores()
    
    # Insert a column of the sample names
    pcoa_df$SampleName <- rownames(pcoa_df)
    # Merge metadata with PCoA dataframe based on row names
    merged_df <-
      left_join(pcoa_df, meta_data_table, by = "SampleName")
    # Change all metadata into categorical data
    merged_df <-
      merged_df %>% mutate_if(!names(.) %in% c("PCoA1", "PCoA2"), factor)
    
    # Define the available shapes and colors
    # I need to add the option to include a gradient
    available_shapes <- c(21, 22, 23, 24, 25, 14, 13:1)
    available_colors <- 2:27
    available_fill <- 2:27
    
    #If the user selects shape options
    if ((input$shape_choice) == TRUE){
      pcoa_plot <- ggplot(merged_df, aes(x = PCoA1, y = PCoA2)) +
        geom_point(size = input$pcoa_size_select, aes(
          shape = get(isolate(input$pcoa_shape)),
          colour = get(input$pcoa_fill_col),
          fill = get(input$pcoa_fill_col))) + 
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
          shape = isolate(input$pcoa_shape),
        ) +
        scale_shape_manual(values = available_shapes)
      
      #Uses a colour gradient if selected
      if (input$pcoa_gradient == TRUE){
        pcoa_plot <- pcoa_plot + scale_colour_viridis(option = input$pcoa_pallet_selection,discrete = TRUE, direction = -1)
        pcoa_plot <- pcoa_plot + scale_fill_viridis(option = input$pcoa_pallet_selection,discrete = TRUE, direction = -1)
      } else {
        pcoa_plot <- pcoa_plot + scale_fill_manual(values = available_fill) +
          scale_colour_manual(values = available_fill)
      }
    }
    
    #If the user does not select shapes
    if ((input$shape_choice) == FALSE){
      
      pcoa_colour_name <- input$pcoa_fill_col
      pcoa_plot <- ggplot(merged_df, aes(x = PCoA1, y = PCoA2)) +
        geom_point(size = input$pcoa_size_select, aes(
          colour = get(input$pcoa_fill_col),
          fill = get(input$pcoa_fill_col)
        )) +
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
          colour = input$pcoa_fill_col
        )
      
      ## If colour gradient is selected
      if (input$pcoa_gradient == TRUE){
        pcoa_plot <- pcoa_plot + scale_colour_viridis(option = input$pcoa_pallet_selection,discrete = TRUE, direction = -1)
        pcoa_plot <- pcoa_plot + scale_fill_viridis(option = input$pcoa_pallet_selection,discrete = TRUE, direction = -1)
      } else {
        pcoa_plot <- pcoa_plot + scale_fill_manual(values = available_fill) +
          scale_colour_manual(values = available_fill)
      }
    } #end of shape and colour settings
    
    #Add coordinates and line segments for the environmental data, but only if it is present 
    if (dim(pcoa_envfit_df_filt)[1] != 0) {
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
        )}
        # geom_label(
        #   data = pcoa_envfit_df_filt,
        #   aes(
        #     label = pvalue,
        #     x = pcoa_envfit_df_filt$axis1 / 3,
        #     y = pcoa_envfit_df_filt$axis2 / 3
        #   )
        # ) +
        # geom_label(
        #   data = pcoa_envfit_df_filt,
        #   aes(
        #     label = R2_rounded,
        #     x = pcoa_envfit_df_filt$axis1 / 3,
        #     y = pcoa_envfit_df_filt$axis2 / 3
        #   ),
        #   hjust = 2
        # ) + 
        # geom_label(
        #   data = pcoa_envfit_df_filt,
        #   aes(
        #     label = "R2value",
        #     x = pcoa_envfit_df_filt$axis1 / 3,
        #     y = pcoa_envfit_df_filt$axis2 / 3
        #   ),
        #   hjust = 3
        # )}
    
    
    
    #If sample labels are selected:
    if (input$pcoa_sample_labels == TRUE){
      pcoa_plot <- pcoa_plot +
        geom_text(aes(label = SampleName))
    }
    
    
    #Add elipses to the data
    if ((input$pcoa_elips) == TRUE) {
      
      pcoa_plot <-
        pcoa_plot + stat_ellipse(aes(color = get(input$pcoa_fill_col)),
                                 show.legend = FALSE)
    }
    
    # Add taxon abundance data, but only if it is present
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
        labs(size = "Relative abundance")+
        scale_size_area(max_size = 15)
    }
    
    if (dim(taxon_weighted_scores)[1] != 0) {
      # Add taxon annotation
      pcoa_plot <- pcoa_plot + geom_text(
        data = taxon_weighted_scores,
        aes(Axis1, Axis2, label = TaxaName),
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
      ) +
    guides(fill = guide_legend(override.aes = list(shape = 22)))
    
    # Plot PCoA
    pcoa_plot
  })
  
  
  ## Add options to download the important data sheets for manual stats calculations
  output$pcoa_envfit_table <- downloadHandler(
    filename = "pcoa_envfit_table.csv",
    content = function(pcoa_envfit_table) {
      write.csv(bc_pcoa_envfit_re(), pcoa_envfit_table)
    })
  
  output$pcoa_envfit_filt_table <- downloadHandler(
    filename = "pcoa_envfit_filt_table.csv",
    content = function(pcoa_envfit_filt_table) {
      write.csv(pcoa_envfit_df_filt_re(), pcoa_envfit_filt_table)
    })
  
  ## Adjusting the PCoA image and saving
  ## You must define the input for width/height within a reactive context, then call it in the output.
  pcoa_plot_width <- reactive(input$pcoa_plot_outw)
  pcoa_plot_height <- reactive(input$pcoa_plot_outh)
  
  output$pcoa_plot_out <- renderPlot({
    req(input$pcoa_start)
    pcoa_plot <- bc_pcoa_plot_react()
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
        plot = bc_pcoa_plot_react(),
        device = "pdf",
        height = as.numeric(input$pcoa_plot_outh),
        width = as.numeric(input$pcoa_plot_outw),
        units = "px",
        scale = 4
      )
    })
  
  output$pcoa_stats_table <- renderDataTable({
    bray_stats <- bc_pcoa_envfit_re()
    # output$proc_alttext <- renderText("This is your stats data")
    bray_stats
  })
  
  
  
  #### UniFrac Triplot ####
  ## Update selection - UniFrac PCoA plot
  observe({
    req(input$meta_file)
    
    meta_datafile <- meta_datafile()
    meta_colnames <- c(colnames(meta_datafile), "TaxaName")
    meta_colnames <- meta_colnames[meta_colnames != "SampleName"]
    
    updateSelectInput(session, "uni_pcoa_fill_col", choices = sort(meta_colnames))
    updateSelectInput(session, "uni_pcoa_elips_col", choices = sort(meta_colnames))
    updateSelectInput(session, "uni_pcoa_shape", choices = sort(meta_colnames))
    updateSelectInput(session, "uni_pcoa_fill_col", choices = sort(meta_colnames))
    updateSelectInput(session, "uni_pcoa_elips_col", choices = sort(meta_colnames))
    updateSelectInput(session, "uni_pcoa_shape", choices = sort(meta_colnames))
    updateSelectInput(session, "uni_pcoa_pallet_selection")
    updateSelectInput(session, "uni_pcoa_gradient")
  })
  
  
  
  ## Import the data
  uni_data_tree_react <- reactive({
    req(input$uni_pcoa_start)
    req(input$unifrac_tree)
    # req(input$main_file)
    # req(input$meta_file)
    read.tree(
      file = input$unifrac_tree$datapath
    )
  })
  
  #Testing optional metadata file upload. Also needs to filter missing samples
  uni_metadata_table <- reactive({
    req(input$uni_pcoa_start)
    req(input$uni_metadata)
    meta_data_table <- read.table(
      file = input$uni_metadata$datapath,
      fill = TRUE,
      header = TRUE,
      sep = "\t"
    )
    # srs_table <- bc_pcoa_react
    # pcoa_filt_colnames <- colnames(srs_table)
    # meta_data_table <-
    #   meta_data_table %>% filter(SampleName %in% pcoa_filt_colnames)
    # meta_data_table
    meta_data_table
  })
  
  # uni_asv_filter <- reactive({
  #   main_data_table <- data_tran_contam_filt_react()
  #   meta_data_table <- uni_metadata_table()
  #   
  #   meta_names <-
  #     c(
  #       meta_data_table$SampleName,
  #       "Consensus.Lineage",
  #       "rowID",
  #       "Feature.ID",
  #       "ReprSequence"
  #     )
  #   main_data_table <-
  #     main_data_table[, names(main_data_table) %in% meta_names]
  #   main_data_table
  #   
  # })
  
  
  # main_data_table <- read.table("Alex test/UniFrac/fishbiomass_asv.tsv", header = TRUE, sep = "\t")
  # data_tree <- read.tree("Alex test/UniFrac/fishbiomass_rooted_tree.nwk")
  # meta_data_table <- read.table("Alex test/UniFrac/2022_10_metadata_noNTC_noculture.tsv", header = TRUE, sep ="\t")
  
  ## Modify the ASV table and create a proportion table (see above scriptss). We actually need to keep the Feature.ID stored so we can put them back in. So long as we keep the Feature.ID and taxonomy linked, we can do what we need to. 
  
  # Forgot I need to rarify with SRS first:
  uni_srs_table_re <- reactive({
    req(input$uni_pcoa_start)
    main_data_table <- main_datafile()
    meta_data_table <- meta_datafile()
    feature_taxonomy_labels <- data_labels_react()
    req(input$unifrac_tree)
    # req(unifrac_tree)
    
    srs_table <- main_data_table[, !colnames(main_data_table) %in% c("Consensus.Lineage",
                                                                     "Feature.ID",
                                                                     "rowID",
                                                                     "ReprSequence",
                                                                     "last_taxon")]
    srs_table <- SRS(srs_table, Cmin = isolate(input$uni_srs_depth), seed = 231)
    rownames(srs_table) <- rownames(feature_taxonomy_labels)
    srs_table
  })
  
  #Create a proportion table
  uni_srs_prop_table_re <- reactive({
    req(input$uni_pcoa_start)
    srs_table <- uni_srs_table_re()
    
    proportion_table <- srs_table / colSums(srs_table)
    proportion_table
  })
  
  #Filter the metadata table
  uni_metadata_filt_react <- reactive({
    req(input$uni_pcoa_start)
    srs_table <- uni_srs_table_re()
    meta_data_table <- meta_datafile()
    
    # Collect the column names after SRS rarefaction
    pcoa_filt_colnames <- colnames(srs_table)
    
    # # Filter the metadata table to remove samples no longer present after SRS
    meta_data_table <-
      meta_data_table %>% filter(SampleName %in% pcoa_filt_colnames)
    meta_data_table
  })
  
  #Generate the UniFrac distances; must use a transposed proportion table
  uni_diss_react <- reactive({
    req(input$uni_pcoa_start)
    proportion_table <- uni_srs_prop_table_re()
    data_tree <- uni_data_tree_react()
    proportion_table <- t(proportion_table)
    uni_diss <- GUniFrac(proportion_table, data_tree, alpha = c(0,0.5,1))$unifracs
    uni_diss
  })
  
  #Separate the weightd UniFrac; d_1 is weighted
  uni_diss_react_w <- reactive({
    req(input$uni_pcoa_start)
    w_uni_diss <- uni_diss_react()
    w_uni_diss <- w_uni_diss[, , "d_1"]
    w_uni_diss
  })
  
  #Separate the weighted UniFrac; d_UW unweighted
  uni_diss_react_uw <- reactive({
    req(input$uni_pcoa_start)
    w_uni_diss <- uni_diss_react()
    w_uni_diss <- w_uni_diss[, , "d_UW"]
    w_uni_diss
  })
  
  #Generate the PcoA for the weighted UniFrac
  uni_weighted_pcoa_react <- reactive({
    req(input$uni_pcoa_start)
    uni_weighted_diss_w <- uni_diss_react_w()
    
    pcoa_result <- ape::pcoa(uni_weighted_diss_w, correction = "cailliez")
    pcoa_result
  })
  
  #Generate the PcoA for the unweighted UniFrac
  uni_unweighted_pcoa_react <- reactive({
    req(input$uni_pcoa_start)
    uni_unweighted_diss_uw <- uni_diss_react_uw()
    
    pcoa_result <- ape::pcoa(uni_unweighted_diss_uw, correction = "cailliez")
    pcoa_result
  })
  
  #Capture the first and second axis coordinates for the weighted UniFrac
  uni_w_pcoa_coords_react <- reactive({
    req(input$uni_pcoa_start)
    pcoa_result <- uni_weighted_pcoa_react()
    
    
    
    pcoa_coords <- pcoa_result$vectors[, 1:2]
    pcoa_df <-
      data.frame(
        PCoA1 = pcoa_coords[, 1],
        PCoA2 = pcoa_coords[, 2],
        row.names = row.names(pcoa_coords)
      )
    pcoa_df
  })
  
  #Capture the first and second axis coordinates for the Unweighted UniFrac
  uni_uw_pcoa_coords_react <- reactive({
    req(input$uni_pcoa_start)
    pcoa_result <- uni_unweighted_pcoa_react()
    
    pcoa_coords <- pcoa_result$vectors[, 1:2]
    pcoa_df <-
      data.frame(
        PCoA1 = pcoa_coords[, 1],
        PCoA2 = pcoa_coords[, 2],
        row.names = row.names(pcoa_coords)
      )
    pcoa_df
  })
  
  #Capture the Eigen values for the weighted UniFrac. 
  #Note: depending on seemingly nothing, the column for these values changes names and so don't always capture properly. An alternative is to select the column using column IDs (i.e., 1, 2). Assuming that it always is the third column.
  uni_w_eigen_react <- reactive({
    req(input$uni_pcoa_start)
    pcoa_result <- uni_weighted_pcoa_react()
    
    eigenvalues <- pcoa_result$values[3]
    Axis1 <- eigenvalues[1,] * 100
    Axis2 <- eigenvalues[2,] * 100
    eigen_df <- data.frame(Axis1 = Axis1, Axis2 = Axis2)
    eigen_df
    
  })
  
  #Capture eigen values for the unweighted UniFrac
  uni_uw_eigen_react <- reactive({
    req(input$uni_pcoa_start)
    pcoa_result <- uni_unweighted_pcoa_react()
    
    eigenvalues <- pcoa_result$values[3]
    Axis1 <- eigenvalues[1,] * 100
    Axis2 <- eigenvalues[2,] * 100
    eigen_df <- data.frame(Axis1 = Axis1, Axis2 = Axis2)
    eigen_df
    
  })
  
  #Fit the environmental variables to the PCoA for the unweighted UniFrac. 
  uni_envfit_react <- reactive({
    req(input$uni_pcoa_start)
    meta_data_table <- uni_metadata_filt_react()
    
    pcoa_vectors <- as.data.frame(pcoa_result$vectors)
    
    # meta_data_table <- meta_datafile()
    if (input$uni_diss_select == "unweighted"){
      pcoa_result <- uni_unweighted_pcoa_react()
      pcoa_envfit <- envfit(pcoa_vectors, meta_data_table, perm = 10000)
      ## Scales the arrow vectors so they aren't huge
      pcoa_envfit_df <- as.data.frame(pcoa_envfit$vectors$arrows * sqrt(pcoa_envfit$vectors$r))
      pcoa_envfit_df <- cbind(pcoa_envfit_df, pcoa_envfit$vectors$r)
      pcoa_envfit_df <- cbind(pcoa_envfit_df, pcoa_envfit$vectors$pvals)
      colnames(pcoa_envfit_df) <- c("axis1", "axis2", "R2", "pvalue")
      pcoa_envfit_df$R2_rounded <- round(pcoa_envfit_df$R2, 2)
      pcoa_envfit_df$pvalue <- round(pcoa_envfit_df$pvalue, 4)
      # pcoa_envfit_df$pvaluecorr <- pcoa_envfit_df$pvalue*100
      # colnames(pcoa_envfit_df) <- c("axis1", "axis2", "R", "pvalue","pcorrected")
      pcoa_envfit_df
    }
    #Fit the environmental variables to the PCoA for the weighted UniFrac
    else if (input$uni_diss_select == "weighted"){
      pcoa_result <- uni_weighted_pcoa_react()
      pcoa_envfit <- envfit(pcoa_vectors, meta_data_table, perm = 10000)
      ## Scales the arrow vectors so they aren't huge
      pcoa_envfit_df <- as.data.frame(pcoa_envfit$vectors$arrows * sqrt(pcoa_envfit$vectors$r))
      pcoa_envfit_df <- cbind(pcoa_envfit_df, pcoa_envfit$vectors$r)
      pcoa_envfit_df <- cbind(pcoa_envfit_df, pcoa_envfit$vectors$pvals)
      colnames(pcoa_envfit_df) <- c("axis1", "axis2", "R2", "pvalue")
      pcoa_envfit_df$R2_rounded <- round(pcoa_envfit_df$R2, 2)
      pcoa_envfit_df$pvalue <- round(pcoa_envfit_df$pvalue, 4)
      # pcoa_envfit_df$pvaluecorr <- pcoa_envfit_df$pvalue*100
      # colnames(pcoa_envfit_df) <- c("axis1", "axis2", "R", "pvalue","pcorrected")
      pcoa_envfit_df
    }
  })
  
  #Filter the environmental statistics based on R or p-values.
  uni_envfit_filt_react <- reactive({
    req(input$uni_pcoa_start)
    pcoa_envfit_df <- uni_envfit_react()
    
    pcoa_envfit_df_filt <- filter(pcoa_envfit_df,
                                  pcoa_envfit_df$pvalue < input$uni_env_thresh &
                                    pcoa_envfit_df$R2 > input$uni_env_r_thresh)
    pcoa_envfit_df_filt
  })
  
  #Fit taxonomy abundances to PCoA data
  uni_taxonomy_scores <- reactive({
    req(input$uni_pcoa_start)
    meta_data_table <- uni_metadata_filt_react()
    feature_taxonomy_labels <- data_labels_react()
    pcoa_srs_prop <- uni_srs_prop_table_re()
    
    if (input$uni_diss_select == "unweighted"){
      pcoa_result <- uni_unweighted_pcoa_react()
      pcoa_df <- uni_uw_pcoa_coords_react()
      pcoa_result
    } 
    
    else if (input$uni_diss_select == "weighted"){
      pcoa_result <- uni_weighted_pcoa_react()
      pcoa_df <- uni_w_pcoa_coords_react()
      pcoa_result
    }
    
    #Use the wascores function to calculated weighted average scores
    t_pcoa_srs_prop <- t(pcoa_srs_prop)
    taxon_weighted_scores <-
      wascores(pcoa_result$vectors[, 1:3], t_pcoa_srs_prop)
    taxon_weighted_scores[is.na(taxon_weighted_scores)] <- 0 # remove NA values
    
    # Calculate normalized, total abundance of each taxa
    taxa_total_abundance <-
      sum(t_pcoa_srs_prop) #total number of asvs in the table
    taxa_count <-
      apply(t_pcoa_srs_prop, 2, sum) #total number of each asv across all samples
    normalized_taxa_count <-
      as.data.frame(taxa_count / taxa_total_abundance)
    colnames(normalized_taxa_count) <- "abundance"
    
    taxon_weighted_scores <- as.data.frame(cbind(
      taxon_weighted_scores,
      normalized_taxa_count$abundance
    )) #Append the abundance information
    colnames(taxon_weighted_scores) <- c("Axis1", "Axis2", "Axis3", "Abundance") 
    taxon_weighted_scores <- filter(
      taxon_weighted_scores,
      taxon_weighted_scores$Abundance > input$uni_taxa_thresh / 100
    ) # Filter taxonomy abundance based on a threshold
    taxon_weighted_scores$FeatureID <- rownames(taxon_weighted_scores)
    feature_taxonomy_labels$FeatureID <- rownames(feature_taxonomy_labels)
    
    ## Add taxaname based on featureID:
    taxon_weighted_scores <- merge(taxon_weighted_scores, feature_taxonomy_labels, by = "FeatureID")
    taxon_weighted_scores
  })
  
  #Merge the final dataframes
  uni_merged_df_react <- reactive({
    req(input$uni_pcoa_start)
    meta_data_table <- uni_metadata_filt_react()
    if (input$uni_diss_select == "unweighted"){
      pcoa_df <- uni_uw_pcoa_coords_react()
    } 
    
    else if (input$uni_diss_select == "weighted"){
      pcoa_df <- uni_w_pcoa_coords_react()
    }
    # Insert a column of the sample names
    pcoa_df$SampleName <- rownames(pcoa_df)
    # Merge metadata with PCoA dataframe based on row names
    merged_df <-
      left_join(pcoa_df, meta_data_table, by = "SampleName")
    # Change all metadata into categorical data
    merged_df <-
      merged_df %>% mutate_if(!names(.) %in% c("PCoA1", "PCoA2"), factor)
    merged_df
  })
  
  #Plot the triplot
  uni_triplot_react <- reactive({
    req(input$uni_pcoa_start)
    merged_df <- uni_merged_df_react()
    meta_data_table <- uni_metadata_filt_react()
    pcoa_envfit_df_filt <- uni_envfit_filt_react()
    
    if (input$uni_diss_select == "weighted"){
      eigen_df <- uni_w_eigen_react()
    } 
    
    if (input$uni_diss_select == "unweighted"){
      eigen_df <- uni_uw_eigen_react()
    } 
    
    available_shapes <- c(21, 22, 23, 24, 14, 13:1)
    available_colors <- 2:27
    available_fill <- 2:27
    
    # Plot PCoA using ggplot2 with shape and color based on metadata
    if (input$uni_shape_choice == TRUE){
      pcoa_plot <- ggplot(merged_df, aes(x = PCoA1, y = PCoA2)) +
        geom_point(size = input$uni_pcoa_size_select, aes(
          shape = get(input$uni_pcoa_shape),
          colour = get(input$uni_pcoa_fill_col),
          fill = get(input$uni_pcoa_fill_col))) + 
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
          shape = input$uni_pcoa_shape,
        ) +
        scale_shape_manual(values = available_shapes)
      
      ## IF selected, allows you to colour based on a viridis gradient. Right now it requires rechecking the box to switch between gradients. I'll have to fix this in the future
      if (input$uni_pcoa_gradient == TRUE){
        pcoa_plot <- pcoa_plot + scale_colour_viridis(option = input$uni_pcoa_pallet_selection,discrete = TRUE, direction = -1)
        pcoa_plot <- pcoa_plot + scale_fill_viridis(option = input$uni_pcoa_pallet_selection,discrete = TRUE, direction = -1)
      } else {
        pcoa_plot <- pcoa_plot + scale_fill_manual(values = available_fill) +
          scale_colour_manual(values = available_fill)
      }
    } #End of shape selection
    
    #If the user doesn't select shape options
    if (input$uni_shape_choice == FALSE){
      # Collect the names of the metadata category selected
      pcoa_colour_name <- input$uni_pcoa_fill_col
      pcoa_plot <- ggplot(merged_df, aes(x = PCoA1, y = PCoA2)) +
        geom_point(size = input$uni_pcoa_size_select, aes(
          colour = get(input$uni_pcoa_fill_col),
          fill = get(input$uni_pcoa_fill_col)
        )) +
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
        )
      
      #Setting a colour gradient, if selected
      if (input$uni_pcoa_gradient == TRUE){
        pcoa_plot <- pcoa_plot + scale_colour_viridis(option = input$uni_pcoa_pallet_selection,discrete = TRUE, direction = -1)
        pcoa_plot <- pcoa_plot + scale_fill_viridis(option = input$uni_pcoa_pallet_selection,discrete = TRUE, direction = -1)
      } else {
        pcoa_plot <- pcoa_plot + scale_fill_manual(values = available_fill) +
          scale_colour_manual(values = available_fill)
      }
      
    }# End of general PCoA settings
    
    
    #Adding on environmental data via arrows only if the dataframe is not empty after filtering
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
        ) +
        geom_label(
          data = pcoa_envfit_df_filt,
          aes(
            label = pvalue,
            x = pcoa_envfit_df_filt$axis1 / 3,
            y = pcoa_envfit_df_filt$axis2 / 3
          )
        ) +
        geom_label(
          data = pcoa_envfit_df_filt,
          aes(
            label = R2_rounded,
            x = pcoa_envfit_df_filt$axis1 / 3,
            y = pcoa_envfit_df_filt$axis2 / 3
          ),
          hjust = 2
        ) + 
        geom_label(
          data = pcoa_envfit_df_filt,
          aes(
            label = "R2value",
            x = pcoa_envfit_df_filt$axis1 / 3,
            y = pcoa_envfit_df_filt$axis2 / 3
          ),
          hjust = 3
        )
    }
    
    
    # If sample labels are selected:
    if (input$uni_pcoa_sample_labels == TRUE){
      pcoa_plot <- pcoa_plot +
        geom_text(aes(label = SampleName))
    }
    
    
    #Adding elipses around the PCoA points depending on colour setting
    if (input$uni_pcoa_elips == TRUE) {
      pcoa_plot <-
        pcoa_plot + stat_ellipse(aes(color = get(input$uni_pcoa_fill_col)),
                                 show.legend = FALSE)
      
    }
    
    #Layer on the taxon data, only if the dataframe isn't empty after filtering
    taxon_weighted_scores <- uni_taxonomy_scores()
    
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
        labs(size = "Relative abundance")+
        scale_size_area(max_size = 15)
    }
    
    if (dim(taxon_weighted_scores)[1] != 0) {
      # Add taxon annotation
      pcoa_plot <- pcoa_plot + geom_text(
        data = taxon_weighted_scores,
        aes(Axis1, Axis2, label = TaxaName),
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
      ) +
    guides(fill = guide_legend(override.aes = list(shape = 22)))
    
    # Plot PCoA
    pcoa_plot
  })
  
  
  ## Add options to download the important data sheets for manual stats calculations
  output$uni_stats_full <- downloadHandler(
    filename = "pcoa_envfit_table.csv",
    content = function(uni_envfit_table) {
      write.csv(uni_envfit_react(), uni_envfit_table)
    })
  
  output$uni_stats_filtered <- downloadHandler(
    filename = "pcoa_envfit_filt_table.csv",
    content = function(uni_envfit_filt_table) {
      write.csv(uni_envfit_filt_react(), uni_envfit_filt_table)
    })
  
  
  ## Adjusting the PCoA image and saving
  ## You must define the input for width/height within a reactive context, then call it in the output.
  uni_pcoa_plot_width <- reactive(input$uni_pcoa_plot_outw)
  uni_pcoa_plot_height <- reactive(input$uni_pcoa_plot_outh)
  
  output$uni_pcoa_plot_out <- renderPlot({
    req(input$uni_pcoa_start)
    pcoa_plot <- uni_triplot_react()
    pcoa_plot
  },
  width = uni_pcoa_plot_width,
  height = uni_pcoa_plot_height)
  
  output$uni_pcoa_download <- downloadHandler(
    filename = "uni_pcoa_plot.pdf",
    contentType = ".pdf",
    content = function(pcoa_file) {
      ggsave(
        pcoa_file,
        plot = uni_triplot_react(),
        device = "pdf",
        height = as.numeric(input$uni_pcoa_plot_outh),
        width = as.numeric(input$uni_pcoa_plot_outw),
        units = "px",
        scale = 4
      )}
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
      round(data_long_bubble$Percentage, digits = as.numeric(isolate(input$b1_num_dec)))
    
    
    
    
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
    # if (is.na(isolate(input$b1_meta_group)) == TRUE){
    #   warning("No metadata category selected. Script will continue without filtering by groups.")
    # } else {
    #   sample_hits <- grepl(pattern = isolate(input$b1_meta_keyword), ignore.case = TRUE, x = (eval(parse(text=paste("data_long_bubble$",isolate(input$b1_meta_group))))))
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
      data_long_bubble <-
        data_long_bubble[with(data_long_bubble,
                              order(eval(parse(
                                text = isolate(input$b1_tax_sort)
                              )), TaxaName, decreasing = TRUE)), ]
      data_long_bubble$TaxaName <-
        as.character(data_long_bubble$TaxaName)
      data_long_bubble$TaxaName <-
        factor(data_long_bubble$TaxaName,
               levels = unique(data_long_bubble$TaxaName))
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
        
        # aes(x = reorder_within(x = SampleName, by = eval(parse(text=input$b1_sort_axis)),within = eval(parse(text=isolate(input$b1_sort_param)))),
        
        # x = if(!is.na(isolate(input$b1_sort_param))){
        #   x = reorder_within(x = SampleName, by = eval(parse(text=input$b1_sort_axis)),within = eval(parse(text=isolate(input$b1_sort_param))))
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
            eval(parse(text = isolate(input$b1_ab_thresh))),
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
            eval(parse(text = isolate(input$b1_ab_thresh))),
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
          strip.background.y = element_rect(fill = "white"),
          strip.background.x = element_rect(fill = "white"),
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
          strip.background.y = element_rect(fill = "white"),
          strip.background.x = element_rect(fill = "white"),
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
  output$helpme = renderUI({
    tags$iframe(src = "helpme.pdf", style = "height: 800px; width: 100%")
  })
  
  
} # End of server

## The server launch ## KEEP THIS COMMENTED (unless you need to uncomment it?). In the past, this had to be uncommented to run in RStudio but would crash ShinyServer.
#shinyApp(ui = ui, server = server)
