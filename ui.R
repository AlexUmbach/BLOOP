## LIBRARIES:-
library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(ecodist)
library(tidyr)
library(forcats)
library(vegan)
library(ape)
library(ade4)
library(gtools)
library(SRS)
library(vroom)
library(reactable)
library(shinycssloaders)
library(shinydashboard)
library(stringi)
library(shinyWidgets)
library(fresh)
library(htmltools)
library(tidytext)
library(ggh4x)

## Set working directory. This should be the directory where app.R is stored. 
#setwd("C:/Users/alexu/OneDrive/UW/Grad Studies/R workspace/R Scripts//ALEXIOME/ALEXIOME/")
#gc()


#### Website text ####
# This section contains the website text, integrated into the individual panels on the website. I thought it easier to define them here,
# rather than having to edit them within the code. 

# main page
Rversion = R.Version()$version.string
bloop_welcome = "BLOOP (working title) contains a series of R-based visualization scripts wrapped within an R Shiny UI, created by Alex Umbach and the Neufeld Lab at the University of Waterloo. Its intended purpose is to allow for real-time figure generation with minimal user input, all generated from an ASV/OTU and metadata table. BLOOP also offers real-time customization of figures with easy to use UI inputs and sliders that can allow you to make quick changes and (near) instantly view the results without ever having to touch a piece of code."


bloop_req = "BLOOP requires an ASV/OTU table produced from sequence data processing pipelines (i.e., QIIME2) using the QIIME2 taxonomy formatting. Any phylogenetic marker gene can be used (i.e., cpn60, amoA) so long as the taxonomy formatting is identical. All uploaded files must be in TSV format. Your ASV/OTU table should be formatted as detailed in the example picture below:"
# asv_example
# meta_example


## This is some custom HTML settings for colour, background, textsize, and whatnot:
ui <- navbarPage(id = "navbarID",
                 setBackgroundColor(color = "#E7E7E7"),
                 tags$style(HTML("
        .navbar-default .navbar-brand {color:white;font-size:28px}
        .navbar-default .navbar-brand:hover {color:white;}
        .navbar { background-color:#3C8DBC;}
        .navbar-default .navbar-nav > li > a {color:white;font-size:22px;black}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color:black;background-color:white;}
        .navbar-default .navbar-nav > li > a:hover {color:black;background-color:white;text-decoration}
        .well {background:white;}


                  ")),
        
        title = "BLOOP v2.4",
        
        #title = img(src="ALEXIOME_logo.png", height = "100%", width = "100%"),
        
        #### Main Page ####
        tabPanel("WELCOME",
                 sidebarLayout(
                   sidebarPanel(Rversion,
                                img(src="bloop_logo.png", height = "60%", width = "100%"),
                                #h1("WOW!"),
                                #img(src="eric-head.gi", height = "100%", width = "100%"),
                                style = "overflow-y:scroll; height: 850px; position:relative; border-color:#000000",
                                width = 3
                   ),
                   mainPanel(width = 9,
                             fluidRow(
                               box(h1(em("Welcome to BLOOP!")),
                                   p(bloop_welcome),
                                   p(h1(em("What kind of data are required?"))),
                                   p(bloop_req),
                                   width = 12,
                                   #style = "background-color:#FFFFFF; border-color:#000000; border-style: solid; margin-left:0px; margin-right:30px; padding: 10px",
                                   style = "background-color:#FFFFFF; border-color:#000000; border-style: solid; border-width: 1.5px; margin-left:0px; margin-right:30px; padding: 10px",
                               ),
                             ),
                             br(),
                             box(
                               p("This is how your ASV/OTU table should be formatted. The column",strong("Feature.ID"),"is required and must contain ASV IDs or a list of unique identifiers"),
                               p("ASV/OTU tables can also be in a collapsed format, where the 'Feature.ID' column contains taxonomy. The",strong("Consensus.Lineage"),"and",strong("ReprSequence"),"headers must be as shown. The sample names are not restricted."),
                               img(src="ASV_example.png",height = "100%", width = "100%"),
                               width = 13,
                               style = "background-color:#FFFFFF; border-color:#000000; border-style: solid; border-width: 1.5px; margin-left:0px; margin-right:30px; padding: 10px",
                             ),
                             br(),
                             box(
                               p("Your metadata file should appear as below. There is a single required feature of your metadata table: a column labelled",strong("SampleName."),
                                 "The 'SampleName' column should include a list of sample names that are",strong("identical"), "to the samples in your ASV/OTU table. Any additional columns containing sample information (e.g., temperature, location, group) can be included and will be incorporated as options during analyses."),
                               img(src="meta_example.png",height = "25%", width = "25%"),
                               width = 13,
                               style = "background-color:#FFFFFF; border-color:#000000; border-style: solid; border-width: 1.5px; margin-left:0px; margin-right:30px; padding: 10px",
                             ),
                             br(),
                             box(
                               p(h1("Column name blacklist")),
                               p("BLOOP requires priority when assigning column names. Because of this, you",strong("must not use"),"the following names in your metadata:"),
                               p("TaxaName"),
                               p("input"),
                               p("Feature ID (or variants"),
                               p("rowID (or variants)"),
                               p("Taxonomy (or variants)"),
                               width = 13,
                               style = "background-color:#FFFFFF; border-color:#000000; border-style: solid; border-width: 1.5px; margin-left:0px; margin-right:30px; padding: 10px",
                             )
                             
                   )
                 )
        ),
        
        
        
        #### Upload Data and review ####
        tabPanel("Upload data",
                 sidebarLayout(
                   sidebarPanel(h5("Please upload your data"),
                                fileInput("main_file","Primary ASV table"),
                                radioButtons("is_main_collapsed","Is this a collapsed table?",c("Yes","No"),selected = "No", inline = TRUE),
                                #radioButtons("main_filetype","Please select your filetype",c("tsv","csv","txt")),
                                fileInput("meta_file","Metadata table"),
                                #radioButtons("meta_filetype","Please select your filetype",c("tsv","csv","txt")),
                                fileInput("contam_file","ASV contaminant list"),
                                #radioButtons("contam_filetype","Please select your filetype",c("tsv","csv","txt"))
                                width = 3,
                                style = "overflow-y:scroll; max-height: 850px; position:relative; border-color:#000000"
                   ),
                   mainPanel(width = 9,
                             fluidRow(
                               box(h3(em("Upload your data")),
                                   p("This is where you can upload your ASV tables, metadata table, and",em("optional"),"contaminant list. Once you have uploaded your ASV table, indicate whether it is a true ASV table or a collapsed taxon table."
                                   ),
                                   width = 12,
                               ),
                               
                               
                               
                               style = "background-color:#FFFFFF; border-color:#000000; border-style: solid; border-width: 1.5px; margin-left:0px; margin-right:30px; padding: 10px",
                               width = 6,        
                             ),
                             textOutput("maintext"),
                             dataTableOutput("main_table"),
                             textOutput("metatext"),
                             dataTableOutput("meta_table"),
                             textOutput("contamtext"),
                             dataTableOutput("contam_table"),
                             style = "overflow-y:scroll; max-height: 850px; position:relative;"
                             
                   )
                 )
        ),
        
        
        #### Processed Data #####
        tabPanel("Processed data",
                 sidebarLayout(
                   sidebarPanel(h4("Select preferences"),
                                radioButtons("remove_prefix","Do you want to remove prefixes?",c("Yes","No"),selected = "Yes", inline = TRUE),
                                radioButtons("truncate_taxa","Do you want to truncate taxa to the most resolved taxon?",c("Yes","No"), selected = "Yes",inline = TRUE),
                                radioButtons("contam_filter","Do you want to remove or analyze contaminants?", c("Remove","Analyze","No action"),selected = "No action",inline = TRUE),
                                width = 3,
                                style = "overflow-y:scroll; max-height: 900px; position:relative;border-color:#000000"
                   ),
                   mainPanel(width = 9,
                             fluidRow(
                               box(h3(em("Processed data review")),
                                   p("This page is where you can review your processed data to ensure that everything is looking as it should.
                                        The table is interactable and searchable and should allow you to cross reference samples and metadata with your original tables if you have any concerns
                                        about whether the data has been processed properly."))),
                             br(),
                             textOutput("proc_maintext",),
                             dataTableOutput("proc_main"),
                             textOutput("proc_alttext"),
                             dataTableOutput("proc_new_data"),
                             textOutput("proc_new_text"),
                             dataTableOutput("proc_main_alt"),
                             style = "overflow-y:scroll; max-height: 800px; position:relative;"
                             
                             
                   )
                 )
        ),
        
        
        
        
        #### Total read plot ####
        tabPanel("Read plot",
                 sidebarLayout(
                   sidebarPanel(h4("Select preferences"),
                                actionButton("read_start",label = "Start!"),
                                radioButtons("box_select","What kind of plot?",choices = c("Box","Bar"),selected = "Bar", inline = TRUE),
                                selectInput("read_sortby_axis","How do you want to group your data?",choices = NULL),
                                #selectInput("read_colour","How do you want to colour your data?",choices = "Updating"),
                                #selectInput("read_meta_group","Select specific metadata group",choices = "Updating"),
                                #textInput("read_meta_key","Provide a keyword"),
                                numericInput("read_yaxis_limit","Change the y-axis limit",value = 100000),
                                #textInput("read_xaxis","Provide an x-axis label",value = "Samples"),
                                #textInput("read_yaxis","Provide a y-axis label",value = "Total reads following DADA2"),
                                #textInput("read_legend_label","Provide a legend title"),
                                radioButtons("read_panel","Do you want panel borders?",c("Yes","No"),selected = "Yes", inline = TRUE),
                                sliderInput("read_panel_spacing","Modify panel spacing",min = 0,max = 20,step = 1,value = 0),
                                #sliderInput("read_width","Change bar width",min=0, max=1, step = 0.1, value = 0.9),
                                sliderInput("read_border_bold","Change border thickness",min=0, max=1, step = 0.1, value = 0.4),
                                sliderInput("read_alpha","Change bar alpha",min=0, max=1, step = 0.1, value = 0.7),
                                #sliderInput("read_xaxis_space","Change x-axis spacing",min = 0, max = 0.5, step = 0.005, value = 0),
                                #sliderInput("read_yaxis_space","Change x-axis spacing",min = 0, max = 0.5, step = 0.005, value = 0),
                                
                                width = 3,
                                style = "overflow-y:scroll; max-height: 850px; position:relative;border-color:#000000"
                                
                   ),
                   mainPanel("",
                             width = 9,
                             dataTableOutput("read_table_out") %>% withSpinner(type = 8, color.background = "white"),
                             fluidRow(
                               # box(
                               #   numericInput("read_plotheight","Select height", value = 6),
                               #   numericInput("read_plotwidth","Select width", value = 12),
                               # ),
                               box(
                                 sliderInput("read_plot_out_w","Plot width",min = 0, max = 2000,step = 100,value = 600),
                                 sliderInput("read_plot_out_h","Plot height",min = 0, max = 2000,step = 100,value = 400),
                                 downloadButton("read_download","Save figure"),
                               ),
                             ),
                             br(),
                             br(),
                             # plotOutput("read_plot"),
                             plotOutput("read_plot") %>% withSpinner(type = 1,color.background = "white",size = 3),
                             style = "overflow-y:scroll; max-height: 850px; position:relative;"
                   )
                 )
        ),
        
        
        #### Taxa bar plot analysis ####
        tabPanel("Taxonomic bar plot",
                 sidebarLayout(
                   sidebarPanel(h4("Select preferences"),
                                actionButton("bar_start",label = "Start!"),
                                sliderInput("bar_cutoff","Select your cutoff",min = 0, max = 100, step = 1, value = 20),
                                # textInput("bar_plot_x",label = "Enter x-axis",value = "SampleName"),
                                # textInput("bar_plot_y",label = "Enter y-axis",value = "Percentage"),
                                selectInput("bar_sortby_xaxis", label = "How do you want to group your samples?",choice = "Updating"),
                                textInput("taxon_subset","Filter for specific taxa (e.g., 'staph')",value = NA),
                                #textInput("bar_plot_fill","Select bar plot fill",value = "TaxaName"),
                                #textInput("bar_xaxis","Provide a x-axis label","Samples"),
                                #textInput("bar_yaxis","Provide a y-axis label","Read proportions (%)"),
                                #textInput("bar_legend_label","Provide a legend title",""),
                                radioButtons("bar_panel_border","Do you want panel borders?",choices = c("Yes","No"),selected = "Yes", inline = TRUE),
                                sliderInput("bar_panel_spacing","Modify panel spacing",min = 0, max = 30, value = 0, step = 1),
                                # selectInput("bar_fill_col",label = "Select bar colour",choices = c("Set1","Set2","Set3","Paired"),selected = "Paired" ),
                                #sliderInput("bar_border_bold","Select border boldness",min = 0, max = 1, step = 0.1, value = 0.5),
                                #sliderInput("bar_width", "select bar width",min= 0,max= 1,step= 0.1,value= 1),
                                sliderInput("bar_alpha","Select your bar alpha",min= 0,max= 1,step= 0.1,value= 0.4),
                                #sliderInput("bar_leftright","Change your vertical border spacing",min = 0,max = 0.1,step = 0.005,value = 0.035),
                                #sliderInput("bar_topbottom","Change your horizontal border spacing",min = 0, max = 0.1, step = 0.005,value = 0.005),
                                width = 3,
                                style = "overflow-y:scroll; max-height: 850px; position:relative;border-color:#000000"
                                
                   ),
                   mainPanel(width = 9,
                             "Main",
                             dataTableOutput("bar_table_out") %>% withSpinner(type = 1,color.background = "white"),
                             fluidRow(
                               #   box(
                               #   numericInput("bar_plotheight","Select height",value = 6),
                               #   numericInput("bar_plotwidth","Select width",value = 16),
                               # ),
                               box(
                                 sliderInput("taxa_plot_out_w","Plot width",min = 0, max = 2000,step = 100,value = 600),
                                 sliderInput("taxa_plot_out_h","Plot height",min = 0, max = 2000,step = 100,value = 400),
                                 downloadButton("bar_download","Save figure"),
                                 width = 3
                               )
                             ),
                             br(),
                             br(),
                             plotOutput("taxa_bar") %>% withSpinner(type = 1,color.background = "white",size = 3),
                             style = "overflow-y:scroll; max-height: 850px; position:relative;"
                             
                   )
                 )
        ),
        
        
        #### Bubble plot ####
        tabPanel("Bubble plot",
                 sidebarLayout(
                   sidebarPanel(h4("Select preferences"),
                                actionButton("bubble_start",label = "Start!"),
                                radioButtons("b1_new_old","Do you want the 'old' or 'new' bubbleplot?", c("New","Old"), selected = "New", inline = TRUE),
                                textInput("b1_ab_thresh","Relative abundace threshold (%)", value = 20),
                                radioButtons("b1_incl_percent","Do you want to include percenta abundance numbers?",c("Yes","No"), selected = "Yes", inline = TRUE),
                                sliderInput("b1_num_dec","Set the number of decimals (0 to 5)",min = 0, max = 5, value = 0, step = 1, ticks = FALSE),
                                selectInput("b1_sort_axis","How do you want to order your samples?",choices = "Updating"),
                                selectInput("b1_sort_param","How do you want to group your samples (facet)?",choices = "Updating"),
                                radioButtons("b1_second_facet","Do you want to group by a second category?",choices = c("Yes","No"), selected = "No", inline = TRUE),
                                selectInput("b1_second_facet_meta","Choose the second ordering",choices = "Updating"),
                                selectInput("b1_color_param","How do you want to colour your bubbles?", choices = "Updating"),
                                radioButtons("b1_confirm_sort","Do you want to sort the y-axis by taxonomic classification?",c("Yes","No"), selected = "Yes", inline = TRUE),
                                selectInput("b1_tax_sort","By what taxonomic level will the y-axis be sorted?",c("Phylum","Class","Order","Family","Genus","Species")),
                                textInput("b1_tax_keyword","Filter for specific taxa (e.g., 'staph')"),
                                #selectInput("b1_meta_group","Select a metadata category for data filtering",choices = "Updating"),
                                #textInput("b1_meta_keyword","Provide a specific keyword for the metadata filtering"),
                                br(),
                                h4("Choose facetting options"),
                                radioButtons("b1_panel_border","Do you want panel borders?",choices = c("Yes","No"),selected = "No", inline = TRUE),
                                radioButtons("b1_facet_side_x","Where do you want the sample bar?",c("Top","Bottom"),selected = "Bottom",inline = TRUE),
                                radioButtons("b1_facet_side_y","Where do you want the taxon bar?",c("Left","Right"),selected = "Left",inline = TRUE),
                                sliderInput("b1_panel_spacing","Modify the facet spacing",min = 0, max = 20, value = 0),  
                                width = 3,
                                style = "overflow-y:scroll; max-height: 850px; position:relative;border-color:#000000"
                   ),
                   mainPanel(width = 9,
                             br(),
                             dataTableOutput("b1_table_out"),
                             fluidRow(box(
                               # (textInput("b1_bubble_width","Plot width",value = 10),
                               #   textInput("b1_bubble_height", "Plot height",value = 8),
                               #   width = 3,
                               # ),
                               # box(
                               downloadButton("b1_data_table","Save table"),
                               sliderInput("b1_plot_out_w","Plot width",min = 0, max = 2000,step = 100,value = 600),
                               sliderInput("b1_plot_out_h","Plot height",min = 0, max = 2000,step = 100,value = 600),
                               downloadButton("b1_bubble_download","Save figure"),
                               width = 3
                             )
                             ),
                             br(),
                             br(),
                             plotOutput("bubble_out") %>% withSpinner(type = 1,color.background = "white"),
                             style = "overflow-y:scroll; max-height: 850px; position:relative;"
                             
                             
                   )
                 )
        ),
        
        
        #### Bray curtis PCOA ####
        tabPanel("Bray-Curtis PCoA",
                 sidebarLayout(
                   sidebarPanel(h4("Select preferences"),
                                actionButton("pcoa_start",label = "Start!"),
                                numericInput("pcoa_srs_depth","Select your SRS depth",min = 0, value = 1000),
                                numericInput("pcoa_env_thresh", "Select your p-value threshold (0 - 1)", min = 0, max = 1, value = 0.5),
                                numericInput("pcoa_taxa_thresh", "Select your taxon abundance (0 - 100)", min = 0, max = 100, value = 5),
                                selectInput("pcoa_fill_col","Select your fill colour",choices = "Updating"),
                                selectInput("pcoa_shape","Select your shape",choices = "Updating"),
                                #selectInput("pcoa_elips_col","Select your elipsis color",choices = "Updating"),
                                radioButtons("pcoa_elips","Do you want statistically generated elipses?", choices = c("Yes", "No"),inline = TRUE),
                                
                                width = 3,
                                style = "overflow-y:scroll; max-height: 850px; position:relative;border-color:#000000"
                   ),
                   mainPanel(width = 9,
                             "PCoA",
                             dataTableOutput("pcoa_table_out"),
                             fluidRow(
                               #   box(
                               #   textInput("pcoa_plot_width","Plot width",value = 12),
                               #   textInput("pcoa_plot_height", "Plot height", value = 8),
                               #   width = 3
                               # ),
                               box(
                                 
                                 sliderInput("pcoa_plot_outw","Plot width",min = 0, max = 2000,step = 100,value = 1000),
                                 sliderInput("pcoa_plot_outh","Plot height",min = 0, max = 2000,step = 100,value = 600),
                                 downloadButton("pcoa_download","Save figure"),
                                 width = 3
                               )
                             ),
                             br(),
                             br(),
                             plotOutput("pcoa_plot_out") %>% withSpinner(type = 1,color.background = "white"),
                             style = "overflow-y:scroll; max-height: 850px; position:relative;"
                             
                   )
                 )
        ),
        
        
        
        #### Ranked abundance plot ####
        tabPanel("Ranked abundance plot",
                 sidebarLayout(
                   sidebarPanel(h4("Select preferences"),
                                actionButton("ranked_start",label = "Start!"),
                                # radioButtons("ranked_new_old","Do you want the 'old' or 'new' bubbleplot?", c("New","Old"), selected = "New", inline = TRUE),
                                numericInput("ranked_threshold","Relative abundace threshold (%)", min = 0, max = 1, value = 0.9),
                                radioButtons("ranked_incl_percent","Do you want to include percenta abundance numbers?",c("Yes","No"), selected = "Yes", inline = TRUE),
                                sliderInput("ranked_num_dec","Set the number of decimals (0 to 5)",min = 0, max = 5, value = 0, step = 1, ticks = FALSE),
                                selectInput("ranked_sort_axis","How do you want to order your samples?",choices = "Updating"),
                                selectInput("ranked_sort_param","How do you want to group your samples (facet)?",choices = "Updating"),
                                radioButtons("ranked_second_facet","Do you want to group by a second category?",choices = c("Yes","No"), selected = "No", inline = TRUE),
                                selectInput("ranked_second_facet_meta","Choose the second ordering",choices = "Updating"),
                                selectInput("ranked_color_param","How do you want to colour your bubbles?", choices = "Updating"),
                                radioButtons("ranked_confirm_sort","Do you want to sort the y-axis by taxonomic classification?",c("Yes","No"), selected = "Yes", inline = TRUE),
                                selectInput("ranked_tax_sort","By what taxonomic level will the y-axis be sorted?",c("Phylum","Class","Order","Family","Genus","Species")),
                                textInput("ranked_tax_keyword","Filter for specific taxa (e.g., 'staph')"),
                                #selectInput("b1_meta_group","Select a metadata category for data filtering",choices = "Updating"),
                                #textInput("b1_meta_keyword","Provide a specific keyword for the metadata filtering"),
                                br(),
                                h4("Choose facetting options"),
                                radioButtons("ranked_panel_border","Do you want panel borders?",choices = c("Yes","No"),selected = "No", inline = TRUE),
                                radioButtons("ranked_facet_side_x","Where do you want the sample bar?",c("Top","Bottom"),selected = "Bottom",inline = TRUE),
                                radioButtons("ranked_facet_side_y","Where do you want the taxon bar?",c("Left","Right"),selected = "Left",inline = TRUE),
                                sliderInput("ranked_panel_spacing","Modify the facet spacing",min = 0, max = 20, value = 0),  
                                width = 3,
                                style = "overflow-y:scroll; max-height: 850px; position:relative;border-color:#000000"
                   ),
                   mainPanel(width = 9,
                             br(),
                             dataTableOutput("ranked_table_out"),
                             fluidRow(box(
                               # (textInput("b1_bubble_width","Plot width",value = 10),
                               #   textInput("b1_bubble_height", "Plot height",value = 8),
                               #   width = 3,
                               # ),
                               # box(
                               downloadButton("ranked_data_table","Save table"),
                               sliderInput("ranked_plot_out_w","Plot width",min = 0, max = 2000,step = 100,value = 600),
                               sliderInput("ranked_plot_out_h","Plot height",min = 0, max = 2000,step = 100,value = 600),
                               downloadButton("ranked_bubble_download","Save figure"),
                               width = 3
                             )
                             ),
                             br(),
                             br(),
                             plotOutput("ranked_plot_out") %>% withSpinner(type = 1,color.background = "white"),
                             style = "overflow-y:scroll; max-height: 850px; position:relative;"
                             
                             
                   )
                 )
        ),
        
        
        #### Next Plot ####
        
        #### FAQ PAGE ####
        tabPanel(title = "FAQ",
                 sidebarLayout(
                   sidebarPanel(
                     "Oopsies",
                     style = "background-color:#FFFFFF; border-color:#000000; border-style: solid; border-width: 1.5px; margin-left:0px; margin-right:30px; padding: 10px",
                   ),
                   mainPanel(
                     fluidRow(
                       box("SO YOU GOTS SOME QUESTIONS?",
                           uiOutput("helppdf"),
                           width = 12,
                           height = 200,
                           style = "background-color:#FFFFFF; border-color:#000000; border-style: solid; border-width: 1.5px; margin-left:0px; margin-right:30px; padding: 10px; height: 850px",
                           
                           
                       )
                     )
                   )
                 )
        )
        
        
)

