library(shiny)
library(semantic.dashboard)
#library(shinydashboard)
library(data.table)
library(tidyverse)
library(ggridges)
library(cowplot)
library(plotly)
library(ggbeeswarm)
library(RColorBrewer)
library(ggsci)

## Load data based on different modes, for now defined here

## Mode 1: Subfolder mode where each subfolder is 1 sample
## Mode 2: TMA mode, where multiple quantification files are in the quantification folder (one for each core)
mode_selected <- "mode_2"

if(mode_selected == "mode_1"){
base_dir <- "/Users/florian_wuennemann/Heidelberg/Projects/Collabs/Christoph_Kuppe/mcmicro_test/analysis"

all_quants <-  list() # Stores all quantification tables
metadata <- data.frame() # Stores info about samples and slides
tec_all <- data.frame()

for(stain in list.files(base_dir)){
    samples <- list.files(paste(base_dir,stain,sep="/"))
    for(sample in samples){
        ## Read in quantification results
        this_quant <- paste(base_dir,"/",stain,"/",sample,"/","quantification/mesmer-",sample,"_cell.csv",sep="")
        quant_res <- fread(this_quant)
        
        ## Calculate some descriptive statistics about the sample
        n_cells <- nrow(quant_res)
        markers <- setdiff(colnames(quant_res),c("CellID","X_centroid","Y_centroid","Area","MajorAxisLength","MinorAxisLength","Eccentricity","Solidity","Extent","Orientation"))
        markers_string <- paste(markers,collapse="_")
        n_markers <- length(markers)
        quant_res$sample <- sample
        quant_res$staining <- stain
        
        ## save all technical measures in one big data frame
        quant_res_tec <- quant_res %>%
            select(-markers)
        
        tec_all <- rbind(tec_all,quant_res_tec)
        
        quant_res <- quant_res %>%
            select(CellID,markers,sample,staining)
        
        all_quants[[sample]] <- quant_res
        sample_meta <- data.frame("sample_id" = sample,
                                  "staining" = stain,
                                  "n_cells" = n_cells,
                                  "n_markers" = n_markers,
                                  "markers" = markers_string)
        metadata <- rbind(metadata,sample_meta)
    }
}
}else if(mode_selected == "mode_2"){
    base_dir <- "/Users/florian_wuennemann/Heidelberg/Projects/Margot_TMAs/CCL_Core1/quantification"
    all_quants <-  list() # Stores all quantification tables
    metadata <- data.frame() # Stores info about samples and slides
    tec_all <- data.frame()
    for(quant_file in list.files(base_dir)){
        this_quant <- paste(base_dir,quant_file,sep="/")
        quant_res <- fread(this_quant)
        
        ## Calculate some descriptive statistics about the sample
        n_cells <- nrow(quant_res)
        markers <- setdiff(colnames(quant_res),c("CellID","X_centroid","Y_centroid","Area","MajorAxisLength","MinorAxisLength","Eccentricity","Solidity","Extent","Orientation"))
        markers_string <- paste(markers,collapse="_")
        n_markers <- length(markers)
        quant_res$quant_file <- quant_file
        
        ## save all technical measures in one big data frame
        quant_res_tec <- quant_res %>%
            select(-all_of(markers))
        
        tec_all <- rbind(tec_all,quant_res_tec)
        
        all_quants[[quant_file]] <- quant_res
        sample_meta <- data.frame("sample_id" = quant_file,
                                  "n_cells" = n_cells,
                                  "n_markers" = n_markers)
        sample_meta$group <- "TMA"
        metadata <- rbind(metadata,sample_meta)
    }
}

## Summarize technical parameters per core or sample for quicker plotting
tec_summary <- tec_all %>%
    group_by(quant_file) %>%
    summarize("mean_Area" = mean(Area),
              "mean_Eccentricity" = mean(Eccentricity),
              "mean_Solidity" = mean(Solidity)
              ) %>%
    mutate("group" = "TMA")

# "sd_Area" = sd(Area),
# "sd_Eccentricity" = sd(Eccentricity),
# "sd_Solidity" = sd(Solidity)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ## Overview elements
    if(mode_selected == "mode_1"){
        sample_text <- "N Samples"
    }else if(mode_selected == "mode_2"){
        sample_text <- "N Cores"
    }
    n_samples <- nrow(metadata)

    output$n_samples_box <- renderInfoBox({
        infoBox(
            sample_text, paste(n_samples), icon = icon("list"), color = "green"
            )})
    
    ## Plot number of cells segmented per slide
    output$n_cells_plot <- renderPlotly({
        ggplot(metadata,aes(group,n_cells, text = sample_id)) +
            geom_boxplot(aes(fill = group)) +
            geom_beeswarm(size = 4) +
            theme_cowplot() +
            coord_flip() +
            theme(legend.position = "none") +
            scale_fill_brewer(palette= "Dark2") +
            labs(title = "Number of cells per sample")
    })
    
    ## Plot technical parameter distribution across samples
    output$tec_plot <- renderPlotly({
        req(input$tec_param)
        ggplot(tec_summary,aes(group,get(input$tec_param))) +
            geom_boxplot() +
            theme_cowplot() +
            coord_flip() +
            theme(legend.position = "none") +
            labs(title = "Distribution of measurements across samples") + 
            scale_fill_brewer(palette = "Dark2") +
            labs(y = input$tec_param)
    })
    
    ## Plot 
    output$corrplot <- renderPlotly({
        req(input$tec_param)
        req(input$tec_corr)
        ggplot(tec_summary,aes(get(input$tec_param),get(input$tec_corr))) +
            geom_point() +
            theme_cowplot() +
            labs(title = "Correlation of cell measurements across samples") + 
            scale_color_brewer(palette = "Dark2") +
            labs(x = input$tec_param,
                 y = input$tec_corr)
    })
    
    
    
    ## Sample selection and sample view controls
    output$sample_selection = renderUI({
        selectizeInput('sample_sel', 'Select your sample:', choices = metadata$sample_id, 
                       multiple = FALSE, selected = metadata$sample_id[0])
    })

    observeEvent(eventExpr = input$sample_sel,{
        
        ## Get data for selected sample
        selected_data <- all_quants[[input$sample_sel]]

        ## Overview elements
        if(mode_selected == "mode_1"){
            markers <- setdiff(colnames(selected_data),c("CellID","X_centroid","Y_centroid","Area","MajorAxisLength","MinorAxisLength","Eccentricity","Solidity","Extent","Orientation","sample"))
        }else if(mode_selected == "mode_2"){
            markers <- setdiff(colnames(selected_data),c("CellID","X_centroid","Y_centroid","Area","MajorAxisLength","MinorAxisLength","Eccentricity","Solidity","Extent","Orientation","quant_file"))
        }
       
        ## Plot data

        ## reformat wide marker data into long to be able to use ggplot
        marker_distribution <- selected_data %>%
            select(all_of(markers)) %>%
            pivot_longer(cols = markers,
                         names_to = "markers",
                         values_to = "intensity")

        output$marker_intensity <- renderPlot(height = 600,{

            ggplot(marker_distribution,aes(log10(intensity),markers, fill = markers)) +
                geom_density_ridges() +
                theme_cowplot()
        })

    })

})