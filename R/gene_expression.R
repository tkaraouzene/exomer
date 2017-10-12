
retrieve_exp <- function(my_genes, tissues = NULL, path = "data/expression/Illumina_Body_Map") {
  
  file <- paste0(path, "/E-MTAB-513-query-results.tsv")
  
  df <- fread(file) %>% 
    rename(SYMBOL = `Gene Name`) %>% 
    mutate(SYMBOL2 = toupper(SYMBOL)) %>%
    filter(SYMBOL2 %in% toupper(my_genes)) %>%
    select(-SYMBOL2)%>%
    gather(Tissues,Expression, `adipose tissue`:`thyroid gland`) %>%
    group_by(SYMBOL) %>%
    mutate(P_Expression = Expression / sum(Expression), 
           P_Expression = if_else(is.nan(P_Expression), 0, P_Expression))
  
  if(!is.null(tissues)) df <-filter(df, Tissues %in% tissues)
    
  df

}
  
retrieve_gene_case <- function(gene, tissues = NULL, path = "data/expression/Illumina_Body_Map") {
  
  file <- paste0(path, "/E-MTAB-513-query-results.tsv")
  
  fread(file) %>% 
    rename(SYMBOL = `Gene Name`) %>% 
    mutate(SYMBOL2 = toupper(SYMBOL)) %>%
    filter(SYMBOL2 %in% toupper(gene)) %>%
    distinct(SYMBOL) %>%
    collect() %>%
    .[["SYMBOL"]]

}




  
add_exp <- function(df, path = "data/expression/Illumina_Body_Map")  {
    
file <- paste0(path, "/E-MTAB-513-query-results.tsv")

exp <- fread(file) %>% 
  select(- `Gene ID`) %>% 
  rename(SYMBOL = `Gene Name`) 


  left_join(df,exp, by = "SYMBOL")
    

}


plot_expression <- function(genes, mode = "value", flip = FALSE, tissues = NULL, path = "data/expression/Illumina_Body_Map") {
  
  if (!is.null(tissues) &
      length(tissues) == 1) x_string <- "SYMBOL"
  else x_string <- "Tissues"
  
  if (mode == "value") {
    
    y_string <- "Expression"
    y_lab <- ("RPKM")
    y_scale <- NULL
    leg_name <- "Expression"
    
  } else if (mode == "percent") {
    
    y_string <- "P_Expression"
    y_lab <- ("% of RPKM")
    y_scale <- scale_y_continuous(labels = percent) 
    leg_name <- "%Expression"
    
  } else {
    
    print("unexpected mode, must be one of: \"value\" or \"percent\"")
    
  }
  
  plot_fill <- NULL
  x_lab <- NULL
  
  if (length(genes) == 1) { 
    
    my_gene <- retrieve_gene_case(genes[1], path = path)
    
    leg_name <- paste0(leg_name," :\n ", my_gene)
    plot_fill <- y_string
    
    if (mode == "percent") scale_fill <- viridis::scale_fill_viridis(name = leg_name, labels = percent)
    else scale_fill <- viridis::scale_fill_viridis(name = leg_name)
    
  } else if(length(tissues) == 1) { 
  
    x_string <- paste0("reorder(", x_string,", ", y_string, ")")
    plot_fill <- y_string
    x_lab <- ""
    
    if (mode == "percent") scale_fill <- viridis::scale_fill_viridis(name = leg_name, labels = percent)
    else scale_fill <- viridis::scale_fill_viridis(name = leg_name)
    
  } else {
    
    plot_fill <- "SYMBOL"
    scale_fill <- scale_fill_brewer(name = "Genes", palette = "Set1")
    
  }
  
  df_exp <- retrieve_exp(genes, tissues, path = path)
  
  p <- ggplot(df_exp, aes_string(x_string, y_string)) +
    geom_col(aes_string(fill = plot_fill), position = position_dodge()) + 
    ylab(y_lab) + 
    scale_fill
  
  if (flip == TRUE) p <- p + coord_flip()
  else p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  if (!is.null(y_scale)) p <- p + y_scale
  if (!is.null(x_lab)) p <- p + xlab(x_lab)
  p
  
}


plot_expression_bk <- function(genes, mode = "value", flip = FALSE, tissues = NULL) {
  
  
  if (mode == "value") {
    
    y_string <- "Expression"
    y_lab <- ("RPKM")
    y_scale <- NULL
    leg_name <- "Expression"
    
  } else if (mode == "percent") {
    
    y_string <- "P_Expression"
    y_lab <- ("% of RPKM")
    y_scale <- scale_y_continuous(labels = percent) 
    leg_name <- "%Expression"
    
  } else {
    
    print("unexpected mode, must be one of: \"value\" or \"percent\"")
    
  }
  
  plot_fill <- NULL
  
  if (length(genes) == 1) { 
    
    leg_name <- paste0(leg_name," : ",genes[1])
    plot_fill <- y_string
    
    if (mode == "percent") scale_fill <- viridis::scale_fill_viridis(name = leg_name, labels = percent)
    else scale_fill <- viridis::scale_fill_viridis(name = leg_name)
    
  } else { 
    
    plot_fill <- "SYMBOL"
    scale_fill <- scale_fill_brewer(name = "Genes", palette = "Set1")
    
  }
  
  df_exp <- retrieve_exp(genes, tissues)
  
  p <- ggplot(df_exp, aes_string("Tissues", y_string)) +
    geom_col(aes_string(fill = plot_fill), position = position_dodge()) + 
    ylab(y_lab) + 
    scale_fill
  
  if (flip == TRUE) p <- p + coord_flip()
  else p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  if (!is.null(y_scale)) p <- p + y_scale
  p
  
}



