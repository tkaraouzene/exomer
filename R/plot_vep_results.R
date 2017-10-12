



plot_nmd <- function(df, scale = "count", display_value = FALSE) {
  
  df <- ungroup(df) %>%
    select(Consequence, RUN) %>%
    mutate(Is_NMD = grepl("NMD_transcript_variant", Consequence)) %>% 
    group_by(RUN) %>% mutate(N = n()) %>% 
    group_by(Is_NMD, RUN) %>%
    summarise(Nb = n(),
              N = mean(N))

  p_nmd <- ggplot(df, aes(Is_NMD, fill = Is_NMD)) +
    scale_fill_discrete(guide = "none") +
    scale_x_discrete(labels = c("Not NMD transcript", "NMD transcript")) +
    xlab("")
  
  p_count <- p_nmd +
    geom_bar(stat = "identity", aes(y = Nb)) +
    ylab("Count") +
    facet_wrap(~RUN)
  
  p_percent <- p_nmd +
    geom_bar(stat = "identity", aes(y = (Nb / N))) +
    scale_y_continuous(labels = percent) +
    ylab("Percentage") +
    facet_wrap(~RUN)
  

  if (display_value == TRUE) {
    
    p_count <- p_count + geom_text(aes(label = Nb, y = Nb), position = position_dodge(width = .9), vjust = 1)
    p_percent <- p_percent + geom_text(aes(label = percent(Nb/N), y = Nb/N), position = position_dodge(width = .9), vjust = 1)
    
  }

  if (scale == "count") p_count
  else if (scale == "percent") p_percent
  else if (scale == "both") list(p_count,p_percent)
  
  
  
}