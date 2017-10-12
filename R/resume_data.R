

resume_gene <- function(df) {
  
  df %>% 
    ungroup() %>%
    distinct(SYMBOL, CHR, POSITION, REF, ALT) %>%
    select(Chromosome = CHR, Position = POSITION, 
           `Reference allele` = REF,`Alterated allele` = ALT, 
           Gene = SYMBOL)

}


resume_run <- function(df, type = "df") {


  if (type == "df")
    ungroup(df) %>%
        distinct(RUN)
  else if (type == "nb")
    ungroup(df) %>%
    distinct(RUN) %>%
    nrow()
  
}