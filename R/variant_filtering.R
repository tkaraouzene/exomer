library(dplyr)

filter_genotype <- function(df, homo = TRUE, het = TRUE) {

  kept_genotypes <- vector('character')

  if (homo == TRUE) kept_genotypes <- c(kept_genotypes,"Homozygous")
  if (het == TRUE) kept_genotypes <- c(kept_genotypes,"Heterozygous")
  
  filter(df, CALL %in% kept_genotypes) 
  
}

filter_frequence <- function(df, cutoff) {
  
  filter(df,
    is.na(ExAC_AF) | ExAC_AF <= cutoff,
    is.na(ESP) | ESP <= cutoff,
    is.na(`1KG`) | `1KG` <= cutoff)

    
}
# 
# filter_impact <- function(df, 
#                           remove_csq = NULL, 
#                           remove_nc_transcript = TRUE, 
#                           remove_NMD = TRUE, 
#                           remove_impact = NULL,
#                           remove_missense = NULL) {
#   
#   filter(df, 
#          !(IMPACT %in% remove_impact),
#          
#          isTRUE(remove_NMD) & !grepl("NMD_transcript_variant",Consequence),
#          isTRUE(remove_nc_transcript) & !grepl("non_coding_transcript_variant",Consequence),
#          !is.null(remove_csq) & !(Consequence %in% remove_csq),
#          !is.null(remove_missense) & remove_missense == "both" & !(grepl("tolerated", SIFT) & grepl("benign", PolyPhen))
# 
#            # if(remove_missense == "one") !(grepl("tolerated", SIFT) | grepl("benign", PolyPhen))
#            # if(remove_missense == "sift") !grepl("tolerated", SIFT)
#            # if(remove_missense == "polyphen") !grepl("benign", PolyPhen)
#   )
#   
# }
# 
# filter_union <- function(df, union_group) {
#   
#   # union_group <- quote(union_group)
#   print(union_group)
#   df %>%
#     group_by(union_group) %>%
#     mutate(NB_GRP = n_distinct(RUN)) %>%
#     group_by(CHR, POSITION, REF, ALT, union_group, Feature) %>%
#     filter(n() == mean(NB_GRP))
#   
# }

