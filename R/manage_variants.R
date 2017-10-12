
library(dplyr)
library(data.table)

variant_join <- function(df1, df2, mode, same_genotype = TRUE, add_by = NULL, filter_specific = TRUE) {
  
  by <- join_by(df1, df2, add_by, same_genotype)
  
  df_tmp <- join_mode(df1, df2, by, mode)
  
}

join_by <- function(df1, df2, add_by, same_genotype = TRUE) {
  
  # Takes two dataframe as input
  # it determines by what these two df are going to be merged
  
  by <- c("CHR", "POSITION", "REF", "ALT")

  if (!is.null(add_by)) by <- c(by, add_by)
  
  cn1 <- colnames(df1)
  cn2 <- colnames(df2)
  
  if (same_genotype == TRUE &
      "GENOTYPE" %in% cn1 &
      "GENOTYPE" %in% cn2) by <- c(by, "GENOTYPE")
  
  if (("TYPE" %in% cn1) &
      ("TYPE" %in% cn2)) by <- c(by, "TYPE")
  
  by
  
}

join_mode <- function(df1, df2, by, mode) {
  
  if (mode == "inner") 
    inner_join(df1, df2, by = by)
  else if (mode == "anti")
    anti_join(df1, df2, by = by)
  else if (mode == "right")
    right_join(df1, df2, by = by)
  else if (mode == "left")
    left_join(df1, df2, by = by)
  else if (mode == "full")
    full_join(df1,df2,by = by)
}

# variant_union_list <- function(..., same_genotype = TRUE, mode = "inner") {
#   
# # same as variant_union
# # but can take more than 2 df as args
# # PB: very slow compare to traditional inner_join
#
#   l <- list(...)
#   
#   if (length(l) == 2) {
# 
#     uc <- join_by(..., same_genotype)
#     join_mode(df1, df2, by = uc, mode = mode)
#     
#   } else {
#     
#     l %>%
#       
#       Reduce(function(df1,df2) {
#         
#         uc <- join_by(df1, df2, same_genotype)
#         
#         join_mode(df1, df2, by = uc, mode = mode)
#         
#       },  .)
#   }
# }
