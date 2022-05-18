
#Create a matrix of experimental contrasts

#' A function to create a pairwise comparison matrix
#'
#' @details A table to show all the pairwise comparisons for the contrast of interest (tally of lake-years) in the database
#' @title contrast_mat
#' @name contrast_mat
#' @param minN the minimum sample size of fish captured in a cohort to be included in the summary.
#' The tally in the matrix in by lake-year, but the sample size filter is at the age-class level. So to be counted in the matrix at least one age class is above the minN in that lake-year.   
#' @keywords contrast matrix
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @examples

#' contrast_mat()

contrast_mat <- function(minN = 10) {
  #Summarize the number of fish observed for each level of the contrast
  df = gdf%>%dplyr::group_by(Lk_yr, Lk_yr_age, .data[[Contrast]])%>%
    dplyr::summarize(N = sum(N))
  
  #Now spread out the levels of the contrast to set up for the matrix, and filter to cohorts where minN or greater fish observed
  df = df%>%tidyr::pivot_wider(names_from = Contrast, values_from = N)%>%
    dplyr::mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE), na_cnt = sum(is.na(across(where(is.numeric)))))%>%
    dplyr::filter(total >= minN, na_cnt<2)%>%
    dplyr::ungroup()
  
 #Now just group by lake year for the tally 
 df = df%>%
   dplyr::group_by(Lk_yr)%>%summarize_if(is.numeric,sum)%>%
   dplyr::ungroup()%>%
   dplyr::select(-c(Lk_yr, total, na_cnt))
  
  x = apply(df,2,function(x){
    apply(df,2,function(y){
      sum(!is.na(x) & !is.na(y))
    })
  })
  
  x
}