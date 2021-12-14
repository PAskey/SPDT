#' A plot function for SPDT data when a contrast has been specified.
#' Function only usable by FFSBC staff who have a direct or vpn connection to SLD.
#'
#'
#'                                         
#'
#'
#' @title SPDTplot
#' @name SPDTplot
#' @keywords SPDT, SPDTplot, plot
#' @export
#' @param Metric a character string that describes the performance metric (dependent variable) to be plotted.This must be entered for function to work.
#' The specific options that can be stated are: 
#' "survival", 
#' "mu_growth_FL" (average length per age), 
#' "growth_FL" (individual lengths per age), 
#' "length_freq", 
#' "age_freq" (length and age histograms respectively), 
#' "maturation" (the proportion mature per age)
#' In all cases the data will be grouped by the "Contrast" stated during the SPDT data call.
#' @param Method a character string describing the capture method. Defaults to "GN" (gillnet), but any other capture method code fo rmthe database is acceptable.
#' @param min_N an integer to set a minimum sample size to include in plots. This sample size applies to the overall sample across contrast groups.
#' In other words a minimum sample size of 5 would still include a stocked cohort where one group had 5 fish and the other contrast group had 0.
#' @param save_png a logical TRUE/FALSE indicating whether a copy of the plot should be saved with the filename Metric.png
#' @examples
#' #Must be connected to VPN if working remotely
#' 
#' #Download all data from lake years that had a genotype comparison for KO
#' SPDTdata(Spp = "KO", Contrast = "Genotype")
#' 
#' Plot the relative survival of the contrast groups
#' SPDTplot(Metric = "survival")
#' 
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


SPDTplot <- function(Metric = NULL, Method = "GN", min_N = 0, save_png = FALSE){
  #Allowable metrics are: maturation, growth, survival
  #c("survival", "growth_FL", "mu_growth_FL", "length_freq", "age_freq", "maturation")

  #For colouring plots by brood year (with fewer unique levels), set up grouping column col_group
  #TD add filter to remove age classes that don't have releases for multiple levels of Contrast
  plot_idf <- idf%>%dplyr::filter(Capture_Method == Method, !is.na(N_rel))%>%
    dplyr::group_by(WBID, Int.Age)%>%
    dplyr::mutate(col_group = as.integer(factor(sby_code, levels = unique(sby_code))))%>%
    dplyr::group_by(Lk_yr_age)%>%
    dplyr::mutate(N_a = dplyr::n(), Dec.Age = Int.Age+(lubridate::month(Date)-1)/12)%>%
    dplyr::ungroup()%>%
    dplyr::filter( N_a >= min_N)
  
  plot_gdf <- gdf%>%dplyr::filter(Capture_Method == Method, !is.na(N_rel))%>%
    dplyr::group_by(WBID, Int.Age)%>%
    dplyr::mutate(col_group = as.integer(factor(sby_code, levels = unique(sby_code))))%>%
    dplyr::group_by(Lk_yr_age)%>%
    dplyr::mutate(N_a = sum(N))%>%
    dplyr::ungroup()%>%
    dplyr::filter(N_a >= min_N)

  #Could add an additional filter [!is.na(plot_idf$Length_mm),]
  #But it is better to get the warning I think. It still plots
  
if (Metric == "YOY"){
  p = ggplot2::ggplot(data = plot_idf%>%dplyr::filter(Year-sby_code == 1), ggplot2::aes(x = .data$Length_mm, fill = .data$Genotype))+
    ggplot2::geom_histogram(alpha = 0.5, colour = "black", position = "identity", binwidth = 10)+
    viridis::scale_fill_viridis(discrete = TRUE)+
    ggplot2::facet_wrap(~.data$Waterbody_Name+sby_code+Year, scales = "free_y")+
    ggplot2::theme_bw() 
  
}  
  
  

  
if (Metric == "survival"){
  p = ggplot2::ggplot(data = plot_gdf, ggplot2::aes(x = .data$Int.Age, y = .data$NetXN, fill = get(Contrast), shape = as.factor(.data$col_group), group = .data$sby_code))+
    ggplot2::geom_point(size = 4, alpha = 0.7, position = ggplot2::position_dodge(width = 0.2))+
    ggplot2::scale_shape_manual(values = rep(21:25, 5))+
    viridis::scale_fill_viridis(discrete = TRUE)+
    #ggplot2::scale_y_continuous(trans='log10')+
    ggplot2::facet_wrap(~.data$Waterbody_Name, scales = "free_y")+
    ggplot2::labs(y = "Catch (selectivity adjusted)", x = "Age", shape = Contrast)+
    ggplot2::theme_bw()+
    ggplot2::guides(shape = "none", fill = ggplot2::guide_legend(override.aes = list(shape = 21)))
}  
  
  
# if (Metric == "mu_growth_FL"){
#  p = ggplot2::ggplot(data = plot_gdf, ggplot2::aes(x = .data$Int.Age, y = .data$NetX_FL, shape = get(Contrast), fill = as.factor(.data$col_group), group = .data$sby_code))+
#    ggplot2::geom_point(size = 4, alpha = 0.7, position = ggplot2::position_dodge(width = 0.2))+
#    ggplot2::scale_shape_manual(values = rep(21:25, 5))+
#    viridis::scale_fill_viridis(discrete = TRUE)+
#    ggplot2::facet_wrap(~.data$Waterbody_Name)+
#    ggplot2::labs(x = "Age", y = "Mean Fork Length (mm)", shape = Contrast)+
#    ggplot2::theme_bw()+
#    ggplot2::guides(fill="none")
# }
 
  if (Metric == "mu_growth_FL"){
    p = ggplot2::ggplot(data = plot_gdf, ggplot2::aes(x = .data$Waterbody_Name, y = .data$NetX_FL, shape = as.factor(.data$sby_code), fill = get(Contrast), group = .data$sby_code))+
      ggplot2::geom_point(size = 5, alpha = 0.7, position = ggplot2::position_dodge(width = 0.5))+
      ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$NetX_FL-.data$sd_FL, ymax=.data$NetX_FL+.data$sd_FL), width=.2, position=ggplot2::position_dodge(.5))+
      ggplot2::scale_shape_manual(values = rep(21:25, 5))+
      viridis::scale_fill_viridis(discrete = TRUE)+
      ggplot2::facet_wrap(~.data$Int.Age, scales = "free", nrow = 3)+
      ggplot2::labs(x = "Age", y = "Mean Fork Length (mm)", fill = Contrast)+
      ggplot2::theme_bw()+
      ggplot2::guides(shape = "none", fill = ggplot2::guide_legend(override.aes = list(shape = 21)))+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, vjust = 1, hjust=1))
  }
  
 ##Here is a code blcok to break u, when there are may lake-years of data 
  #WBIDs = unique(plot_idf$WBID)
  #NL = length(unique(plot_idf$WBID))
  #half = as.integer(0.5*NL)
  
  #WBIDs1 = WBIDs[1:half]
  #WBIDs2 = WBIDs[half:NL]
  
  #Then add this filter to the data
  #plot_idf[plot_idf$WBID%in%WBIDs1,]
  
  
#when plotting raw data need more facets 
 if (Metric == "growth_FL"){
   
   p = ggplot2::ggplot(data = plot_idf, ggplot2::aes(x = .data$Dec.Age, y = .data$Length_mm, colour = get(Contrast), fill = get(Contrast), group = Dec.Age))+
     ggplot2::geom_point(size = 4, alpha = 0.4, shape = 21, position = ggplot2::position_jitter(width = 0.1))+
     #ggplot2::scale_shape_manual(values = rep(21:25, 5))+
     #ggplot2::geom_boxplot(ggplot2::aes(group = Genotype))+
     viridis::scale_fill_viridis(discrete = TRUE)+
     viridis::scale_colour_viridis(discrete = TRUE)+
     ggplot2::facet_wrap(.data$Waterbody_Name~.data$Year, scales = "free_y")+
     ggplot2::labs(x = "Age", y = "Fork Length (mm)", fill = Contrast, colour = Contrast)+
     ggplot2::theme_bw()+
     #ggplot2::guides(colour=ggplot2::guide_legend(override.aes = list(shape = 21)),fill=ggplot2::guide_legend(override.aes = list(shape = 21)))+
     ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "black"))
 }
  
  if (Metric == "growth_FL"){
    p = ggplot2::ggplot(data = plot_idf, ggplot2::aes(x = .data$Waterbody_Name, y = .data$Length_mm, shape = as.factor(sby_code), fill = get(Contrast), group = get(Contrast)))+
      ggplot2::geom_point(size = 4, alpha = 0.5, position = ggplot2::position_dodge(width = 0.75))+
      ggplot2::scale_shape_manual(values = rep(21:25, 5))+
      viridis::scale_fill_viridis(discrete = TRUE)+
      ggplot2::facet_wrap(~.data$Int.Age)+
      ggplot2::labs(x = "", y = "Fork Length (mm)", fill = Contrast)+
      ggplot2::theme_bw()+
      ggplot2::guides(shape="none", fill=ggplot2::guide_legend(override.aes = list(shape = 21)))+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, vjust = 1, hjust=1))
  }
  
  
  
  
 
  if (Metric == "length_freq"){
    plotdf = idf%>%dplyr::filter(Outlier != 1)%>%
      dplyr::group_by(Waterbody_Name, Year)%>%
      dplyr::mutate(N_lake = dplyr::n())%>%
      dplyr::filter(N_lake >= min_N)
    
    
   p = ggplot2::ggplot(data = plotdf, ggplot2::aes(x = .data$Length_mm, fill = .data$Genotype))+
    ggplot2::geom_histogram(alpha = 0.4, colour = "black", position = "identity", binwidth = 10)+
    viridis::scale_fill_viridis(discrete = TRUE)+
    ggplot2::xlim(c(100,450))+
    ggplot2::facet_wrap(~.data$Waterbody_Name, scales = "free_y")+
    ggplot2::theme_bw() 
  }
  
  if (Metric == "fishery_length_freq"){
    plotdf = idf%>%dplyr::filter(Length_mm>200)%>%
                dplyr::group_by(Waterbody_Name, Year)%>%
                dplyr::mutate(N_lake = dplyr::n())%>%
                dplyr::filter(N_lake >= min_N)
    p = ggplot2::ggplot(data = plotdf, ggplot2::aes(x = .data$Length_mm, fill = .data$Genotype))+
      ggplot2::geom_histogram(alpha = 0.4, colour = "black", position = "identity", binwidth = 10)+
      viridis::scale_fill_viridis(discrete = TRUE)+
      ggplot2::xlim(c(200,NA))+
      ggplot2::facet_wrap(~.data$Waterbody_Name, scales = "free_y")+
      ggplot2::theme_bw() 
  } 
  
  
 
 if (Metric == "age_freq"){
   p = ggplot2::ggplot(data = plot_gdf, ggplot2::aes(x = as.factor(.data$Int.Age), y = .data$NetXN, fill = get(Contrast), colour = get(Contrast)))+
     #ggplot2::geom_histogram(alpha = 0.5, colour = "black", stat= "count")+
     #ggplot2::geom_bar(stat = "count", position = ggplot2::position_dodge2(preserve = "single", width = 0.8))+
     ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single"), ggplot2::aes(alpha = .data$col_group))+
     viridis::scale_fill_viridis(discrete = TRUE)+
     viridis::scale_colour_viridis(discrete = TRUE)+
     ggplot2::facet_wrap(~.data$Waterbody_Name, scales = "free_y")+
     ggplot2::labs(x = "Age", y = "Catch (selectivity adjusted)", fill = Contrast, colour = Contrast)+
     ggplot2::theme_bw()+
     ggplot2::guides(alpha = "none")+
     ggplot2::scale_alpha(range = c(0.4, 0.8))
 }
 
 


 if (Metric == "maturation"){
   p = ggplot2::ggplot(data = plot_gdf, 
      ggplot2::aes(x = .data$Int.Age, y = .data$p_mat, 
                   shape = get(Contrast), fill = get(Contrast), group = get(Contrast)))+
     ggplot2::geom_point(size = 4, alpha = 0.7, position = ggplot2::position_dodge(width = 0.2))+
     ggplot2::scale_shape_manual(values = c(21:24))+
     viridis::scale_fill_viridis(discrete = TRUE)+
     viridis::scale_colour_viridis(discrete = TRUE)+
     #ggplot2::facet_wrap(~.data$Waterbody_Name)+
     ggplot2::ylim(0,1)+
     ggplot2::geom_smooth(se = FALSE, ggplot2::aes(colour = get(Contrast)))+
     ggplot2::theme_bw()+
     ggplot2::guides(fill=ggplot2::guide_legend(override.aes=list(shape=21)))+
     ggplot2::labs(x = "Age", y = "Proportion mature or maturing", 
                   shape = Contrast, fill = Contrast, colour = Contrast)  
 } 
  
  if (Metric == "maturation_by_sex"){
    
    mat_df = plot_idf%>%
              dplyr::filter(Maturity != 'UNK', !is.na(Sex))%>%
              dplyr::group_by(Waterbody_Name, WBID, Year, Strain, Genotype, Int.Age, Sex)%>%
              dplyr::summarize(N = dplyr::n(),
                               Nkn = sum(.data$Maturity != 'UNK', na.rm = TRUE),
                               Nm = sum(.data$Maturity != 'IM'& .data$Maturity != 'UNK', na.rm = TRUE),
                               p_mat = sum(.data$Maturity != 'IM'& .data$Maturity != 'UNK', na.rm = TRUE)/sum(.data$Maturity != 'UNK', na.rm = TRUE))%>%
              dplyr::ungroup()
      
    
    p = ggplot2::ggplot(data = mat_df, 
                        ggplot2::aes(x = .data$Int.Age, y = .data$p_mat, 
                                     shape = .data$Sex, fill = get(Contrast), group = get(Contrast)))+
      ggplot2::geom_point(size = 4, alpha = 0.7, position = ggplot2::position_jitterdodge(  jitter.width = .1,
                                                                                            jitter.height = 0.02,
                                                                                            dodge.width = 0.3, seed = 1))+
      ggplot2::scale_shape_manual(values = c(21:24))+
      viridis::scale_fill_viridis(discrete = TRUE)+
      viridis::scale_colour_viridis(discrete = TRUE)+
      #ggplot2::facet_wrap(~.data$Waterbody_Name)+
      ggplot2::ylim(0,1)+
      ggplot2::scale_x_continuous(breaks = seq(min(mat_df$Int.Age), max(mat_df$Int.Age), by = 1))+
      ggplot2::geom_smooth(span = 1, se = FALSE, ggplot2::aes(colour = get(Contrast)))+
      ggplot2::theme_bw()+
      ggplot2::guides(fill=ggplot2::guide_legend(override.aes=list(shape=21)))+
      ggplot2::labs(x = "Age", y = "Proportion mature or maturing", 
                    shape = "Sex", fill = Contrast, colour = Contrast)  
  }   
  
  
  
 

  #print(p)   
  return(p)

 
#Save a .png of plot  
  
if(save_png == TRUE){
  filename = paste0("plot_",Metric,".png")
   
#In all cases above the Waterbody is used to facet, so the plot facets are arranged as:   
  facets = 1
  if(class(p$facet)[1] != "FacetNull"){
  facets = length(unique(p$data$Waterbody_Name)) 
  }
  ncol <- ceiling(sqrt(facets))
  nrow <- ceiling(facets/ncol)   
 

 ggplot2::ggsave(filename, p, dpi = "print", width = 7, height = 7*(nrow/ncol)*.95, units = "in")
}
 
}
  
##FUTURE CONSIDERATIONS

#FOr model plotting functions to work, need to convert factors outside of plotting formula
#idf$Int.Age = as.factor(idf$Int.Age)

#model = glm(data = idf, Length_mm~Lk_yr+(Int.Age)*Genotype)
#summary(model) 
#sjPlot::plot_model(model, type = "eff", terms = c("Int.Age", "Genotype"))

#jtools::effect_plot(model, pred = c(Genotype,Int.Age), plot.points = TRUE)+
#  ggplot2::facet_wrap(~Int.Age)

#model = glm(data = idf, Weight_g~Lk_yr+as.factor(Int.Age)*Genotype)
#summary(model) 
#sjPlot::plot_model(model, type = "pred", terms = c("Int.Age", "Genotype"))



# if (!is.null(Method)) {
#   #idf = subset(idf, Capture_Method %in% Method)
#   plotdf = subset(gdf, Capture_Method %in% Method)
## } else{
#   plotdf = gdf%>%dplyr::filter(!is.na(N_rel))
# }

#Sections to pull specific contrasts into wide format for survival comparisons.
#Using controls is risky becasue we rarely acheive the same SAR, etc.
#If a 1:1 stocking objective was achieved then the need for this disappears, and can just use glm

#f = paste("NetXN ~ Lk_yr + Int.Age*",Contrast)

#plot_gdf$Int.Age = as.factor(plot_gdf$Int.Age)

#model = stats::glm(as.formula(f), data = plot_gdf)
#plot(model)

#sjPlot::plot_model(model, type = "pred")
#jtools::effect_plot(model, pred = Int.Age, plot.points = TRUE, data = plot_gdf)
#jtools::effect_plot(model, pred = Genotype, plot.points = TRUE, data = plot_gdf)

#get(Contrast)


#if (!is.null(Contrast)) {
  
#  Contrast_possible = c("Genotype", "SAR_cat", "Strain")
  
#  controls = dplyr::setdiff(Contrast_possible, Contrast)
#} 


#  surv_gdf = plot_gdf%>%
#    dplyr::group_by(Waterbody_Name, Lk_yr, sby_code, Int.Age, !!!rlang::syms(controls), !!!rlang::syms(Contrast))%>%
#   dplyr::summarize(groups = dplyr::n(), xN = sum(NetXN), Nr = sum(N_rel))%>%
#   tidyr::pivot_wider(names_from = Contrast, values_from = c(xN, Nr), names_sort = TRUE)



