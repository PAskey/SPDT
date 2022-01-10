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
#' "survival" (standardized relative catch rate of one contrast group to another), 
#' A series of plots that summarize all age classes captured in a given sampling event.
#' The plots show changes in size, numbers or maturation over time if multiple age classes were captured.
#' 
#' "survival' (relative survival of each group, value of 1 equivalent, estimated relative survival shown by dotted line)
#' 
#' "catch" (catch rates of each group - catch curve type data),
#' 
#' "age_freq" (same data as above, but columns instead of points),
#' 
#' "mu_growth_FL" (average length per age), 
#' 
#' "mu_growth_wt" (average weight per age),
#' 
#' "growth_FL" (individual lengths per age),
#' 
#' "growth_wt" (raw weights by age),
#' 
#' "maturation" (the proportion mature per age),
#' 
#' "maturation_by_sex" (partitions data used in plot above by sex) 
#' 
#' A series of plots looking at the overall size distribution of the entire population in a lake (all age classes)
#' 
#' "FL_freq" (fork-length frequencies per lake-sample session as lines),
#' 
#' "FL_density"(fork-length frequencies per lake-sample session as smoothed densities),
#' 
#' "FL_hist"(fork-length frequencies per lake-sample session as histograms), 
#' 
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
#' 
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


SPDTplot <- function(Metric = NULL, Method = "GN", min_N = 0, save_png = FALSE){

  #If no specific contrast is given in SPDT data, then make defaults for colouring and shape schemes
  
  if (is.null(Contrast)){
    Contrast = "Genotype"
    controls = c("Strain")
  }
  
  if (Metric == "survival"){ 
    
 p =  ggplot2::ggplot(data = wide_df, ggplot2::aes(x = .data$Waterbody_Name, group = Int.Age, fill = Int.Age, colour = Sig_p,  shape = get(controls[1])))+
    ggplot2::geom_hline(yintercept = 1)+
    ggplot2::geom_point(ggplot2::aes(y = .data$surv_diff, size = .data$N), stroke = 1, alpha = 0.6, position = ggplot2::position_dodge(width = 0.5))+
    ggplot2::geom_line(ggplot2::aes(y = .data$avg_surv), colour = "black", lty = 2)+
    ggplot2::scale_y_continuous(trans='log10')+
    ggplot2::labs(y = "Relative survival")+
    ggplot2::scale_shape_manual(values = rep(21:25, 5))+
    viridis::scale_fill_viridis()+
    ggplot2::scale_colour_manual(values = c("black", "red"))+
    ggplot2::facet_wrap(~.data$Comparison, scales = "free")+
    ggplot2::labs(shape = controls[1], colour = "p < 0.05")+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text.x=element_text(angle=45, hjust=1))
  } 
  
  
  
  
  

  #For colouring plots by brood year (with fewer unique levels), set up grouping column col_group
  #TD add filter to remove age classes that don't have releases for multiple levels of Contrast

  
  plot_gdf <- gdf%>%
    dplyr::filter(Capture_Method == Method, !is.na(N_rel), N >= min_N)%>%
    dplyr::mutate(Year_Season = paste0(Year,"_",Season),
                  Dec.Age = round(Int.Age+(lubridate::month(avg_sample_date)-1)/12, 1))%>%
    dplyr::group_by(Lk_yr_age, Season)%>%
    dplyr::filter(n()>1, sum(N)>0)%>%
    dplyr::ungroup()
  
  plot_idf <- idf%>%
    dplyr::filter(Capture_Method == Method, !is.na(N_rel))%>%
    dplyr::mutate(Year_Season = paste0(Year,"_",Season),
                  Dec.Age = round(Int.Age+(lubridate::month(Date)-1)/12, 1))%>%
    dplyr::group_by(Lk_yr_age, Season)%>%
    dplyr::filter(paste0(Lk_yr_age, Season)%in% paste0(plot_gdf$Lk_yr_age, plot_gdf$Season))%>%
    dplyr::ungroup()

  #Could add an additional filter [!is.na(plot_idf$Length_mm),]
  #But it is better to get the warning I think. It still plots
  
  #Should add an option to include or eliminate outliers.
  #dplyr::filter(Outlier %in% c(0,NA))
  
  
  #Make colour and fill = to Contrast always.
  
#Base ggplot call for a plots based on group data with age as x-axis
pg = ggplot2::ggplot(data = plot_gdf, ggplot2::aes(x = .data$Int.Age, fill = get(Contrast), colour = get(Contrast), shape = get(controls[1])))+#, y = .data$NetXN
    ggplot2::scale_shape_manual(values = rep(21:25, 5))+
    viridis::scale_fill_viridis(discrete = TRUE)+
    viridis::scale_colour_viridis(discrete = TRUE)+
    scale_y_continuous(limits = c(0,NA),expand=expansion(add=c(0,10))) +
    ggplot2::facet_wrap(~.data$Waterbody_Name+.data$Year_Season, scales = "free_y")+
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(1))+
    ggplot2::labs(x = "Age", fill = Contrast, colour = Contrast, shape = controls[1])+
    ggplot2::theme_bw()
  
#Survival plot additions.  
if (Metric == "catch"){
p = pg + 
    ggplot2::geom_point(ggplot2::aes(y = .data$NetXN),size = 4, alpha = 0.6, position = ggplot2::position_dodge(width = 0.2))+
    #ggplot2::scale_y_continuous(trans='log10')+
    ggplot2::labs(y = "Catch (selectivity adjusted)")
}

#Avg growth plot additions.  
if (Metric == "mu_growth_FL"){
  p = pg + 
    ggplot2::geom_point(ggplot2::aes(y = .data$NetX_FL),size = 4, alpha = 0.7, position = ggplot2::position_dodge(width = 0.5))+
    ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$NetX_FL-.data$sd_FL, ymax=.data$NetX_FL+.data$sd_FL), width=.2, position=ggplot2::position_dodge(.5))+
    ggplot2::scale_y_continuous(breaks = scales::breaks_width(50))+
    ggplot2::labs(y = "Fork length (mm, selectivity adjusted)")
}

if (Metric == "mu_growth_wt"){
  p = pg + 
    ggplot2::geom_point(ggplot2::aes(y = .data$NetX_wt),size = 4, alpha = 0.7, position = ggplot2::position_dodge(width = 0.5))+
    ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$NetX_wt-.data$sd_wt, ymax=.data$NetX_wt+.data$sd_wt), width=.2, position=ggplot2::position_dodge(.5))+
    ggplot2::scale_y_continuous(breaks = scales::breaks_width(200))+
    ggplot2::labs(y = "Weight (g, selectivity adjusted)")
}

if (Metric == "age_freq"){
  
  p = pg + 
    ggplot2::geom_col(ggplot2::aes(y  =.data$NetXN),position = ggplot2::position_dodge2(preserve = "single"), alpha = 0.6)+
    ggplot2::facet_wrap(.data$Waterbody_Name~.data$Year_Season, scales = "free_y")+
    ggplot2::scale_y_continuous(breaks = scales::breaks_width(10))+
    ggplot2::labs(y = "Catch (selectivity adjusted)")+
    ggplot2::theme_bw()
}

if (Metric == "maturation"){
  p = pg + 
    ggplot2::geom_point(ggplot2::aes(x = .data$Dec.Age, y = .data$p_mat), size = 4, alpha = 0.7, position = ggplot2::position_dodge(width = 0.2))+
    #ggplot2::facet_wrap(~get(controls[1]))+
    ggplot2::facet_wrap(~.data$Waterbody_Name)+
    ggplot2::ylim(0,1)+
    ggplot2::geom_smooth(se = FALSE, ggplot2::aes(y = .data$p_mat,colour = get(Contrast)))+
    ggplot2::theme_bw()+
    ggplot2::labs(x = "Age", y = "Proportion mature or maturing", 
                  shape = controls[1], fill = Contrast, colour = Contrast)  
} 

##___________________________________________________________________________
##INDIVIDUAL LEVEL PLOTS

    

pi = ggplot2::ggplot(data = plot_idf, ggplot2::aes(colour = get(Contrast), fill = get(Contrast), shape = get(controls[1]), group=get(Contrast)))+
  #ggplot2::geom_point(size = 4, alpha = 0.4, shape = 21, position = ggplot2::position_jitter(width = 0.1))+
  ggplot2::scale_shape_manual(values = rep(21:25, 5))+
  #ggplot2::geom_boxplot(ggplot2::aes(group = Genotype))+
  viridis::scale_fill_viridis(discrete = TRUE)+
  viridis::scale_colour_viridis(discrete = TRUE)+
  ggplot2::facet_wrap(.data$Waterbody_Name~.data$Year_Season, scales = "free")+
  ggplot2::labs(fill = Contrast, colour = Contrast, shape = controls[1])+
  ggplot2::theme_bw()
  #ggplot2::guides(colour=ggplot2::guide_legend(override.aes = list(shape = 21)),fill=ggplot2::guide_legend(override.aes = list(shape = 21)))+
  #ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "black"))
  
  
if(Metric == "condition"){
  #plot_idf = plot_idf[0<plot_idf$Weight_g,]
  
  #plot_idf$logW = log(plot_idf$Weight_g)
  #plot_idf$logL = log(plot_idf$Length_mm)
  #lm <- lm(data=plot_idf,logW ~ logL + as.factor(Lk_yr) + Strain)
  #ggiraphExtra::ggPredict(lm,se=TRUE,interactive=TRUE)
  #preds = ggeffects::ggpredict(lm, terms = c("Lk_yr", "logL"))
  #ggeffects::plot(preds)
  #plot(preds, add.data = TRUE, facet = TRUE)
  
p = pi + 
  ggplot2::geom_point(ggplot2::aes(x = .data$Length_mm, y = .data$Weight_g), size = 4, alpha = 0.5)+
  ggplot2::scale_x_log10()+
  ggplot2::scale_y_log10()
}



 if (Metric == "growth_wt"){
 
p = pi + 
     ggplot2::geom_point(ggplot2::aes(x = .data$Dec.Age, y = .data$Weight_g), size = 4, alpha = 0.5, position = ggplot2::position_dodge(width = 0.3))+
     ggplot2::labs(x = "Age", y = "Weight (g)", fill = Contrast, colour = Contrast)+
     ggplot2::scale_y_continuous(breaks = scales::breaks_width(200))+
     ggplot2::scale_x_continuous(breaks = scales::breaks_width(1))+
     ggplot2::theme_bw()+
     ggplot2::facet_wrap(.data$Waterbody_Name~.data$Year_Season)#, scales = "free_y")
 }

if (Metric == "growth_FL"){
  
  p = pi + 
    ggplot2::geom_point(ggplot2::aes(x = .data$Dec.Age, y = .data$Length_mm), size = 4, alpha = 0.5, position = ggplot2::position_dodge(width = 0.3))+
    ggplot2::labs(x = "Age", y = "Fork Length (mm)", fill = Contrast, colour = Contrast)+
    ggplot2::scale_y_continuous(breaks = scales::breaks_width(50))+
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(1))+
    ggplot2::theme_bw()+
    ggplot2::facet_wrap(.data$Waterbody_Name~.data$Year_Season)#, scales = "free_y")
}



  if (Metric == "FL_age_facets"){
    p = pi+
      ggplot2::geom_point(ggplot2::aes(x = .data$Waterbody_Name, y = .data$Length_mm),size = 4, alpha = 0.5, position = ggplot2::position_dodge(width = 0.75))+
      ggplot2::facet_wrap(~.data$Dec.Age)+
      ggplot2::labs(x = "", y = "Fork Length (mm)", fill = Contrast)+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, vjust = 1, hjust=1))
  }
  
  
  
  
 
  if (Metric == "FL_density"){
    if(min_N<20){warning("Min_N should be >=20 for pop length frequency plots")}
    
p = pi+
    ggplot2::geom_density(ggplot2::aes(x = .data$Length_mm), alpha = 0.4, lwd = 1, adjust = 1/2)+
    ggplot2::xlim(c(100,NA))+
    ggplot2::theme_bw()+
    ggplot2::labs( fill = Contrast, colour = Contrast) 
   

  }

if (Metric == "FL_freq"){
  if(min_N<20){warning("Min_N should be >=20 for pop length frequency plots")}
  
  p = pi+
    ggplot2::geom_freqpoly(ggplot2::aes(x = .data$Length_mm), alpha = 0.9, lwd = 1)+
    ggplot2::xlim(c(100,450))+
    ggplot2::theme_bw()+
    ggplot2::labs(colour = Contrast)+ 
    ggplot2::guides(colour=ggplot2::guide_legend(override.aes = list(line = 2)),fill="none")
  
}

if (Metric == "FL_hist"){
  if(min_N<20){warning("Min_N should be >=20 for pop length frequency plots")}
  
  p = pi+
    ggplot2::geom_histogram(ggplot2::aes(x = .data$Length_mm), alpha = 0.2, lwd = 1, position = "identity")+
    ggplot2::xlim(c(100,450))+
    ggplot2::theme_bw()+
    ggplot2::labs( fill = Contrast, colour = Contrast) 
  
}


  
  if (Metric == "maturation_by_sex"){
    if(min_N<5){warning("Min_N should be >=5 for maturation plots")}
    mat_df = plot_idf%>%
              dplyr::filter(Maturity != 'UNK', !is.na(Sex))%>%
              dplyr::group_by(Waterbody_Name, WBID, Year, Strain, Genotype, Int.Age, Dec.Age, Sex)%>%
              dplyr::summarize(N = dplyr::n(),
                               Nkn = sum(.data$Maturity != 'UNK', na.rm = TRUE),
                               Nm = sum(.data$Maturity != 'IM'& .data$Maturity != 'UNK', na.rm = TRUE),
                               p_mat = sum(.data$Maturity != 'IM'& .data$Maturity != 'UNK', na.rm = TRUE)/sum(.data$Maturity != 'UNK', na.rm = TRUE))%>%
              dplyr::filter(N>=min_N)%>%
              dplyr::ungroup()
      
    
    p = ggplot2::ggplot(data = mat_df, 
                        ggplot2::aes(x = .data$Dec.Age, y = .data$p_mat, 
                                     fill = get(Contrast), colour = get(Contrast)))+
      ggplot2::geom_point(size = 4, alpha = 0.7, position = ggplot2::position_jitterdodge(  jitter.width = .1,
                                                                                            jitter.height = 0.02,
                                                                                            dodge.width = 0.3, seed = 1))+
      ggplot2::scale_shape_manual(values = 21)+
      viridis::scale_fill_viridis(discrete = TRUE)+
      viridis::scale_colour_viridis(discrete = TRUE)+
      ggplot2::facet_grid(.data$Sex~.data$Genotype)+
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



