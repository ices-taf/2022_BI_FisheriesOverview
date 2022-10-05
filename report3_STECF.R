
library(icesTAF)
taf.library(icesFO)
library(sf)
library(ggplot2)
library(dplyr)

##########
#Load data
##########


frmt_effort <- read.taf("data/frmt_effort.csv")
frmt_landings <- read.taf("data/frmt_landings.csv")

################################
## 2: STECF effort and landings#
################################

bi_df$country <- bi_df$`country name`
bi_df$total.kW.days.at.sea <- bi_df$`total kW days at sea`

#~~~~~~~~~~~~~~~#
# Effort by country
#~~~~~~~~~~~~~~~#
plot_stecf <- function(x, type, variable = NULL, cap_year, cap_month, line_count, stecf_report, return_data = FALSE) {
        
        if(type == "effort"){
                if(variable=="COUNTRY"){
                        dat <- dplyr::rename(bi_df, "type_var" = "country",
                                              "VALUE" = "total.kW.days.at.sea")
                        }
                if(variable=="GEAR"){
                        dat <- dplyr::rename(bi_df, "type_var" ="gear_class",
                                              "VALUE" = "total.kW.days.at.sea")
                }
                Label <- "Nominal effort (1000 kW days at sea)"
        }
        if(type == "landings"){
                dat <- dplyr::rename(landings_BtS, "type_var" ="gear_class",
                                     "VALUE" = "total.live.weight.landed..tonnes.")
                Label <- "Landings (thousand tonnes)"
        }
        
        dat$type_var <- as.factor(dat$type_var)
        
        dat$VALUE <- as.numeric(dat$VALUE)
        Plot <- dplyr::group_by(dat,type_var)
        Plot <- dplyr::summarise(Plot,typeTotal = sum(VALUE, na.rm = TRUE))
        Plot <- dplyr::arrange(Plot,-typeTotal)        
        Plot <- dplyr::filter(Plot, typeTotal >= 1)
        Plot <- dplyr::mutate(Plot,RANK = dplyr::min_rank(dplyr::desc(typeTotal))) 
        
        Plot <- subset(Plot,select = -typeTotal)
        dat <- dplyr::left_join(dat, Plot)
        # dat <- dat[complete.cases(dat), ]
        # dat <- dat %>% filter(typeTotal>0)
        
        # confidential data here, "Belgium"     "Denmark"     "Spain"       "France"      "Ireland"     "Netherlands" "Poland"      "Portugal"   
        dat <- dat %>% filter(confidential == "N")
        dat <- dplyr::mutate(dat, type_var = replace(type_var, RANK > 7, "other"))
        dat <- dplyr::group_by(dat,type_var, year) 
        dat <- dplyr::summarise(dat, typeTotal = sum(VALUE, na.rm = TRUE))
        dat <- dplyr::filter(dat,!is.na(year))
        
        dat <- rbind(dat[!dat$type_var == "other",],
                     dat[dat$type_var == "other",])
        
        my_caption = sprintf("STECF. Accessed %s/%s.",
                             "October",
                             "2021")
        
        cap_lab <- ggplot2::labs(title = "", x = "", y = Label,
                                 caption = my_caption)
        
        colList <- ggthemes::tableau_color_pal('Tableau 20')(7 + 1)
        
        order <- dplyr::group_by(dat, type_var)
        order <- dplyr::summarise(order, total = sum(typeTotal, na.rm = TRUE))
        order <- dplyr::arrange(order, -total)
        order <- dplyr::ungroup(order)
        order <- dplyr::mutate(order,type_var = factor(type_var, type_var))
        
        dat$type_var <- factor(dat$type_var,
                               levels = order$type_var[order(order$total)])
        
        myColors <- colList[1:length(unique(dat$type_var))]
        names(myColors) <- levels(dat$type_var)
        myColors["other"] <- "#7F7F7F"
        if(type == "effort"){
                dat$typeTotal <- dat$typeTotal/1000
        }
        
        pl <- ggplot2::ggplot(dplyr::ungroup(dat), ggplot2::aes(x = year, y = typeTotal)) +
                ggplot2::scale_fill_manual(values = myColors) +
                ggplot2::scale_color_manual(values = myColors) +
                ggplot2::scale_x_continuous(breaks = seq(min(dat$year, na.rm = TRUE),
                                                         max(dat$year, na.rm = TRUE), by = 2)) +
                ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = 2018, y = -Inf, yend = -Inf), color = "grey50")+
                ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
                ggplot2::expand_limits(x = c(min(dat$year, na.rm = TRUE), 2022)) + # So that we have enough room along x-axis for labels.
                cap_lab +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(legend.position = 'none',
                               panel.grid.minor = ggplot2::element_blank(),
                               panel.grid.major = ggplot2::element_blank(),
                               panel.border = ggplot2::element_blank(),
                               strip.background = ggplot2::element_blank(),
                               plot.caption = ggplot2::element_text(size = 6),
                               axis.line = ggplot2::element_blank())
        
        
        pl <- pl + ggplot2::geom_line(ggplot2::aes(color = type_var),
                                      alpha = .9, position = "identity")
        dat2 <- dplyr::filter(dat,year == max(year, na.rm = TRUE))
        pl <- pl + ggrepel::geom_label_repel(data = dat2 ,
                                             ggplot2::aes(label = type_var,
                                                          fill = type_var),
                                             nudge_x = 3,
                                             label.size = 0.2,
                                             segment.size = 0.25,
                                             size = 2,
                                             color = 'white',
                                             force = 3,
                                             segment.color = 'grey60')
        
        
        if(return_data == T){
                dat
        }else{
                pl
        }
}


        #Plot
plot_stecf(frmt_effort,type = "effort", variable= "COUNTRY", "2019","August", 9, "15-23", return_data = FALSE)
frmt_effort <- dplyr::filter(frmt_effort, COUNTRY %in% c("Sweden", "Poland", "Germany", "Denmark", "Lithuania","Latvia"))
plot_stecf(frmt_effort,type = "effort", variable= "COUNTRY", "2019","August", 9, "15-23", return_data = FALSE)
ggplot2::ggsave(paste0(year_cap, "_", ecoreg,"_FO_STECF_effortCountry.png"), path = "report/", width = 178, height = 130, units = "mm", dpi = 300)
        #data
dat <- plot_stecf(frmt_effort,type = "effort", variable= "COUNTRY", "2019","August", 9, "15-23", return_data = TRUE)
write.taf(dat, file= paste0(year_cap, "_", ecoreg,"_FO_STECF_effortCountry.csv"), dir = "report")


#~~~~~~~~~~~~~~~#
#Effort by gear
#~~~~~~~~~~~~~~~#
plot_stecf <- function(x, type, variable = NULL, cap_year, cap_month, line_count, stecf_report, return_data = FALSE) {
        
        if(type == "effort"){
                if(variable=="COUNTRY"){
                        dat <- dplyr::rename_(effort_BtS, "type_var" ="country.name",
                                              "VALUE" = "total.kW.days.at.sea")}
                if(variable=="GEAR"){
                        dat <- dplyr::rename_(effort_BtS, "type_var" ="gear_class",
                                              "VALUE" = "total.kW.days.at.sea")
                }
                Label <- "Nominal effort (1000 kW days at sea)"
        }
        if(type == "landings"){
                dat <- dplyr::rename(landings_BtS, "type_var" ="gear_class",
                                     "VALUE" = "total.live.weight.landed..tonnes.")
                Label <- "Landings (thousand tonnes)"
        }
        
        dat$type_var <- as.factor(dat$type_var)
        
        dat$VALUE <- as.numeric(dat$VALUE)
        Plot <- dplyr::group_by(dat,type_var)
        Plot <- dplyr::summarise(Plot,typeTotal = sum(VALUE, na.rm = TRUE))
        Plot <- dplyr::arrange(Plot,-typeTotal)        
        Plot <- dplyr::filter(Plot, typeTotal >= 1)
        Plot <- dplyr::mutate(Plot,RANK = dplyr::min_rank(dplyr::desc(typeTotal))) 
        
        Plot <- subset(Plot,select = -typeTotal)
        dat <- dplyr::left_join(dat, Plot)
        # dat <- dat[complete.cases(dat), ]
        dat <- dplyr::mutate(dat, type_var = replace(type_var, RANK > 7, "other"))
        dat <- dplyr::group_by(dat,type_var, year) 
        dat <- dplyr::summarise(dat, typeTotal = sum(VALUE, na.rm = TRUE))
        dat <- dplyr::filter(dat,!is.na(year))
        
        dat <- rbind(dat[!dat$type_var == "other",],
                     dat[dat$type_var == "other",])
        
        my_caption = sprintf("STECF %s. Accessed %s/%s.",
                             "19-11",
                             "August",
                             "2020")
        
        cap_lab <- ggplot2::labs(title = "", x = "", y = Label,
                                 caption = my_caption)
        
        colList <- ggthemes::tableau_color_pal('Tableau 20')(7 + 1)
        
        order <- dplyr::group_by(dat, type_var)
        order <- dplyr::summarise(order, total = sum(typeTotal, na.rm = TRUE))
        order <- dplyr::arrange(order, -total)
        order <- dplyr::ungroup(order)
        order <- dplyr::mutate(order,type_var = factor(type_var, type_var))
        
        dat$type_var <- factor(dat$type_var,
                               levels = order$type_var[order(order$total)])
        
        myColors <- colList[1:length(unique(dat$type_var))]
        names(myColors) <- levels(dat$type_var)
        myColors["other"] <- "#7F7F7F"
        if(type == "effort"){
                dat$typeTotal <- dat$typeTotal/1000
        }
        
        pl <- ggplot2::ggplot(dplyr::ungroup(dat), ggplot2::aes(x = year, y = typeTotal)) +
                ggplot2::scale_fill_manual(values = myColors) +
                ggplot2::scale_color_manual(values = myColors) +
                ggplot2::scale_x_continuous(breaks = seq(min(dat$year, na.rm = TRUE),
                                                         max(dat$year, na.rm = TRUE), by = 2)) +
                ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = 2018, y = -Inf, yend = -Inf), color = "grey50")+
                ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
                ggplot2::expand_limits(x = c(min(dat$year, na.rm = TRUE), 2022)) + # So that we have enough room along x-axis for labels.
                cap_lab +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(legend.position = 'none',
                               panel.grid.minor = ggplot2::element_blank(),
                               panel.grid.major = ggplot2::element_blank(),
                               panel.border = ggplot2::element_blank(),
                               strip.background = ggplot2::element_blank(),
                               plot.caption = ggplot2::element_text(size = 6),
                               axis.line = ggplot2::element_blank())
        
        
        pl <- pl + ggplot2::geom_line(ggplot2::aes(color = type_var),
                                      alpha = .9, position = "identity")
        dat2 <- dplyr::filter(dat,year == max(year, na.rm = TRUE))
        pl <- pl + ggrepel::geom_label_repel(data = dat2 ,
                                             ggplot2::aes(label = type_var,
                                                          fill = type_var),
                                             nudge_x = 3,
                                             label.size = 0.2,
                                             segment.size = 0.25,
                                             size = 2,
                                             color = 'white',
                                             force = 3,
                                             segment.color = 'grey60')
        
        
        if(return_data == T){
                dat
        }else{
                pl
        }
}





        #Plot
plot_stecf(frmt_effort,type = "effort", variable= "GEAR", "2019","August", 9, "15-23")
ggplot2::ggsave(paste0(year_cap, "_", ecoreg,"_FO_STECF_effortGear.png"), path = "report/", width = 178, height = 130, units = "mm", dpi = 300)

        #data
dat<-plot_stecf(frmt_effort,type = "effort", variable= "GEAR", "2019","August", 9, "15-23", return_data = TRUE)
write.taf(dat, file= paste0(year_cap, "_", ecoreg,"_FO_STECF_effortGear.csv"), dir = "report")

#~~~~~~~~~~~~~~~#
#Landings by country
#~~~~~~~~~~~~~~~#
plot_stecf <- function(x, type, variable = NULL, cap_year, cap_month, line_count, stecf_report, return_data = FALSE) {
        
        if(type == "effort"){
                if(variable=="COUNTRY"){
                        dat <- dplyr::rename_(x, "type_var" ="COUNTRY",
                                              "VALUE" = "EFFORT")}
                if(variable=="GEAR"){
                        dat <- dplyr::rename_(x, "type_var" ="GEAR",
                                              "VALUE" = "EFFORT")
                }
                Label <- "Nominal effort (1000 kW days at sea)"
        }
        if(type == "landings"){
                dat <- dplyr::rename(bi_df, "type_var" ="gear_class",
                                      "VALUE" = "total live weight landed (tonnes)")
                Label <- "Landings (thousand tonnes)"
        }
        
        dat$type_var <- as.factor(dat$type_var)
        
        dat$VALUE <- as.numeric(dat$VALUE)
        Plot <- dplyr::group_by(dat,type_var)
        Plot <- dplyr::summarise(Plot,typeTotal = sum(VALUE, na.rm = TRUE))
        Plot <- dplyr::arrange(Plot,-typeTotal)        
        Plot <- dplyr::filter(Plot, typeTotal >= 1)
        Plot <- dplyr::mutate(Plot,RANK = dplyr::min_rank(dplyr::desc(typeTotal))) 
        
        Plot <- subset(Plot,select = -typeTotal)
        dat <- dplyr::left_join(dat, Plot)
        # dat <- dat[complete.cases(dat), ]
        dat <- dplyr::mutate(dat, type_var = replace(type_var, RANK > 6, "other"))
        dat <- dplyr::group_by(dat,type_var, year) 
        dat <- dplyr::summarise(dat, typeTotal = sum(VALUE, na.rm = TRUE))
        dat <- dplyr::filter(dat,!is.na(year))
        
        dat <- rbind(dat[!dat$type_var == "other",],
                     dat[dat$type_var == "other",])
        
        my_caption = sprintf("STECF. Accessed %s/%s.",
                             "October",
                             "2021")
        
        cap_lab <- ggplot2::labs(title = "", x = "", y = Label,
                                 caption = my_caption)
        
        colList <- ggthemes::tableau_color_pal('Tableau 20')(6 + 1)
        
        order <- dplyr::group_by(dat, type_var)
        order <- dplyr::summarise(order, total = sum(typeTotal, na.rm = TRUE))
        order <- dplyr::arrange(order, -total)
        order <- dplyr::ungroup(order)
        order <- dplyr::mutate(order,type_var = factor(type_var, type_var))
        
        dat$type_var <- factor(dat$type_var,
                               levels = order$type_var[order(order$total)])
        
        myColors <- colList[1:length(unique(dat$type_var))]
        names(myColors) <- levels(dat$type_var)
        myColors["other"] <- "#7F7F7F"
        if(type == "effort"){
                dat$typeTotal <- dat$typeTotal/1000
        }
        
        pl <- ggplot2::ggplot(dplyr::ungroup(dat), ggplot2::aes(x = year, y = typeTotal)) +
                ggplot2::scale_fill_manual(values = myColors) +
                ggplot2::scale_color_manual(values = myColors) +
                ggplot2::scale_x_continuous(breaks = seq(min(dat$year, na.rm = TRUE),
                                                         max(dat$year, na.rm = TRUE), by = 2)) +
                ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = 2018, y = -Inf, yend = -Inf), color = "grey50")+
                ggplot2::geom_segment(ggplot2::aes(y = -Inf, yend = Inf, x = -Inf, xend = -Inf), color = "grey50")+
                ggplot2::expand_limits(x = c(min(dat$year, na.rm = TRUE), 2022)) + # So that we have enough room along x-axis for labels.
                cap_lab +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(legend.position = 'none',
                               panel.grid.minor = ggplot2::element_blank(),
                               panel.grid.major = ggplot2::element_blank(),
                               panel.border = ggplot2::element_blank(),
                               strip.background = ggplot2::element_blank(),
                               plot.caption = ggplot2::element_text(size = 6),
                               axis.line = ggplot2::element_blank())
        
        
        pl <- pl + ggplot2::geom_line(ggplot2::aes(color = type_var),
                                      alpha = .9, position = "identity")
        dat2 <- dplyr::filter(dat,year == max(year, na.rm = TRUE))
        pl <- pl + ggrepel::geom_label_repel(data = dat2 ,
                                             ggplot2::aes(label = type_var,
                                                          fill = type_var),
                                             nudge_x = 3,
                                             label.size = 0.2,
                                             segment.size = 0.25,
                                             size = 2,
                                             color = 'white',
                                             force = 3,
                                             segment.color = 'grey60')
        
        
        if(return_data == T){
                dat
        }else{
                pl
        }
}

# confidential landings all years:
# "Belgium"     "Denmark"     "Spain"       "France"      "Ireland"     "Netherlands" "Poland"      "Portugal"   


        #Plot
plot_stecf(frmt_landings,type = "landings", variable= "GEAR", "2019","August", 9, "15-23")
ggplot2::ggsave(paste0(year_cap, "_", ecoreg,"_FO_STECF_landings.png"), path = "report/", width = 178, height = 130, units = "mm", dpi = 300)
        #dat
dat <- plot_stecf(frmt_landings, type = "landings", variable="landings", "2019","August", 9, "15-23", return_data = TRUE)
write.taf(dat, file= paste0(year_cap, "_", ecoreg,"_FO_STECF_landings.csv"), dir = "report")
