rm(list=ls())
dev.off()
library(readxl)
library(tidyverse)
library(gridExtra)
library(magrittr)
library(viridis)
library(gganimate)
library(easycsv)

# geometric functions ------------------------------------------------
circle = function(center = c(0,0), diameter = 1, npoints = 100) {
    r = diameter / 2
    tt = seq(0, 2 * pi,length.out = npoints)
    xx = center[1] + r * cos(tt)
    yy = center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}

# Ethovision settings ------------------------------------------------
arenaCenter = c(8.11,-1.78)
arenaDiameter = 150
TQ_platformCenter = c(49.46,.32)
TQ_platformDiameter = 13.165
OppQ_platformCenter = c(NA,NA)
OppQ_platformDiameter = 150

# simple HEATMAP for EthoVision --------------------------------------

# files_dir = choose_dir()
setwd("./Ethovision/data"); files_dir = getwd()

files = list.files(files_dir)

all_coor = data.frame()

skip_lines = as.numeric(unlist(strsplit(readLines(files[1])[1], "\""))[4])

# ii = 8
for(ii in 1:length(files)) {
    print(ii)
    temp = 
        suppressMessages(suppressWarnings(read_delim(files[ii], col_names = c("Time","Seconds", "x","y"), skip = skip_lines, delim = ";"))) %>%
        select(Seconds,x,y) %>%
        mutate(x = as.numeric(x),
               y = as.numeric(y),
               Subject = ii,
               Frame = ceiling(Seconds / 5)) %>% # default 5
        filter(between(Frame,1,max(Frame))) %>% # remove first frames that include floaters: default 2
        select(Subject,Seconds,Frame,x,y)
    all_coor %<>% bind_rows(temp)
}

all_coor %<>% tbl_df()

p1 = ggplot(all_coor, aes(x,y)) + theme_bw() +
    geom_point(color = "red", size = .2, alpha = .25) +
    geom_path(aes(x,y), dat = circle(arenaCenter, arenaDiameter, npoints = 100), color = "white") +
    geom_path(aes(x,y), dat = circle(TQ_platformCenter, TQ_platformDiameter, npoints = 100), color = "white") +
    # geom_vline(xintercept = 216, color = "white") + geom_hline(yintercept = 224.5, color = "white") +
    coord_fixed() +
    scale_fill_viridis(name = "density", option = "B") +
    theme(legend.position = "bottom", legend.text = element_blank(), legend.title = element_text(),
          panel.grid = element_blank(), panel.background = element_rect("black"),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
p2 = ggplot(all_coor, aes(x,y)) + theme_bw() +
    stat_density_2d(geom = "tile", aes(fill = ..density..), contour = FALSE) +
    geom_path(aes(x,y), dat = circle(arenaCenter, arenaDiameter, npoints = 100), color = "white") +
    geom_path(aes(x,y), dat = circle(TQ_platformCenter, TQ_platformDiameter, npoints = 100), color = "white") +
    # geom_vline(xintercept = 216, color = "white") + geom_hline(yintercept = 224.5, color = "white") +
    coord_fixed() +
    scale_fill_viridis(name = "density", option = "B") +
    theme(legend.position = "none", legend.text = element_blank(), legend.title = element_text(),
          panel.grid = element_blank(), panel.background = element_rect("black"),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank())

# plot individual static traces
p1 + facet_wrap(~Subject, nrow = 4, ncol = 10)

# plot collective static traces and heatmap
grid.arrange(p1, p2, ncol = 2)

# plot collective static traces and heatmap with individual static traces
grid.arrange(arrangeGrob(p1, p2, ncol = 2), p1 + facet_wrap(~Subject, nrow = 4, ncol = 10), nrow = 2)

# plot dynamic collective heatmap
p2 + transition_time(Frame) + labs(title = "Heatmap: general distribution\nFrame: {frame_time}")

# plot dynamic collective heatmap; adjust frame rate
p3 = p2 + transition_time(Frame) + labs(title = "Frame: {frame_time}"); animate(p3, fps = 8)

# grouped HEATMAP for EthoVision -------------------------------------

# files_dir = choose_dir()
setwd("./Ethovision/data"); files_dir = getwd()

files = list.files(files_dir)
skip_lines = as.numeric(unlist(strsplit(readLines(files[1])[1], "\""))[4])
table = read_excel(path = "../trial_list.xlsx")

all_coor = data.frame()

# ii = 1
for(ii in 1:nrow(table)) {
    print(ii)
    file = paste0(table$Track[ii], ".txt")
    
    # IN PROGRESS #
    temp = 
        suppressMessages(suppressWarnings(read_delim(files[ii], col_names = c("Time","Seconds", "x","y"), skip = skip_lines, delim = ";"))) %>%
        select(Seconds,x,y) %>%
        mutate(x = as.numeric(x),
               y = as.numeric(y),
               Subject = ii,
               Genotype = 
                   Frame = ceiling(Seconds / 5)) %>% # default 5
        filter(between(Frame,1,NA)) %>% # remove first frames that include floaters: default 2
        select(Subject,Seconds,Frame,x,y)
    all_coor %<>% bind_rows(temp)
}

all_coor %<>% tbl_df()