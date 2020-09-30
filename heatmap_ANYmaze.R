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

# ANY-maze settings --------------------------------------------------
arenaCenter = c(216,224.5)
arenaDiameter = 418
TQ_platformCenter = c(288,300)
TQ_platformDiameter = 45
OppQ_platformCenter = c(128,142)
OppQ_platformDiameter = 45

# simple HEATMAP for ANY-maze ----------------------------------------

# files_dir = choose_dir()
setwd("./ANYmaze/data"); files_dir = getwd()

files = list.files(files_dir)

all_coor = data.frame()
# ii = 1
for(ii in 1:length(files)) {
    temp = 
        suppressMessages(suppressWarnings(read_csv(files[ii], col_names = c("Time","x","y"), skip = 2))) %>%
        mutate(Subject = ii,
               Seconds = as.numeric(substr(Time,4,5))*60 + as.numeric(substr(Time,7,8)),
               Frame = ceiling(Seconds / 5)) %>%
        filter(between(Frame,3,NA)) %>% # remove first frames that include floaters: default 2
        select(Subject,Seconds,Frame,x,y)
    all_coor %<>% bind_rows(temp)    
}

all_coor %<>% as_tibble()

p1 = ggplot(all_coor, aes(x,y)) + theme_bw() +
    geom_point(color = "red", size = .2, alpha = .25) +
    scale_x_continuous(limits = c(0,435)) + scale_y_continuous(limits = c(0,435)) +
    geom_path(aes(x,y), dat = circle(arenaCenter, arenaDiameter, npoints = 100), color = "white") +
    geom_path(aes(x,y), dat = circle(TQ_platformCenter, TQ_platformDiameter, npoints = 100), color = "white") +
    geom_vline(xintercept = 216, color = "white") + geom_hline(yintercept = 224.5, color = "white") +
    coord_fixed() +
    scale_fill_viridis(name = "density", option = "B") +
    theme(legend.position = "bottom", legend.text = element_blank(), legend.title = element_text(),
          panel.grid = element_blank(), panel.background = element_rect("black"),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
p2 = ggplot(all_coor, aes(x,y)) + theme_bw() +
    stat_density_2d(geom = "tile", aes(fill = ..density..), contour = FALSE) +
    scale_x_continuous(limits = c(0,435)) + scale_y_continuous(limits = c(0,435)) +
    geom_path(aes(x,y), dat = circle(arenaCenter, arenaDiameter, npoints = 100), color = "white") +
    geom_path(aes(x,y), dat = circle(TQ_platformCenter, TQ_platformDiameter, npoints = 100), color = "white") +
    geom_vline(xintercept = 216, color = "white") + geom_hline(yintercept = 224.5, color = "white") +
    coord_fixed() +
    scale_fill_viridis(name = "density", option = "B") +
    theme(legend.position = "none", legend.text = element_blank(), legend.title = element_text(),
          panel.grid = element_blank(), panel.background = element_rect("black"),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank())

# plot individual static traces
# p1 + facet_wrap(~Subject, nrow = 4, ncol = 10)

# plot collective static traces and heatmap
# grid.arrange(p1, p2, ncol = 2)

# plot collective static traces and heatmap with individual static traces
# grid.arrange(arrangeGrob(p1, p2, ncol = 2), p1 + facet_wrap(~Subject, nrow = 4, ncol = 10), nrow = 2)

# plot static collective heatmap
p2
setwd("..")
ggsave("./probe2_static.png")

# plot dynamic collective heatmap
p2_dyn = p2 + transition_time(Frame) + labs(title = "Heatmap: general distribution\nFrame: {frame_time}")
anim_save("./probe2_dynamic.gif", p2_dyn)

# plot dynamic collective heatmap; adjust frame rate
# p3 = p2 + transition_time(Frame) + labs(title = "Frame: {frame_time}"); animate(p3, fps = 8) 

# grouped HEATMAP for ANY-maze ---------------------------------------

# files_dir = choose_dir()
setwd("./ANYmaze")
trial_list = read_excel("trial_list.xlsx")
setwd("./data"); files_dir = getwd()

files = list.files(files_dir)

all_coor = data.frame()
# ii = 1
for(ii in 1:nrow(trial_list)) {
    temp =
        suppressMessages(suppressWarnings(read_csv(paste0(trial_list$Track[ii], ".csv"), col_names = c("Time","x","y"), skip = 2))) %>%
        mutate(Subject = ii,
               Genotype = trial_list$Genotype[ii],
               Seconds = as.numeric(substr(Time,4,5))*60 + as.numeric(substr(Time,7,8)),
               Frame = ceiling(Seconds / 2)) %>%
        filter(between(Frame,3,max(Frame))) %>% # remove first frames that include floaters: default 2
        select(Subject,Genotype,Seconds,Frame,x,y)
    all_coor %<>% bind_rows(temp)    
}
all_coor %<>% tbl_df() %>% mutate(Genotype = factor(Genotype))

p1 = ggplot(all_coor, aes(x,y)) + theme_bw() +
    geom_point(color = "red", size = .2, alpha = .25) +
    scale_x_continuous(limits = c(0,435)) + scale_y_continuous(limits = c(0,435)) +
    geom_path(aes(x,y), dat = circle(arenaCenter, arenaDiameter, npoints = 100), color = "white") +
    geom_path(aes(x,y), dat = circle(TQ_platformCenter, TQ_platformDiameter, npoints = 100), color = "white") +
    geom_vline(xintercept = 216, color = "white") + geom_hline(yintercept = 224.5, color = "white") +
    coord_fixed() +
    facet_wrap(~Genotype) +
    scale_fill_viridis(name = "density", option = "B") +
    theme(legend.position = "bottom", legend.text = element_blank(), legend.title = element_text(),
          panel.grid = element_blank(), panel.background = element_rect("black"),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
p2 = ggplot(all_coor, aes(x,y)) + theme_bw() +
    stat_density_2d(geom = "tile", aes(fill = ..density..), contour = FALSE) +
    scale_x_continuous(limits = c(0,435)) + scale_y_continuous(limits = c(0,435)) +
    geom_path(aes(x,y), dat = circle(arenaCenter, arenaDiameter, npoints = 100), color = "white") +
    geom_path(aes(x,y), dat = circle(TQ_platformCenter, TQ_platformDiameter, npoints = 100), color = "white") +
    geom_vline(xintercept = 216, color = "white") + geom_hline(yintercept = 224.5, color = "white") +
    coord_fixed() +
    facet_wrap(~Genotype) +
    scale_fill_viridis(name = "density", option = "B") +
    theme(legend.position = "none", legend.text = element_blank(), legend.title = element_text(),
          panel.grid = element_blank(), panel.background = element_rect("black"),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank())

# plot collective static traces and heatmap
# grid.arrange(p1, p2, ncol = 2)

# plot dynamic collective heatmap
p2 + transition_time(Frame) + labs(title = "Heatmap: general distribution\nFrame: {frame_time}")

# plot dynamic collective heatmap; adjust frame rate
# p3 = p2 + transition_time(Frame) + labs(title = "Frame: {frame_time}"); animate(p3, fps = 8)