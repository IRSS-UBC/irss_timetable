### generates the IRSS timeline graphic ###

#### IMPORTANCE: if you run this script for the first time, need to authorize R to access read permit to google sheet

#load packages
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(magrittr)
library(prismatic)
library(png)
library(grid)
library(extrafont)
library(Cairo) #nicer pdfs
library(patchwork) 

# Configurable output directory
output_dir <- "X:\\home\\yye\\irss_timetable"

#custom ggplot theme
theme_pt <- function (base_size = 12, base_family = "") 
{
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) + 
    ggplot2::theme(axis.text = ggplot2::element_text(margin = ggplot2::margin(10, 
                                                                              10, 10, 10)), plot.margin = grid::unit(c(1.2, 1.2, 
                                                                                                                       1.2, 1.2), "lines"), axis.ticks.length = grid::unit(0.15, 
                                                                                                                                                                           "cm"), panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_rect(colour = "black", 
                                                                                                     fill = F, size = 1), legend.key = ggplot2::element_blank(), 
                   axis.title.x = ggplot2::element_text(margin = ggplot2::margin(10, 
                                                                                 0, 0, 0)), axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 
                                                                                                                                                          10, 0, 0), angle = 90), strip.text = ggplot2::element_text(lineheight = 1.5), 
                   strip.background = ggplot2::element_blank(), legend.position = "bottom", 
                   legend.title = ggplot2::element_blank())
}

#register fonts (need to run it only when the script is run for the first time)
# font_import()

#load fonts 
loadfonts(device = "win")


#import data from google drive
# you may be asked to authorize access to google sheets
d <- read_sheet("https://docs.google.com/spreadsheets/d/1SRn_2_GyLqsc3_kbkl3CNt5i8DUALjgAowunJsriN48/edit?usp=sharing")

#remove rows without "joined" date
d %<>% filter(!is.na(joined)) 

#remove rows without full date (needs to be 10 characters)
d %<>% filter(str_length(joined) == 10)

#remove where left == x
d %<>% filter(left != "x" | is.na(left))

#convert to date format
d$joined <- lubridate::dmy(d$joined)
d$left <- lubridate::dmy(d$left)
d$left <- if_else(is.na(d$left), today(),d$left) #if no "left" date then assume it is a current member and enter current date

#remove rows when "joined" is AFTER the current date (removes students that will join the lab sometime in the future)
d %<>% filter(joined <= today())




# some data manipulation
d1 <-
  d %>% mutate(name=paste0(firstname," ",lastname)) %>% 
  dplyr::select(-firstname, -lastname)

d2 <- d1 %>% group_by(name) %>% summarise(date=min(joined)) %>% arrange(date)
d2$name <- fct_reorder(d2$name, d2$date)


#convert to long format
d.long <- d1 %>% gather(key, value, -name, -type, -lab_version)

d.long$name <- factor(d.long$name)

# levels(d.long$name) <- levels(d2$name)
d.long$name <- fct_relevel(d.long$name, levels(d2$name))


# d.long$name<- fct_reorder(d.long$name, d.long$value)
d.long$name<- fct_rev(d.long$name)
# 

d.long.labels <- d.long %>% group_by(name)  %>%
  summarise(min_date=min(value))

# unique(d.long$type)

#set colors 
timeline_colors <- c( "#fee391","#a6d96a","#1a9850","#92c5de","#2166ac","#bcbddc", "#807dba")

#################################################################
# removed the 2020 label from the plot as it was not in the background - can change back any time
#################################################################

#define year labels (for the main plot)

x_seq <- seq(2005, 2025, 5)
# 
#timeline_labels <- data.frame(x=ymd(paste0(x_seq, "-01-01")),x_seq, 
                              #y=c(110, 90, 40, 5))

timeline_labels <- data.frame(x=ymd(paste0(x_seq, "-01-01")),x_seq, 
                              y= rep(110, length(x_seq))
)

#reorder factors
d.long$type <- factor(d.long$type)
d.long$type <- fct_relevel(d.long$type, c("worklearn" ,"msc student" ,"phd student","postdoc", "research associate",
                                          "visiting student", "vistiting professor"))



#construct legend labels - factor levels + number of entries
labels <- 
  d.long %>% filter(key=="joined") %>%
  group_by(type)  %>% count() %>% mutate(label= paste0(str_to_title(type)," (",n,")"))  

labels$type %>% factor(labels$type)
labels$type <- fct_reorder(labels$type, levels(d.long$type))


# MAIN PLOT ####

p1 <- ggplot(data=NULL) +
  geom_vline(xintercept = seq(ymd('2005-01-01'),ymd('2025-01-01'), by = 'years'),
             color="grey80")+
  geom_line(data=d.long, aes(y=name, x=value, color=type), lineend = "round", size=3.5)+
  # geom_line(data=d.long, aes(y=name, x=value, color=type), lineend = "round", size=3, alpha=0.5)+
  geom_text(data=d.long.labels,aes(y=name, x=min_date,label=name), hjust=1.1, family="Segoe UI Light", size=3)+
  geom_text(data=timeline_labels, aes(x=x, y=y, label=x_seq), 
            angle=90, vjust=0, size=9, color="grey70", 
            family="Segoe UI Light")+
  geom_line(data=d.long, aes(y=name, x=value, color=type), lineend = "round", size=3.5)+
  
  xlab("")+ylab("")+
  xlim(min(d.long$value-600), max(d.long$value+50))+
  theme_pt()+
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        # axis.text.x = element_text(angle=90, vjust=0, hjust=5), 
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        # axis.ti
        plot.title = element_text(hjust = 0.5, size = 40,  family="Segoe UI Semibold"),
        text = element_text(family="Segoe UI Light"),
        legend.position = c(0.2, 0.2),
        legend.title = element_text(family="Segoe UI Light"),
        legend.title.align = 0,
  )+
  ggtitle("IRSS Timeline") + 
  scale_color_manual(values = timeline_colors, labels=labels$label, name="Legend")+
  labs(caption = paste0("Generated on ",as.character(today())))


### number of members through time ####
seq_of_dates <- seq(min(d1$joined),today(), by = '1 month')

result <- list()
for (i in 1:length(seq_of_dates)) {
  current_date <- seq_of_dates[i]
  
  a <- d1 %>%
    rowwise() %>%
    mutate(test = between(x = current_date, left = joined, right = left)) %>%
    pull(test)
  
  result[[as.character(current_date)]] <- a
}

# do.call(rbind.data.frame, result)
result <- as.data.frame(result)
names(result)

result <- cbind(dplyr::select(d1, name, type), result)
# names(result)[1] <- "name"

result.long <- result %>% gather(key, value, -name, -type)
result.long$key <- gsub(result.long$key, pattern = "X", replacement = "")

result.long$key <- ymd(result.long$key)

ddd <- result.long %>% 
  filter(value==T) %>%
  group_by(key, type) %>% count() 

ddd$type <- as.factor(ddd$type)
ddd$type <-  fct_relevel(ddd$type, c("worklearn","msc student" ,"phd student","postdoc", "research associate",
                                     "visiting student","vistiting professor"   ))

# add zeroes
a <- expand.grid(seq_of_dates, levels(ddd$type))
names(a) <- c("key", "type")

a <- a %>% left_join(ddd)

a$n[is.na(a$n)] <- 0

# plot 2
p2 <- ggplot(a, aes(x=key, y=n, fill=type)) +
  geom_area(position = position_stack()) + 
  scale_fill_manual(values = timeline_colors)+
  theme_pt()+
  xlab("") + ylab("")+
  # expand_limits(y = 0)+
  ggtitle("Number of IRSS lab members through time")+
  theme(legend.position ="none",
        # panel.border = element_rect(color="grey70"),
        panel.border = element_blank(),
        axis.ticks = element_line(color="grey70"),
        plot.title = element_text(hjust = 0.5, size = 10,  family="Segoe UI"),
        text = element_text(family="Segoe UI Light"),
        panel.grid.major.y = element_line(color="grey70")
  ) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

xlimits <- c("01.10.2003", "15.12.2011")
xlimits <-  dmy(xlimits)


### top 10 members ####

top10_names <- d1 %>% mutate(total_time= left-joined) %>% group_by(name) %>%
  summarise(total=sum(total_time)) %>% arrange(desc(total)) %>% 
  top_n(total, n = 10) %>% pull(1)


top10 <- d1 %>% filter(name %in% top10_names) %>%
  mutate(total_time= left-joined) %>% 
  group_by(name, type) %>%
  summarise(total=sum(total_time)) %>% 
  arrange(desc(total))

top10$name <- (fct_reorder(top10$name, .x = top10$total, .fun = sum))

top10$type <- fct_expand(top10$type, c("worklearn","msc student" ,"phd student","postdoc", "research associate",
                                       "visiting student", "vistiting professor"   ))

top10$type <- fct_relevel(top10$type, c("worklearn","msc student" ,"phd student","postdoc", "research associate",
                                        "visiting student", "vistiting professor"     ))

top10_2 <- top10 %>% group_by(name) %>% summarise(total_all = sum(total))

p3 <- ggplot(data=NULL) + 
  geom_col(data=top10, aes(name, as.numeric(total), fill=type), position = position_stack(reverse = T)) + 
  coord_flip()+
  
  scale_fill_manual(values = timeline_colors, drop = FALSE)+
  
  geom_text(data=top10_2, aes(x=name, y=as.numeric(total_all),label=as.numeric(total_all)), 
            hjust=1.1, family="Segoe UI Light", size=4, color="white")+
  
  geom_text(data=top10_2, aes(x=name, y=0,label=name), 
            hjust=0, family="Segoe UI Light", size=4, color="white")+
  
  theme_pt()+
  # theme_void()+
  xlab("") + ylab("")+
  # expand_limits(y = 0, x=0)+
  ggtitle("All time longest serving lab members (days)")+
  theme(legend.position ="none",
        # plot.margin = margin(0, 0, 0, 0),
        # panel.border = element_rect(color="grey70"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10,  family="Segoe UI"),
        text = element_text(family="Segoe UI Light"),
        axis.text = element_blank()
  ) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))


#top 10 CURRENT

current_members <- d1 %>% filter(left == today()) %>% pull(name)

top10_2_names <- d1 %>% 
  mutate(total_time= left-joined) %>% 
  group_by(name) %>%
  summarise(total=sum(total_time)) %>% 
  arrange(desc(total)) %>%
  filter(name %in% current_members) %>%
  top_n(total, n = 10) %>%
  pull(name)

top10_2 <- d1 %>% filter(name %in% top10_2_names) %>%
  mutate(total_time= left-joined) %>% 
  group_by(name, type) %>%
  summarise(total=sum(total_time)) %>% 
  arrange(desc(total))


top10_2$name <- (fct_reorder(top10_2$name, .x = top10_2$total, .fun = sum))

top10_2$type <- fct_expand(top10_2$type, c("worklearn","msc student" ,"phd student","postdoc", "research associate",
                                           "visiting student", "vistiting professor"   ))

top10_2$type <- fct_relevel(top10_2$type, c("worklearn","msc student" ,"phd student","postdoc", "research associate",
                                            "visiting student", "vistiting professor"     ))

top10_2_2 <- top10_2 %>% group_by(name) %>% summarise(total_all = sum(total))

p3_2 <- ggplot(data=NULL) + 
  geom_col(data=top10_2, aes(name, as.numeric(total), fill=type), position = position_stack(reverse = T)) + 
  coord_flip()+
  
  scale_fill_manual(values = timeline_colors, drop = FALSE)+
  
  geom_text(data=top10_2_2, aes(x=name, y=as.numeric(total_all),label=as.numeric(total_all)), 
            hjust=1.1, family="Segoe UI Light", size=4, color="white")+
  
  geom_text(data=top10_2_2, aes(x=name, y=0,label=name), 
            hjust=0, family="Segoe UI Light", size=4, color="white")+
  
  theme_pt()+
  # theme_void()+
  xlab("") + ylab("")+
  # expand_limits(y = 0, x=0)+
  ggtitle("Longest serving current lab members (days)")+
  theme(legend.position ="none",
        # plot.margin = margin(0, 0, 0, 0),
        # panel.border = element_rect(color="grey70"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10,  family="Segoe UI"),
        text = element_text(family="Segoe UI Light"),
        axis.text = element_blank()
  ) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))



# length of stay boxplot

d1 %>% mutate(total_time= left-joined) %>% 
  group_by(type) %>%
  summarise(average_time=mean(total_time))

dd1 <- d1 %>% mutate(total_time= left-joined)
dd1$type <- factor(dd1$type)
# levels(dd1$type)
dd1$type <- fct_relevel(dd1$type, c("worklearn","msc student" ,"phd student","postdoc", "research associate",
                                    "visiting student", "vistiting professor"     ))


p4 <- dd1 %>%
  ggplot(aes(x=type, y=as.numeric(total_time)/365, fill=type)) + 
  geom_boxplot(outlier.colour = NA, alpha=0.9)+
  geom_jitter(aes(color=type), shape=21, color="black")+
  scale_fill_manual(values = timeline_colors, drop = FALSE)+
  scale_color_manual(values = timeline_colors, drop = FALSE)+
  theme_pt() +
  xlab("") + ylab("")+
  ggtitle("Length of stay (years)")+
  theme(legend.position ="none",
        # panel.border = element_rect(color="grey70"),
        panel.border = element_blank(),
        axis.ticks = element_line(color="grey70"),
        plot.title = element_text(hjust = 0.5, size = 10,  family="Segoe UI"),
        text = element_text(family="Segoe UI Light"),
        panel.grid.major.y = element_line(color="grey70"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
  ) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))



#lab logo
# img <- readPNG("\\\\FRST-IRSStor\\Root\\ByUser\\_Logos\\IRSS\\Transparent Background\\IRSS_Logo.png")
# ggg <- rasterGrob(img, interpolate=TRUE)


### totals ####
# all
total_all <- length(unique(d.long$name))

#current
total_current <- d1 %>% filter(left == today()) %>% count() %>% pull(1)

## how many graduated 

# exclude people who are not msc or phd students
g1 <- d1 %>% 
  filter(type %in% c("msc student", "phd student"))


# exclude people who are still in the lab
g1_active <- g1 %>% filter(left == today())

g1 %<>% filter(!(name %in% g1_active$name))

# calculate total time and remove people who were shorter than X

g1 <-  g1 %>%
  mutate(total_time = as.numeric(left - joined)) %>%
  filter(total_time > 100)


#phd first - if somebody did msc and phd then only phd counts, but 3 people have done two degrees
g_phd <- g1 %>% filter(type=="phd student")

# msc - remove people on the phd list except: Colin, Greg, Rory
g_msc <- g1 %>% filter(type == "msc student") %>%
  filter(!(name %in% g_phd$name) | name %in% c("Colin Ferster", "Gregory Rickbeil", "Rory Tooke"))
# g1 %>% filter(type == "msc student" & name %in% g_phd$name)

g <- rbind(g_msc, g_phd)

g %>% group_by(type) %>% count()

# how many postdocs?
total_postdocs <- d1 %>% filter(type == "postdoc") %>% count() %>% pull(1)

#visitors
total_visitors <- d1 %>% filter(type %in% c("visiting student","vistiting professor")) %>% count() %>% pull(1)

# Total number of:
# Lab members: 118
# MSc degrees:
# PhD degrees:
# Postdocs:
# Visitors: 


info_text <- paste0("Total number of: \n",
                    "Lab members (all time) - ",total_all,"\n",
                    "Lab members (current) - ",total_current,"\n",
                    "MSc degrees - ",nrow(g_msc),"\n",
                    "PhD degrees - ",nrow(g_phd),"\n",
                    "Postdocs - ",total_postdocs,"\n",
                    "Visitors - ",total_visitors)

info_text_grob <- grobTree(rectGrob(x=0.30,  y=0.47,
                                    width=0.2, height=0.12,
                                    hjust=0,
                                    gp=gpar(fill="white", col="white")),
                           textGrob(info_text, 
                                    x=0.48,  y=0.47,
                                    hjust=1,
                                    
                                    gp=gpar(col="black", fontsize=10, 
                                            fontfamily="Segoe UI Light")))

n_unique <-  length(unique(d.long$name))

# ppp <- p1 + theme(legend.position = c(0.2, 0.47))+
#   annotation_custom(ggplotGrob(p2), xmin = xlimits[1], xmax=xlimits[2], ymin = 35, ymax = 55)+
#   annotation_custom(ggplotGrob(p4), xmin = xlimits[1], xmax=xlimits[2], ymin = 17, ymax = 37)+
#   annotation_custom(ggplotGrob(p3), xmin = xlimits[1], xmax=xlimits[2], ymin = -5, ymax = 20)+
#   # annotation_custom(ggg, xmin = ymd("2018-01-01"), xmax=ymd("2021-01-01"), ymin = n_unique-10, ymax = n_unique)+
#   annotation_custom(info_text_grob)
# ggsave(plot = ppp, filename = "C:\\Dropbox\\_coding/IRSS_timeline_new/IRSS_timeline.png", width = 11, height = 17, dpi=300)

# ggsave(filename = "C:\\Dropbox\\functions\\R\\irss_timeline\\IRSS_timeline.pdf", device = cairo_pdf,
#        width = 11, height = 17, dpi=300)


df_legend <- data.frame(col=timeline_colors,
                        type=c("worklearn","msc student","phd student","postdoc","research associate","visiting student","vistiting profesor"),
                        label=labels$label,
                        x=1,
                        y=rev(1:length(timeline_colors)))

df_legend$type <- factor(df_legend$type, levels=c("worklearn","msc student" ,"phd student","postdoc", "research associate",  "visiting student", "vistiting profesor"), ordered = T)

legend_plot <- ggplot(df_legend, aes(x, y, fill=type)) +
  geom_point(shape=22, size=5)+
  geom_text(aes(x=1.1, label=label), hjust=0, family="Segoe UI Light")+
  scale_fill_manual(values = timeline_colors, drop = FALSE)+
  theme_void()+
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "white", color="white",size = 5))+
  xlim(1, 2)+
  expand_limits(y = 0)
legend_plot




#combine plots on the left into one
subplots <- p2 + p4+ p3 + p3_2 + plot_layout(ncol = 1)

wrap_elements(panel = textGrob(info_text)) + subplots

ppp <- 
  p1 + #theme(legend.position = c(0.9, 0.95)) + 
  inset_element(
    subplots, 
    left = 0, 
    bottom = 0, 
    right = 0.45, 
    top = 0.6
  )  + inset_element(
    legend_plot,
    left = 0.75, 
    bottom = 0.9, 
    right = 0.95, 
    top = 1
  )+ inset_element(
    wrap_elements(panel = textGrob(info_text, gp=gpar(fontsize=10, 
                                                      fontfamily="Segoe UI Light"))),
    left = 0.75, 
    bottom = 0.8, 
    right = 0.95, 
    top = 0.9
  )


ggsave(plot = ppp, filename = file.path(output_dir, "IRSS_timeline.png"), width = 16, height = 24, dpi=300)
ggsave(plot = ppp, filename = file.path(output_dir, "IRSS_timeline.pdf"), device = cairo_pdf,
       width = 16, height = 24, dpi=300)




