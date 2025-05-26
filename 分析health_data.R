setwd("D:/Joy/爸爸的健康資訊/健康追蹤數據分析")

library(readr)
library(data.table)
library(ggplot2)
library(dplyr)

health_data <- read_csv("health_data.csv", show_col_types=F)
View(health_data)
health_data <- data.table(health_data)
str(health_data)
health_data[,c('date','status','label'):=list(
  as.Date(date),
  factor(status, levels=c('breakfast','lunch','dinner','sleep')),
  factor(label, levels=c('before', 'after'))
)]
long_data <- melt(health_data, id.vars=1:4, variable.name='item', value.name='obs_value') %>% setDT
setorder(long_data, date, time)
label_setting <- c("SBP"="收縮壓", "DBP"="舒張壓", "pulse"="心跳", "Glucose"="血糖")
# summary_data <- long_data %>%
#   group_by(item) %>%
#   summarize(
#     q1 = quantile(obs_value, probs=0.25, na.rm = TRUE),
#     median_value = median(obs_value, na.rm = TRUE),
#     mean_value = mean(obs_value, na.rm=T),
#     q3 = quantile(obs_value, probs=0.75, na.rm = TRUE))
## 只顯示所有日期的「日期：1」
date1_breaks <- long_data %>%
  filter(time == 1) %>%
  pull(date) %>%
  unique() %>%
  sort() %>%
  paste0("：1") 
## (全部的資料)
ggplot() +
  geom_point(data=long_data, aes(
    x = paste0(date,'：',time), y = obs_value, 
    color=as.character(status), shape=label), size=2) +
  geom_line(data = filter(long_data, !(item %in% 'Glucose')), aes(
      x = paste0(date,'：',time), y = obs_value), color='gray', size=1, group=1) +
  geom_vline(xintercept = "2025-04-29：1", size=1, lty='dashed') +
  # 新增趨勢線
  # 趨勢線（所有 item 都畫一條線，每個 facet 內一條）
  geom_smooth(
    data   = long_data,
    aes(
      x     = paste0(date, "：", time),
      y     = obs_value,
      group = 1
    ),
    method = "loess",    # 線性趨勢；改成 "loess" 即可畫局部平滑曲線
    se     = FALSE,    # 不畫信賴區間
    color = 'black',
    size = 0.8
  ) +
  facet_wrap(.~item, scales = 'free_y',
             labeller = as_labeller(label_setting),
             nrow=4) +
  theme_bw() +
  scale_x_discrete(
    breaks = date1_breaks,
    labels = gsub("2025-","",gsub("：1", "", date1_breaks))
  ) +
  # geom_hline(data = summary_data, 
  #            aes(yintercept = median_value), 
  #            color = "black", linetype = "solid", size=0.8) +  # 加中位數線（紅色虛線）
  # geom_hline(data = summary_data, 
  #            aes(yintercept = q1), 
  #            color = "black", linetype = "dashed", size=0.8) +
  # geom_hline(data = summary_data, 
  #            aes(yintercept = q3), 
  #            color = "black", linetype = "dashed", size=0.8) +
  theme(strip.text = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size=12, face='bold'),
        legend.text = element_text(size=12, face='bold'),
        axis.title = element_text(size=12, face='bold'),
        plot.subtitle = element_text(size=12, face='bold')) +
  labs(x='觀察日與時間', y='測量值', color='時間點', shape='狀態',
       subtitle='4/29開始服用調整過的用藥') +
  scale_color_manual(values = c(
    "breakfast" = "red",
    "lunch" = "orange",
    "dinner" = "royalblue",
    "sleep" = "green"
  ),
  breaks = c('breakfast','lunch','dinner','sleep'),
  labels = c('早餐','午餐','晚餐','睡前'))  # 控制圖例順序

## (排除飯後)
ggplot() +
  geom_point(data=long_data[label=='before'], aes(
    x = paste0(date,'：',time), y = obs_value, 
    color=as.character(status)), size=3) +
  geom_line(data = long_data[label=='before'], aes(
    x = paste0(date,'：',time), y = obs_value), color='gray', size=1, group=1) +
  geom_vline(xintercept = "2025-04-29：1", size=1, lty='dashed') +
  # 新增趨勢線
  # 趨勢線（所有 item 都畫一條線，每個 facet 內一條）
  geom_smooth(
    data   = long_data,
    aes(
      x     = paste0(date, "：", time),
      y     = obs_value,
      group = 1
    ),
    method = "loess",    # 線性趨勢；改成 "loess" 即可畫局部平滑曲線
    se     = FALSE,    # 不畫信賴區間
    color = 'black',
    size = 0.8
  ) +
  facet_wrap(.~item, scales = 'free_y',
             labeller = as_labeller(label_setting),
             nrow=4) +
  theme_bw() +
  scale_x_discrete(
    breaks = date1_breaks,
    labels = gsub("2025-","",gsub("：1", "", date1_breaks))
  ) +
  theme(strip.text = element_text(size = 12, face = 'bold'),
        legend.title = element_text(size=12, face='bold'),
        legend.text = element_text(size=12, face='bold'),
        axis.title = element_text(size=12, face='bold'),
        plot.subtitle = element_text(size=12, face='bold')) +
  labs(x='觀察日與時間', y='測量值', color='時間點', 
       subtitle='4/29開始服用調整過的用藥') +
  scale_color_manual(values = c(
    "breakfast" = "red",
    "lunch" = "orange",
    "dinner" = "royalblue",
    "sleep" = "green"
  ),
  breaks = c('breakfast','lunch','dinner','sleep'),
  labels = c('早餐前','午餐前','晚餐前','睡前'))  # 控制圖例順序

long_data[,type:=fifelse(date<as.Date('2025-04-29'),'用藥調整前','用藥調整後')]
ggplot(long_data[label=='before'], aes(x=obs_value, color=type)) +
  geom_density(size=1) +
  facet_wrap(.~item, scales = 'free',
             labeller = as_labeller(label_setting)) +
  theme_bw() +
  labs(x='測量值',  title='調整用藥前後，各種測量值的分布',
       subtitle='黑色是調整前；紅色是調整後', y='') +
  scale_color_manual(values=c('black','orangered')) +
  theme(strip.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size=12, face='bold'),
        plot.subtitle = element_text(size=12, face='bold'),
        plot.title = element_text(size=13, face='bold'),
        legend.position = 'none')

long_data %>%
  group_by(item, label) %>%
  summarize(median_value = median(obs_value, na.rm = TRUE),
            mean_value = mean(obs_value, na.rm=T))

    
