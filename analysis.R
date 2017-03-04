library(ggplot2)
library(dplyr)
library(scales)

# Загружаем данные...
setwd("C:\\Users\\Alexey\\Dropbox\\IRL\\Открытые данные\\Арбитраж\\Мастер-класс")
commercial_courts_cases <- read.csv(file = "IRL_commercial_courts_decisions_2009_RUS.csv", stringsAsFactors = FALSE)
attach(commercial_courts_cases)



# Сумма иска

### Распределение сумм исков


ggplot(commercial_courts_cases, aes(x=sum_dec)) + 
  geom_line(stat="density",adjust = 0.01)+
  #scale_x_continuous(trans = 'log10',
  #                    breaks = trans_breaks('log10', function(x) 10^x),
  #                    labels = comma)+
  xlab("Сумма заявленного иска, в рублях (логарифмированная шкала)")+
  ylab("Доля исков с указанной суммой")+
  ggtitle("Распределение арбитражных дел по сумме заявленного иска")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

### Распределение сумм исков в зависимости от пары истец-ответчик

ggplot(commercial_courts_cases, aes(x=sum_dec, color = pair)) + 
  geom_line(stat="density",adjust = 0.6, aes(color = pair))+
  scale_x_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = comma)+
  xlab("Сумма заявленного иска, в рублях (логарифмированная шкала)")+
  ylab("Доля исков с указанной суммой")+
  ggtitle("Распределение арбитражных дел по сумме заявленного иска в зависимости от пары истец-ответчик")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        legend.position="bottom",
        legend.direction ="vertical",
        plot.title = element_text(hjust = 0.5))


### Распределение сумм для государства VS предприниматель

ggplot(subset(commercial_courts_cases,pair=="Гос. орган vs. Предприниматель"), aes(x=sum_dec)) + 
  geom_line(stat="density",adjust = 0.2)+
  scale_x_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = comma)+
  xlab("Сумма заявленного иска, в рублях (логарифмированная шкала)")+
  ylab("Доля исков с указанной суммой")+
  ggtitle("Распределение арбитражных дел по сумме заявленного иска")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

### Какие суммы исков самые частые в государственных исках
summary(subset(commercial_courts_cases,pair=="Гос. орган vs. Предприниматель")$sum_dec)

head(sort(table(subset(commercial_courts_cases,pair=="Гос. орган vs. Предприниматель")$sum_dec),decreasing = TRUE))


### Медианы суммы исков по заявителям

commercial_courts_cases %>%  
  filter(!is.na(sum_dec)) %>%
  group_by(pair) %>%
  summarise("Медиана суммы иска"=median(sum_dec),Частота = n()) %>% 
  mutate (Доля = Частота / sum(Частота)) %>% 
  arrange(desc(Доля))

commercial_courts_cases %>%  
  filter(!is.na(sum_dec)) %>%
  group_by(dec_type) %>%
  summarise("Медиана суммы иска"=median(sum_dec),Частота = n()) %>% 
  mutate (Доля = Частота / sum(Частота)) %>% 
  arrange(desc(Доля))


## Группы сумм
commercial_courts_cases$sum_grouped <- cut(commercial_courts_cases$sum_dec, 
                       breaks = c(-Inf, 1000, 10000, 50000, 100000, 500000, Inf), 
                       labels = c("<1 т.р.", "1-10 т.р.", "10-50 т.р.", "50-100 т.р.", "100-500 т.р.", ">500т.р."), 
                       right = FALSE)

commercial_courts_cases %>%  
  filter(!is.na(sum_dec)) %>%
  group_by(sum_grouped) %>%
  summarise("Медиана суммы иска"=median(sum_dec),Частота = n()) %>% 
  mutate (Доля = Частота / sum(Частота))

table(commercial_courts_cases$pair, commercial_courts_cases$sum_grouped)

# Длительность рассмотрения

### Вычисляем время рассмотрения

commercial_courts_cases$time_diff_bill_to_court <- as.numeric(as.Date(as.character(date_ses), format="%Y-%m-%d")-
  as.Date(as.character(date_bil), format="%Y-%m-%d"))

commercial_courts_cases$time_diff_bill_to_decision <- as.numeric(as.Date(as.character(date_des), format="%Y-%m-%d")-
  as.Date(as.character(date_bil), format="%Y-%m-%d"))

commercial_courts_cases$time_diff_court_to_decision <- as.numeric(as.Date(as.character(date_des), format="%Y-%m-%d")-
  as.Date(as.character(date_ses), format="%Y-%m-%d"))

### Связь между рассмотрением иска и разбирательством

cor(commercial_courts_cases$time_diff_bill_to_court,
    commercial_courts_cases$time_diff_court_to_decision, 
    method = "spearman", use = "pairwise.complete.obs")

ggplot(commercial_courts_cases,aes(x=time_diff_bill_to_court, y = time_diff_court_to_decision)) + 
  geom_point(alpha=0.1)+
  xlab("От подачи заявления до первого суда, в днях")+
  ylab("От первого суда до решения суда, в днях")+
  ggtitle("Время рассмотрения VS время разбирательства")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

### Отсечка по 200 дням

ggplot(subset(commercial_courts_cases, time_diff_court_to_decision<200 & time_diff_bill_to_court < 200),aes(x=time_diff_bill_to_court, y = time_diff_court_to_decision)) + 
  geom_point(alpha=0.1)+
  xlab("От подачи заявления до первого суда, в днях")+
  ylab("От первого суда до решения суда, в днях")+
  ggtitle("Время рассмотрения VS время разбирательства")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))


## Сумма и время

ggplot(commercial_courts_cases,aes(x=sum_dec, y = time_diff_bill_to_decision)) + 
  geom_point(alpha=0.05)+
  scale_x_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = comma)+
  xlab("Сумма заявленного иска, в рублях (логарифмированная шкала)")+
  ylab("Время от принятия иска до решения суда, в днях")+
  ggtitle("Сумма иска и время рассмотрения")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

# Время по регионам

## Где рассматривают дольше всего?
commercial_courts_cases %>%  
  group_by(regi) %>%
  summarise("Иск->Суд"=median(time_diff_bill_to_court, na.rm = TRUE),
            "Суд->Решение"=median(time_diff_court_to_decision, na.rm = TRUE),
            "Иск->Решение"=median(time_diff_bill_to_decision, na.rm = TRUE),
            Частота = n()) %>% 
  mutate (Доля = Частота / sum(Частота)) %>% 
  arrange(desc(Доля))

## Где рассматривают быстрее всего?
commercial_courts_cases %>%  
  group_by(regi) %>%
  summarise("Иск->Суд"=median(time_diff_bill_to_court, na.rm = TRUE),
            "Суд->Решение"=median(time_diff_court_to_decision, na.rm = TRUE),
            "Иск_Решение"=median(time_diff_bill_to_decision, na.rm = TRUE),
            Частота = n()) %>% 
  mutate (Доля = Частота / sum(Частота)) %>% 
  arrange(Иск_Решение)

# Время и пары заявитель-истец

commercial_courts_cases %>%  
  group_by(pair) %>%
  summarise("Иск->Суд"=median(time_diff_bill_to_court, na.rm = TRUE),
            "Суд->Решение"=median(time_diff_court_to_decision, na.rm = TRUE),
            "Иск_Решение"=median(time_diff_bill_to_decision, na.rm = TRUE),
            Частота = n()) %>% 
  mutate (Доля = Частота / sum(Частота)) %>% 
  arrange(desc(Иск_Решение))

# Время и заявители

commercial_courts_cases %>%  
  group_by(dec_type) %>%
  summarise("Иск->Суд"=median(time_diff_bill_to_court, na.rm = TRUE),
            "Суд->Решение"=median(time_diff_court_to_decision, na.rm = TRUE),
            "ИскРешение"=median(time_diff_bill_to_decision, na.rm = TRUE),
            Частота = n()) %>% 
  mutate (Доля = Частота / sum(Частота)) %>% 
  arrange(desc(ИскРешение))

commercial_courts_cases %>%  
  group_by(dec_type) %>%
  summarise("Иск->Суд"=median(time_diff_bill_to_court, na.rm = TRUE),
            "Суд->Решение"=median(time_diff_court_to_decision, na.rm = TRUE),
            "ИскРешение"=median(time_diff_bill_to_decision, na.rm = TRUE),
            Частота = n()) %>% 
  mutate (Доля = Частота / sum(Частота)) %>% 
  arrange(ИскРешение)

