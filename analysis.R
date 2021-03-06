library(ggplot2)
library(dplyr)
library(scales)

# ��������� ������...
commercial_courts_cases <- read.csv("https://github.com/alexeyknorre/Rbitrazh/raw/master/IRL_commcourts_decisions_cp1251.csv",
          stringsAsFactors = FALSE)
# ����� ���������? �������� ���:
# commercial_courts_cases <- read.csv("https://github.com/alexeyknorre/Rbitrazh/raw/master/IRL_commcourts_decisions.csv",
          stringsAsFactors = FALSE)
attach(commercial_courts_cases)



# ����� ����

### ������������� ���� �����


ggplot(commercial_courts_cases, aes(x=sum_dec)) + 
  geom_line(stat="density",adjust = 0.01)+
  #scale_x_continuous(trans = 'log10',
  #                    breaks = trans_breaks('log10', function(x) 10^x),
  #                    labels = comma)+
  xlab("����� ����������� ����, � ������ (����������������� �����)")+
  ylab("���� ����� � ��������� ������")+
  ggtitle("������������� ����������� ��� �� ����� ����������� ����")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

### ������������� ���� ����� � ����������� �� ���� �����-��������

ggplot(commercial_courts_cases, aes(x=sum_dec, color = pair)) + 
  geom_line(stat="density",adjust = 0.6, aes(color = pair))+
  scale_x_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = comma)+
  xlab("����� ����������� ����, � ������ (����������������� �����)")+
  ylab("���� ����� � ��������� ������")+
  ggtitle("������������� ����������� ��� �� ����� ����������� ���� � ����������� �� ���� �����-��������")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        legend.position="bottom",
        legend.direction ="vertical",
        plot.title = element_text(hjust = 0.5))


### ������������� ���� ��� ����������� VS ���������������

ggplot(subset(commercial_courts_cases,pair=="���. ����� vs. ���������������"), aes(x=sum_dec)) + 
  geom_line(stat="density",adjust = 0.2)+
  scale_x_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = comma)+
  xlab("����� ����������� ����, � ������ (����������������� �����)")+
  ylab("���� ����� � ��������� ������")+
  ggtitle("������������� ����������� ��� �� ����� ����������� ����")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

### ����� ����� ����� ����� ������ � ��������������� �����
summary(subset(commercial_courts_cases,pair=="���. ����� vs. ���������������")$sum_dec)

head(sort(table(subset(commercial_courts_cases,pair=="���. ����� vs. ���������������")$sum_dec),decreasing = TRUE))


### ������� ����� ����� �� ����������

commercial_courts_cases %>%  
  filter(!is.na(sum_dec)) %>%
  group_by(pair) %>%
  summarise("������� ����� ����"=median(sum_dec),������� = n()) %>% 
  mutate (���� = ������� / sum(�������)) %>% 
  arrange(desc(����))

commercial_courts_cases %>%  
  filter(!is.na(sum_dec)) %>%
  group_by(dec_type) %>%
  summarise("������� ����� ����"=median(sum_dec),������� = n()) %>% 
  mutate (���� = ������� / sum(�������)) %>% 
  arrange(desc(����))


## ������ ����
commercial_courts_cases$sum_grouped <- cut(commercial_courts_cases$sum_dec, 
                       breaks = c(-Inf, 1000, 10000, 50000, 100000, 500000, Inf), 
                       labels = c("<1 �.�.", "1-10 �.�.", "10-50 �.�.", "50-100 �.�.", "100-500 �.�.", ">500�.�."), 
                       right = FALSE)

commercial_courts_cases %>%  
  filter(!is.na(sum_dec)) %>%
  group_by(sum_grouped) %>%
  summarise("������� ����� ����"=median(sum_dec),������� = n()) %>% 
  mutate (���� = ������� / sum(�������))

table(commercial_courts_cases$pair, commercial_courts_cases$sum_grouped)

# ������������ ������������

### ��������� ����� ������������

commercial_courts_cases$time_diff_bill_to_court <- as.numeric(as.Date(as.character(date_ses), format="%Y-%m-%d")-
  as.Date(as.character(date_bil), format="%Y-%m-%d"))

commercial_courts_cases$time_diff_bill_to_decision <- as.numeric(as.Date(as.character(date_des), format="%Y-%m-%d")-
  as.Date(as.character(date_bil), format="%Y-%m-%d"))

commercial_courts_cases$time_diff_court_to_decision <- as.numeric(as.Date(as.character(date_des), format="%Y-%m-%d")-
  as.Date(as.character(date_ses), format="%Y-%m-%d"))

### ����� ����� ������������� ���� � ����������������

cor(commercial_courts_cases$time_diff_bill_to_court,
    commercial_courts_cases$time_diff_court_to_decision, 
    method = "spearman", use = "pairwise.complete.obs")

ggplot(commercial_courts_cases,aes(x=time_diff_bill_to_court, y = time_diff_court_to_decision)) + 
  geom_point(alpha=0.1)+
  xlab("�� ������ ��������� �� ������� ����, � ����")+
  ylab("�� ������� ���� �� ������� ����, � ����")+
  ggtitle("����� ������������ VS ����� ���������������")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

### ������� �� 200 ����

ggplot(subset(commercial_courts_cases, time_diff_court_to_decision<200 & time_diff_bill_to_court < 200),aes(x=time_diff_bill_to_court, y = time_diff_court_to_decision)) + 
  geom_point(alpha=0.1)+
  xlab("�� ������ ��������� �� ������� ����, � ����")+
  ylab("�� ������� ���� �� ������� ����, � ����")+
  ggtitle("����� ������������ VS ����� ���������������")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))


## ����� � �����

ggplot(commercial_courts_cases,aes(x=sum_dec, y = time_diff_bill_to_decision)) + 
  geom_point(alpha=0.05)+
  scale_x_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = comma)+
  xlab("����� ����������� ����, � ������ (����������������� �����)")+
  ylab("����� �� �������� ���� �� ������� ����, � ����")+
  ggtitle("����� ���� � ����� ������������")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

# ����� �� ��������

## ��� ������������� ������ �����?
commercial_courts_cases %>%  
  group_by(regi) %>%
  summarise("���->���"=median(time_diff_bill_to_court, na.rm = TRUE),
            "���->�������"=median(time_diff_court_to_decision, na.rm = TRUE),
            "���->�������"=median(time_diff_bill_to_decision, na.rm = TRUE),
            ������� = n()) %>% 
  mutate (���� = ������� / sum(�������)) %>% 
  arrange(desc(����))

## ��� ������������� ������� �����?
commercial_courts_cases %>%  
  group_by(regi) %>%
  summarise("���->���"=median(time_diff_bill_to_court, na.rm = TRUE),
            "���->�������"=median(time_diff_court_to_decision, na.rm = TRUE),
            "���_�������"=median(time_diff_bill_to_decision, na.rm = TRUE),
            ������� = n()) %>% 
  mutate (���� = ������� / sum(�������)) %>% 
  arrange(���_�������)

# ����� � ���� ���������-�����

commercial_courts_cases %>%  
  group_by(pair) %>%
  summarise("���->���"=median(time_diff_bill_to_court, na.rm = TRUE),
            "���->�������"=median(time_diff_court_to_decision, na.rm = TRUE),
            "���_�������"=median(time_diff_bill_to_decision, na.rm = TRUE),
            ������� = n()) %>% 
  mutate (���� = ������� / sum(�������)) %>% 
  arrange(desc(���_�������))

# ����� � ���������

commercial_courts_cases %>%  
  group_by(dec_type) %>%
  summarise("���->���"=median(time_diff_bill_to_court, na.rm = TRUE),
            "���->�������"=median(time_diff_court_to_decision, na.rm = TRUE),
            "����������"=median(time_diff_bill_to_decision, na.rm = TRUE),
            ������� = n()) %>% 
  mutate (���� = ������� / sum(�������)) %>% 
  arrange(desc(����������))

commercial_courts_cases %>%  
  group_by(dec_type) %>%
  summarise("���->���"=median(time_diff_bill_to_court, na.rm = TRUE),
            "���->�������"=median(time_diff_court_to_decision, na.rm = TRUE),
            "����������"=median(time_diff_bill_to_decision, na.rm = TRUE),
            ������� = n()) %>% 
  mutate (���� = ������� / sum(�������)) %>% 
  arrange(����������)

