library(tidyverse)
library(ggthemes)

setwd("/Users/jakesnyder/Columbia/Financial Literacy")
theme_set(theme_tufte())
theme_update(axis.text = element_text(size=14),
             axis.title = element_text(size=16),
             legend.text = element_text(size=12),
             legend.title = element_text(size=14))

#### Budgeting ####
bud_df = data.frame(month = 1:12,
                tuition = c(rep(-5574,4), rep(0,4), rep(-5574,4)),
                rent = c(rep(-1200,12)),
                food = c(rep(-650,12)),
                other = c(rep(-600,12)),
                scholarship = c(rep(5000,4), rep(0,4), rep(5000,4)),
                research_salary = c(rep(960,4), rep(0,4), rep(960,4)),
                internship_salary = c(rep(0,4), rep(2000,4), rep(0,4)))

bud_data = df %>% gather(category, value, -month) %>%
  mutate(type = ifelse(category %in%
                         c('scholarship', 'research_salary', 'internship_salary'),
                           'income', 'expense'))

# Budget as annual sum
bud_data %>% group_by(category,type) %>%
  summarise(value = abs(sum(value))) %>%
  ggplot(., aes(reorder(category,-value), value, fill=type)) +
  geom_bar(stat='identity') +
  scale_x_discrete(name = 'category') +
  scale_fill_discrete(breaks = c('income', 'expense'))
ggsave('annual_budget.jpeg')

# Budget as cumsum
bud_data %>% group_by(month, type) %>%
  summarise(value = sum(value)) %>%
  spread(type, value) %>%
  mutate(net = income + expense) %>%
  gather(type, value, -month) %>%
  arrange(type, month) %>%
  group_by(type) %>%
  mutate(value = cumsum(value)) %>%
  ggplot(., aes(month, value, colour=type)) +
  geom_line() +
  scale_x_continuous(breaks = c(1:12), limits = c(1,12)) +
  scale_y_continuous(name = 'cumulative value', breaks = seq(-80000,80000,by=20000),
                     limits = c(-80000, 80000)) +
  scale_color_discrete(breaks = c('income', 'expense', 'net'))
ggsave('budget_cumsum.jpeg')

#### Savings and Interest ####
compounding_function <- function(principal, payment, months, annual_interest_rate, annual_compound){
  balance = rep(principal, months)
  interest = rep(0, months)
  for (i in 2:(months+1)){
    interest[i] = balance[i-1]*(annual_interest_rate/annual_compound)
    balance[i] = balance[i-1] + interest[i] - payment
    
  }
   
  balance = balance[-1]
  interest = interest[-1]
  
  df = data.frame(month = 1:months,
                  balance = balance,
                  interest = interest)
  
  df = df %>%
    mutate(cumulative_interest = cumsum(interest))
  return(df)
}
option1 <- compounding_function(principal=50000,annual_interest_rate=.06,payment=500,months=60,annual_compound=12) %>%
  mutate(payment_type = '$500 per month')
option2 <- compounding_function(principal=50000,annual_interest_rate=.06,payment=900,months=60,annual_compound=12) %>%
  mutate(payment_type = '$900 per month')

# Loan balance
rbind(option1, option2) %>%
  ggplot(., aes(month, balance, colour=payment_type)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,60,12))
ggsave('loan_balance.jpeg')

# Loan accumulated interest
rbind(option1, option2) %>%
  ggplot(., aes(month, cumulative_interest, colour=payment_type)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,60,12)) +
  scale_y_continuous(name = 'cumulative interest')
ggsave('loan_interest.jpeg')


# stocks
initial_deposit = 10000
stocks = data.frame(year = 1:20)
stocks = stocks %>%
  mutate(invest_early = initial_deposit*1.08^year,
         high_return = ifelse(year<6, 10000, initial_deposit*1.1^(year-5)))

stocks %>%
  gather(type, value, -year) %>%
  ggplot(., aes(year, value, colour=type)) +
  geom_line()


# Simple investment growth
deposit_rate = 2000
retirement = data.frame(deposit_early = c(0,rep(deposit_rate,40)),
                        savings_early = c(rep(0,40+1)),
                        deposit_late = c(rep(0,20+1),rep(deposit_rate*4,20)),
                        savings_late = c(rep(0,40+1)),
                        year = c(0:40))
for (i in 2:41){
  retirement$savings_early[i] = (retirement$deposit_early[i]+retirement$savings_early[i-1])*(1+.08)
  retirement$savings_late[i] = (retirement$deposit_late[i]+retirement$savings_late[i-1])*(1+.08)
}
retirement = retirement %>%
  mutate(deposit_early = cumsum(deposit_early),
         deposit_late = cumsum(deposit_late)) %>%
  gather(type, dollars, -year) %>%
  filter(year != 0) %>%
  separate(type, c('key','deposit_start'), sep='_')

retirement %>%
  ggplot(., aes(year, dollars, colour=deposit_start)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,40,by=10)) +
  facet_wrap(~key) +
  theme(strip.text.x = element_text(size = 16))
ggsave('retirement.jpeg')


# Credit score factors
score <- data.frame(factor=c('Payment History', 'Amount owed', 'Length of credit history',
                      'New credit', 'Types of credit used'),
                    percent=c(.35, .3, .15, .1, .1))
ggplot(score, aes(x=reorder(factor, -percent), y=percent)) +
  geom_bar(stat='identity', fill='light blue') +
  xlab('factor')
ggsave('creditfactor.jpeg')




# Complicated investment growth - did not use
deposit_rate = 200
data = data.frame(deposit = c(0,rep(deposit_rate,12*15)),
                  savings = c(rep(0,12*15+1)),
                  gov_bond = c(rep(0,12*15+1)),
                  market = c(rep(0,12*15+1)),
                  deposit_late = c(rep(0,12*5+1),rep(deposit_rate*1.5,12*10)),
                  savings_late = c(rep(0,12*15+1)),
                  gov_bond_late = c(rep(0,12*15+1)),
                  market_late = c(rep(0,12*15+1)),
                  month = c(0:180))
for (i in 2:181){
  # Savings account: 1.5% interest rate
  data$savings[i] = (data$deposit[i]+data$savings[i-1])*(1+.015/12)
  data$savings_late[i] = (data$deposit_late[i]+data$savings_late[i-1])*(1+.015/12)
  
  # Government bond: 1.873% coupon rate for Treasury note (avg month of Jan 2018)
  data$gov_bond[i] = (data$deposit[i]+data$gov_bond[i-1])*(1+.02/12)
  data$gov_bond_late[i] = (data$deposit_late[i]+data$gov_bond_late[i-1])*(1+.02/12)
  
  # Market: 
  data$market[i] = (data$deposit[i]+data$market[i-1])*(1+.1/12)
  data$market_late[i] = (data$deposit_late[i]+data$market_late[i-1])*(1+.1/12)
}

data = data %>%
  gather(type, value, -month) %>%
  filter(month != 0 & !type %in% c('deposit','deposit_late')) %>%
  mutate(invest = ifelse(grepl('late',type),'late','early'),
         type = gsub('_late','', type))

data %>% #filter(invest == 'early') %>%
  ggplot(., aes(month, value, colour=type)) +
  geom_line()