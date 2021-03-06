library(googlesheets4)
library(tidyverse)
library(gganimate)
library(MASS)
fotball<- sheets_read("1CAmqpuF6ViHyYRhVZjbIuxZ4uXg4xi53_xm1Yz0KuoA", col_names = FALSE)
fotball$K<-1
fotball_lang <- fotball %>%
  pivot_longer(-K, names_to = "test", values_to = "antall") %>% 
  mutate(test_num = as.numeric(str_remove(test, '...'))) %>% 
  mutate(antall_2 = antall - 2) %>% 
  group_by(test_num) %>% 
  mutate(n = cumsum(K)) %>% 
  mutate(person = case_when(test_num %in% c(6, 9, 14) ~ "Olaf",
                        TRUE ~ "Mari")) %>% 
  filter(person == "Mari") %>% 
  ungroup() %>% 
  drop_na()

tabell_data <- fotball_lang %>% 
  group_by(person, antall) %>% 
  summarise(n = n())


ggplot(tabell_data, aes(antall, n, fill = person)) +
  geom_bar(stat = "identity", position = 'dodge')

model.nb <- glm.nb(antall_2 ~ K, data = fotball_lang)
pred.m.nb <- predprob(model.nb) %>% colMeans

m1 <- glm(antall_2 ~ factor(test) + n , data = fotball_lang, family = "poisson")
m2 <- glm.nb(antall_2 ~ factor(test) + n, data = fotball_lang)

pred.m2 <- predprob(m2) %>% colMeans
pred.m1 <- predprob(m1) %>% colMeans


probtable <- as.data.frame((dnbinom(x, size = m2$theta, mu = exp(m2$coefficients[1]))) * 100) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname) + 1)


df <- data.frame(x = 0:8, NegBin = pred.m2)

obs <- table(fotball_lang$antall_2) %>% prop.table() %>% data.frame #Observed
names(obs) <- c("x", 'Observed')

comb <- merge(obs, df, by = 'x', all = T)
comb[is.na(comb)] <- 0

mm <- pivot_longer(comb, -x, names_to = 'Model', values_to = 'prob')

p <- ggplot(mm, aes(x = x, y = prob, group = Model, col = Model)) +
  geom_line(aes(lty = Model), lwd = 1) +
  transition_time(test)
  
  



ggplot(fotball_lang, aes(x = antall, fill = person)) + 
  geom_bar()



snitt <- fotball_lang %>% 
  group_by(test_num) %>% 
  summarize(sum = sum(antall),
            gjennomsnitt = mean(antall),
            n = n(),
            sd = sd(antall),
            se = sd(antall)/sqrt(n()),
            median = median(antall)) %>% 
  mutate(Repetisjon = as.numeric(as.factor(test_num)))

ggplot(snitt, aes(x=Repetisjon, y=gjennomsnitt)) +
  geom_errorbar(aes(ymin=gjennomsnitt-(1.96*se), ymax=gjennomsnitt+(1.96*se)), colour="black", width=.2) +
  geom_line(color = 'red', size = 1.2) +
  geom_hline(yintercept = mean(fotball_lang$antall), color = 'blue', size = 1.2) +
  geom_hline(yintercept = mean(fotball_lang$antall) + (1.96*sd(fotball_lang$antall)/sqrt(nrow(fotball_lang))), color = 'blue', size = 0.2) +
  geom_hline(yintercept = mean(fotball_lang$antall) - (1.96*sd(fotball_lang$antall)/sqrt(nrow(fotball_lang))), color = 'blue', size = 0.2) +
  theme_light()
  


pred.m2 <- predprob(m2)
pred.m1 <- predprob(m1)

df <- data.frame(x = 0:9, Poisson = pred.m1, 
                 NegBin = pred.m2)

obs <- table(fotball_lang$antall_2) %>% prop.table() %>% data.frame #Observed
names(obs) <- c("x", 'Observed')

comb <- merge(obs, df, by = 'x', all = T)
comb[is.na(comb)] <- 0

mm <- pivot_longer(comb, -x, names_to = 'Model', values_to = 'prob')

ggplot(mm, aes(x = x, y = prob, group = Model, col = Model)) +
  geom_line(aes(lty = Model), lwd = 1) 


x <- 0:(max(tabell_data$antall)+1)
dfx <- as.data.frame(x)
for(i in 0:51) {
  hi <- (i*10) +200
  lo <- (i*10)
  sample <- fotball_lang %>% 
    arrange(test_num, n) %>% 
    rownames_to_column() %>% 
    mutate(rname = as.numeric(rowname)) %>%
    filter(rname %in% (lo:hi)) 
  m2 <- glm.nb(antall_2 ~ K, data = sample)
  probtable <- as.data.frame((dnbinom(x, size = m2$theta, mu = exp(m2$coefficients[1]))) * 100)
  dfx <- cbind(dfx, probtable)
}

names(dfx) <- c('x', as.character(1:52))
dfy <- dfx %>% 
  pivot_longer(-x, names_to = 'iter', values_to = 'p') %>% 
  mutate(iter = as.numeric(iter))

p <- ggplot(dfy, aes(x=x, y=p)) + 
  geom_line(color = 'red') +
  transition_time(iter) + 
  labs(title = "Forsøk: {round(frame_time*10)} -- {round(frame_time*10)+200}") +
  theme_light() +
  scale_x_continuous(breaks = seq(0,max(tabell_data$antall)-2,1),
                     labels = seq(2,max(tabell_data$antall),1))


animate(p, fps=5, end_pause = 50)








sample <- fotball_lang %>% 
  arrange(test_num, n) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname)) %>% 
  filter(rowname %in% 200)

probtable <- as.data.frame((dnbinom(x, size = m2$theta, mu = exp(m2$coefficients[1]))) * 100)
































library(gapminder)
head(gapminder)



p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p
