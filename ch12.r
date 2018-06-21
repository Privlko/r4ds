# chapter 12 on factors and categories ------------------------------------



# prereq ------------------------------------------------------------------

library(tidyverse)
library(forcats)


# creating factors --------------------------------------------------------

x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jal", "Mar")

sort(x1)

#if you record as a factor you can fix the above.
#first make levels


month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)


month_levels
# now you can make the variable itself.

y1 <- factor(x1, levels = month_levels)
y1
sort(y1)


y2 <-  parse_factor(x2, levels = month_levels)
factor(x1)

# ordering the factors in a unique way
f1 <- factor(x1, levels = unique(x1))
f1

f2 <- x1
factor(f2)
fct_inorder(f2)

levels(f1)

# General Social Survey ---------------------------------------------------

gss <- forcats::gss_cat

?gss_cat

# looking at levels
gss%>%
count(race)

ggplot(gss, aes(race))+
  geom_bar()

ggplot(gss, aes(race))+
  geom_bar() +
  scale_x_discrete(drop=FALSE)



# EXERCISES ---------------------------------------------------------------


# 1 
gss%>%
  count(rincome)

ggplot(gss, aes(rincome))+
  geom_bar() +
  scale_x_discrete()+
  coord_flip()

# 2
ggplot(gss, aes(relig))+
  geom_bar() +
  scale_x_discrete()+
  coord_flip()

ggplot(gss, aes(partyid))+
  geom_bar() +
  scale_x_discrete()+
  coord_flip()

ggplot(gss, aes(denom))+
  geom_bar() +
  scale_x_discrete()+
  coord_flip()


# 3
gss%>%
  count(denom, relig)

ggplot(gss)+
  geom_bar(aes(denom, fill=relig)) +
  scale_x_discrete()+
  coord_flip()



# changing factor levels --------------------------------------------------

relig <- gss %>%
  group_by(relig) %>%
  summarise(age = mean(age, na.rm=TRUE),
            tvhours= mean(tvhours, na.rm=TRUE),
            n = n()
            )
relig


ggplot(relig, aes(tvhours,relig))+
  geom_point()

#the factors are all over the place, lets tidy

ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours)))+
  geom_point()

##using mutate instead of focusing on aes

ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours)))+
  geom_point()

relig %>%
  mutate(relig= fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()


rincome <- gss %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm=TRUE),
    tvhours = mean(tvhours, na.rm=TRUE),
    n = n())



ggplot(rincome,
       aes(age, fct_reorder(rincome, age)))+
  geom_point()



ggplot(rincome,
       aes(age, fct_relevel(rincome, "Not applicable", "Refused")))+
  geom_point()


##reordering factors by values of x and y

by_age <- gss %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count()

 gss %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count() %>%
  mutate(sum(n))



ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm=TRUE)


#increasing frequency

gss %>%
  mutate(marital = marital %>% fct_infreq() %>%  fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()


gss %>%
  mutate(marital = marital %>% fct_infreq()) %>%
  ggplot(aes(marital)) +
  geom_bar()


ggplot(gss, aes(tvhours))+
  geom_bar()

summarise(gss, mean(tvhours))
mean(gss$tvhours)
gss

# modifying factor levels -------------------------------------------------

gss %>%
  count(partyid)


gss %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong" = "Strong republican",
                              "Republican, weak" = "Not str republican",
                              "Independent, near rep" = "Ind,near rep", 
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak" = "Not str democrat",
                              "Democrat, strong" = "Strong democrat")) %>%
  count(partyid)
  


gss %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong" = "Strong republican",
                              "Republican, weak" = "Not str republican",
                              "Independent, near rep" = "Ind,near rep", 
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak" = "Not str democrat",
                              "Democrat, strong" = "Strong democrat",
                              "Other" = "No answer",
                              "Other" = "Don't know",
                              "Other" = "Other party")) %>%
  count(partyid)
