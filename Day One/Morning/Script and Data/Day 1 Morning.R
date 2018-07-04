library (dplyr) #data wrangling package
library (tidyr) #data wrangling package
library (ggplot2) #needed for ggplot
library (yarrr) #needed for pirate plots
library (viridis) #needed for heat map plotting
library (MASS) #needed for sample from normal distribution
library (Hmisc) #needed for correlation
library (psych) #needed for describeBy to generate descriptives
library (emmeans) #needed for pairwise comparisons
library (DescTools) #needed for effect size calculations
library (afex) #needed for factorial ANOVA (Type III SS and contrast coding)
library (car) #needed for Durbin Watson test
library (olsrr) #needed for adjusted R squared model fit improvement
library (leaps) #helps visualise multiple regression model fit
library (readr) #needed to read in data files
library (stargazer) #nice regression tables
library (multilevel) #needed for Sobel test to interpret mediation via Baron and Kenny method
library (mediation) #needed to use the mediate funciton
library (rtweet) #needed to scrape via Twitter Developer API
library (leaflet) #needed to geo plot Twitter data
library (maps) #needed to geo plot Twitter data


#Data Wrangling
#Create data for 10,000 people - each with measures of Working Memory (WM), IQ, and Reading Comprehension (Comp)
set.seed (1234)
ID <- seq (1:10000)
WM <- as.integer(rnorm (10000, mean=50, sd=5))
IQ <- as.integer(rnorm (10000, mean=100, sd=15))
Comp <- as.integer(rnorm (10000, mean=20, sd=2))

data <- data.frame (ID, WM, IQ, Comp)

colnames(data) = c("ID", "WM", "IQ", "Comp")

#Create data for 48 participants (all present in data) taking part in an experiment
ID <- sample (ID, 48)
Simple <- as.integer(rnorm (48, mean=2000, sd=140))
Complex <- as.integer(rnorm (48, mean=2400, sd=160))

dataRT <- data.frame (ID, Simple, Complex)
colnames(dataRT) = c("ID", "Simple Sentence", "Complex Sentence")

dataRT_all <- inner_join (data, dataRT, by=(c("ID")))

data_transformed <- mutate (dataRT_all, log_Simple=log (dataRT_all$`Simple Sentence`), log_Complex=log (dataRT$`Complex Sentence`))

index <- data_transformed$ID!=1191
index

filtered_data <- data_transformed[index,]

data_long <- gather (dataRT, "Condition", "RT", c("Simple Sentence", "Complex Sentence"))
View (data_long)

data_wide <- spread (data_long, "Condition", "RT", c("Simple Sentence", "Complex Sentence"))
View (data_wide)

#Data Visualisation
#Bar Graph
data_summ <- data_long %>% group_by(Condition) %>% summarise(Mean = mean(RT), sd = sd(RT))
ggplot (data_summ, aes (x=Condition, y=Mean, group=Condition, fill=Condition, ymin=Mean-sd, ymax=Mean+sd)) + geom_bar(stat = "identity", width=.5) + geom_errorbar(width=.25) +  ggtitle("Bar chart with Error Bars") + guides(fill=FALSE) 

#Violin Plot
ggplot (data_long, aes (x=Condition, y=RT, group=Condition, colour=Condition)) + geom_violin() + geom_jitter(alpha=.5, position=position_jitter(0.05)) + guides(colour=FALSE, fill=FALSE) 

#Pirate plot
pirateplot(formula=RT~Condition, data=data_long, inf.method="se", cex.axis=.75, theme=1)

#Bar Graph plus raw data
pirateplot(formula=RT~Condition, data=data_long, inf.method="se", cex.axis=.75, theme=4)

#Scatterplots
ggplot (data, aes (x=data$WM, y=data$IQ)) + geom_point()
ggplot (data, aes (x=data$WM, y=data$IQ)) + geom_jitter(alpha=.1, position=position_jitter(0.5)) 
ggplot (data, aes (x=data$WM, y=data$IQ)) + geom_jitter(alpha=.1, position=position_jitter(0.5)) + geom_smooth(method="lm")
ggplot (data, aes (x=data$WM, y=data$IQ)) + stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) + scale_fill_viridis() +coord_cartesian(expand = FALSE) 

#creating two perfectly correlated variables, N=1000
set.seed(1234)
mysigma <- matrix (c(1,1,1,1),2,2)
x1 <- mvrnorm(n=1000, c(5.3,10), mysigma)
x5 <- as.data.frame (x1)
colnames(x5) <- c("IQ", "WM")

#Scatterplots
ggplot (x5, aes (x=x5$WM, y=x5$IQ)) + geom_jitter(alpha=.1, position=position_jitter(0.5)) 
ggplot (x5, aes (x=x5$WM, y=x5$IQ)) + geom_jitter(alpha=.1, position=position_jitter(0.5)) + geom_smooth(method="lm")
ggplot (x5, aes (x=x5$WM, y=x5$IQ)) + stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) + scale_fill_viridis() +coord_cartesian(expand = FALSE) 
rcorr (x5$IQ, x5$WM)

#Raincloud plot
library(RColorBrewer)
library(plyr) #note, need to detach this after this plot as clashes with aspects of dplyr
source("https://gist.githubusercontent.com/ajstewartlang/6c4cd8ab9e0c27747424acdfb3b4cff6/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

dataRT <- data_long

raincloud_theme = theme(
  text = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 12),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=12),
  legend.text=element_text(size=12),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

sumld <- ddply(dataRT, ~Condition, summarise, mean = mean(RT), median = median(RT), lower = lb(RT), upper = ub(RT))
head(sumld)

ggplot(data = dataRT, aes(y = RT, x = Condition, fill = Condition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, trim=FALSE) +
  geom_point(aes(y = RT, color = Condition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1,  outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 3) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  labs(x=NULL) +
  scale_y_continuous(breaks = seq(1500,3000,by = 200))

#Time series plots using the 'economics' dataset
ggplot(economics, aes(x=date)) + 
  geom_line(aes(y=pop)) + 
  labs(title="Time Series Chart", 
       subtitle="US Population Size from 'Economics' Dataset", 
       caption="Source: Economics", 
       y="Total Population (thousands)", x="Date")

ggplot(economics, aes(x=date)) + 
  geom_line(aes(y=psavert)) + 
  labs(title="Time Series Chart", 
       subtitle="US Savings Rate from 'Economics' Dataset", 
       caption="Source: Economics", 
       y="Savings Rate (%)", x="Date")

ggplot(economics, aes(x=date)) + 
  geom_line(aes(y=unemploy)) + 
  labs(title="Time Series Chart", 
       subtitle="US Unemployement Rate from 'Economics' Dataset", 
       caption="Source: Economics", 
       y="Unemployment (thousands)", x="Date")

#animated plot - first we need to tidy the data and aggregate by Year

economics.tidy <- economics %>% mutate (year=year (date)) %>% group_by(year) %>%
  summarise (mean_pop=mean(pop), mean_un=mean(unemploy))

economics.tidy$mean_pop <- scale (economics.tidy$mean_pop)
economics.tidy$mean_un <- scale (economics.tidy$mean_un)

#first a static plot
data <- economics.tidy
p <- ggplot(data, aes(mean_pop, mean_un)) + ylim(-3, 3) + xlim (-3, 3) +
  geom_point(size=5, colour="red")+ ggtitle("All Years") + labs (x="Population Size (Z-score)", y="Unemployment Size (Z-score)")
print(p)

#now animated
img <- image_graph(720, 408, res = 96)
datalist <- split(economics.tidy, economics.tidy$year)
out <- lapply(datalist, function(data){
  p <- ggplot(data, aes(mean_pop, mean_un)) + ylim(-3, 3) + xlim (-3, 3) +
    geom_point()  + ggtitle(data$year) + labs (x="Population (Z-score)", y="Unemployment (Z-score)")
  print(p)
})
dev.off()
animation <- image_animate(img, fps = 2)
print(animation)

#Animated plot using the 'gapminder' dataset
#might need to add gganimate via devtools
library(gapminder)

img <- image_graph(720, 408, res = 96)
datalist <- split(gapminder, gapminder$year)
out <- lapply(datalist, function(data){
  p <- ggplot(data, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
    scale_size("population", limits = range(gapminder$pop)) + geom_point() + ylim(20, 90) + 
    scale_x_log10(limits = range(gapminder$gdpPercap)) + ggtitle(data$year) + theme_classic() +
    labs (x="GDP", y="Life Expectancy")
  print(p)
})
dev.off()
animation <- image_animate(img, fps = 2)
print(animation)

#this writes the image as an animated gif
image_write(animation, "animated.gif")

#Twitter scraping using rtweet package 
#will request authentication with Twitter Developer API in browser
#this example searches for Tweets about Scandinavian progressive death metal band Opeth
rt <- search_tweets(
  "Opeth", n = 1000, include_rts = FALSE, retryonratelimit = TRUE
)
rt1 <- separate(data = rt, col = created_at, into = c("date", "time"), sep = " ") 
rt1 <- rt1[!is.na(rt1$date),]
rt1$date <- as.factor (rt1$date)
data <- rt1
ggplot (data, aes (x=date)) + geom_bar(fill="blue") + scale_y_continuous(expand = c(0,0),
                                                                         limits = c(0,500)) + 
  labs(x="Day", y="Number of Tweets") + ggtitle ("Tweets Mentioning Opeth") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plotting Tweets on map
mymap <- lat_lng(rt)
m <- leaflet(mymap) %>% addTiles()
m %>% addCircles(lng = ~lng, lat = ~lat, weight = 8, radius = 40, color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)

##get tweets from two news organisations
tmls <- get_timelines(c("Guardian", "thetimes"), n = 10000)
tmls %>%
  dplyr::filter(created_at > "2018-04-1") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by news organization",
    subtitle = "Twitter status (tweet) counts aggregated by day",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#describing your data using describeBy
describeBy (data_long$RT, group=data_long$Condition)

#describing your data using dplyr
data_long %>% group_by (Condition) %>% summarise (mean=mean(RT), sd=sd(RT))

#ANOVA - one factor, three levels between participants design
set.seed(1234)
x1 <- rnorm(n=15, mean=5.3, sd=.4)
x2 <- rnorm(n=15, mean=7.1, sd=.45)
x3 <- rnorm(n=15, mean=9.2, sd=.5)

x5 <- c (x1,x2,x3)
x4 <- (1:45)
x6 <- (1:45)

cond <- as.data.frame(cbind (x4, x6, x5))

for (i in 1:15) {
  cond$x6[i]="Water"
}

for (i in 16:30) {
  cond$x6[i]="Single Espresso"
}

for (i in 31:45) {
  cond$x6[i]="Double Espresso"
}

colnames (cond) <- c("Participant", "Condition", "Ability")
cond <- as.data.frame (cond)
cond$Condition <- as.factor (cond$Condition)

cond$Condition <- relevel(cond$Condition, ref=3)

pirateplot (formula = Ability ~ Condition, data=cond , theme = 0, # Start from scratch
            inf.method = "sd", # Use standard deviations to define band limits
            inf.f.o = .7, # Band opacity
            inf.f.col = piratepal("basel"), # Add color to bands
            point.o = 1, # Point opacity
            avg.line.o = .8, # Average line opacity
            gl.col = gray(.6), # Gridline specifications
            gl.lty = 1, # Gridline specifications
            gl.lwd = c(.5, 0), # Gridline specifications
            ylim=c(0,10), # Y-axis limits
            cex.names=.85, # Axis labels scale (1=full scale)
            main="Data of Ability by Condition") # Graph title

#generate some descriptives
describeBy (cond$Ability, group=cond$Condition)

#run the ANOVA
model <- aov(Ability ~ Condition, data=cond)
anova(model)

#running pairwise comparisons with corrections - also get CIs
emmeans (model, pairwise~Condition, adjust="Bonferroni")
emmeans (model, pairwise~Condition, adjust="Tukey")

#get effect size of Condition
EtaSq(model, type=3, anova=TRUE)

#Repeated Measures ANOVA - one factor
#generate data - repeated measures, one factor with 4 levels 
set.seed(1234)
x1 <- rnorm(n=32, mean=85, sd=4)
x2 <- rnorm(n=32, mean=84, sd=4)
x3 <- rnorm(n=32, mean=72, sd=7)
x4 <- rnorm(n=32, mean=54, sd=6)

x5 <- c((1:32), (1:32), (1:32), (1:32))
x6 <- (1:128)

data <- c (x1,x2,x3,x4)
data <- cbind (x5, x6, data)
data <- as.data.frame(data)

for (i in 1:32) {
  data$x6[i]="Very Easy"
}

for (i in 33:64) {
  data$x6[i]="Easy"
}

for (i in 65:96) {
  data$x6[i]="Hard"
}

for (i in 97:128) {
  data$x6[i]="Very Hard"
}

colnames (data) <- c("Participant", "Condition", "Score")

data$Condition <- factor (data$Condition, levels = c("Very Easy", "Easy", "Hard", "Very Hard"))

data$Score <- as.integer (data$Score)

#Pirateplot
pirateplot (formula = Score ~ Condition, data=data , theme = 0, # Start from scratch
            inf.f.o = .7, # Band opacity
            inf.f.col = piratepal("basel"), # Add color to bands
            point.o = 1, # Point opacity
            avg.line.o = .8, # Average line opacity
            gl.col = gray(.6), # Gridline specifications
            gl.lty = 1,
            inf.method = 'sd',
            gl.lwd = c(.5, 0), ylim=c(0,100))

#plot for each participant
ggplot (data, aes (Condition, Score, colour=Condition)) + ylim(0,100) + geom_point() + facet_wrap(~data$Participant)

#generate some descriptives
describeBy (data$Score, group=data$Condition)

#write data as a .csv file
#write.csv (data, "datafiletest.csv")

model <- aov_4 (Score~Condition + (1+Condition|Participant), data=data)
summary (model)
anova (model)

#running pairwise comparisons with corrections - also get CIs
emmeans (model, pairwise~Condition, adjust="Bonferroni")

#2 x 2 repeated design long data format
DV <- read_csv("DV.csv", col_types = cols(Context = col_factor(levels = c("Positive", 
                                                                          "Negative")), Sentence = col_factor(levels = c("Positive", 
                                                                                                                         "Negative"))))
#By Subjects
model <- aov_4 (RT ~ Sentence*Context + (1+Sentence*Context |Subject), data=DV, na.rm=TRUE)
anova (model)

#By Items
model1 <- aov_4 (RT ~ Sentence*Context + (1+Sentence*Context |Item), data=DV, na.rm=TRUE)
anova (model1)

emmeans (model, pairwise ~ Sentence*Context, adjust="none")

describeBy(DV, group=list(DV$Sentence,DV$Context))

#ANCOVA
cond <- read_csv("cond.csv")
cond$Condition <- as.factor (cond$Condition)

ggplot (cond, aes (x=Gaming, y=Ability,  colour=Condition)) + geom_point () 

describeBy (cond$Ability, group=cond$Condition)
describeBy (cond$Gaming, group=cond$Condition)

ggplot (cond, aes (x=Gaming, y=Ability,  colour=Condition)) + geom_point () 

#Separately by Condition
ggplot (cond, aes (x=Gaming, y=Ability,  colour=Condition)) + geom_point () + facet_wrap('Condition') + geom_smooth(method='lm')

#run the ANOVA (i.e., without the covariate)- model is significant
model <- aov(Ability ~ Condition, data=cond)
anova(model)

#run the ANCOVA - when we add the effect of Gaming Frequency first,
#the model is now not significant
model_ancova <- aov(Ability ~ Gaming + Condition, data=cond)
anova(model_ancova)

#unadjusted means
describeBy (cond$Ability, group=cond$Condition)

#report adjusted means
emmeans (model_ancova, pairwise~Condition, adjust="none")

#Regression
#Read in regression data

regression <- read_csv("regression2.csv")

model.full <- lm (Points ~ Investment + Car_performance + Age + Media_coverage, data=regression)
model.null <- lm (Points ~ 1, data=regression)

anova (model.full, model.null)

summary (model.full)

model2 <- lm (Points ~ Investment , data=regression)
anova (model.full, model2)

AIC (model.full)
AIC (model2)

plot (model2)

durbinWatsonTest(model2)

steplimitsboth <- step(model.null, scope=list (upper=model.full), direction = "both")
summary (steplimitsboth)

#stepwise based on adjusted R squared improvement
pmodel <- ols_step_forward(model.full)
pmodel

leapsmodels <- regsubsets (Points ~ Investment + Car_performance + Age + Media_coverage, data=regression)
plot (leapsmodels, scale="adjr2", main="Models")

vif (steplimitsboth)

#mediation analysis
set.seed(123) #Standardizes the numbers generated by rnorm; see Chapter 5
N <- 100 #Number of participants; graduate students
X <- rnorm(N, 175, 7) #IV; hours since dawn
M <- 0.7*X + rnorm(N, 0, 5) #Suspected mediator; coffee consumption 
Y <- 0.4*M + rnorm(N, 0, 5) #DV; wakefulness
Meddata <- data.frame(X, M, Y)

#using the mediation package
fitM <- lm(M ~ X,     data=Meddata) #IV on M; Hours since dawn predicting coffee consumption
fitY <- lm(Y ~ X + M, data=Meddata) #IV and M on DV; Hours since dawn and coffee predicting wakefulness

fitMed <- mediate(fitM, fitY, treat="X", mediator="M")
summary(fitMed)

fitMedBoot <- mediate(fitM, fitY, boot=TRUE, sims=10000, treat="X", mediator="M")
summary(fitMedBoot)
