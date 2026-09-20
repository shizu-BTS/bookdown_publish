library("bookdown")

install.packages('tinytex')
tinytex::install_tinytex()

library("tidyverse")

library("tinytex")

tinytex::tlmgr("-version")
tlmgr--version

tinytex::install_prebuilt("path-to-downloaded-file")

tinytex:::is_tinytex()
#if false
tinytex::install_tinytex()


tinytex::tlmgr_update(all=TRUE, run_fmtutil = TRUE)

###yuhui debugging tip
# update all RR and latex package
update.packages(ask = FALSE, checkBuilt = TRUE)
tinytex::tlmgr_update()

### set the option in R
options(tinytex.verbose = TRUE)
install.packages("Hmisc", dependencies = TRUE, repos = "https://cran.rstudio.com")


###premable.tex
#xelatex대신 pdflatex ㅠㅠㅠㅠㅠ (24.7.5)

install.packages("tidyverse")
library(tidyverse)

ID<-c(1,2,3,4,5,6,7,8,9,10)
X<-c(1, 0.5, 2, 3, 3, 3.5, 4, 4, 4.5, 5)
Y<-c(12, 11, 18, 19, 21, 18.5, 21.5, 23.5, 22.5, 25)
Learning <-data.frame(ID, X, Y)
Learning %>%
  ggplot()+geom_point(aes(x=X, y=Y))+
  labs(x="learning(hrs)", y="job performance(point)")+
  xlim(0,6)+ylim(0,26)+
  geom_abline(intercept = 10, slope= 3, color="blue")+
  ggtitle("Relation between learning and job performance")+
  theme(plot.title = element_text(hjust=0.5))+
  annotate("text", x=4, y=10, label= "Y=10+3X", size=5)+
  annotate("text", x=4, y=8, label= "intercept=10")+
  annotate("text", x=3.8, y=6, label= "slope=3")

