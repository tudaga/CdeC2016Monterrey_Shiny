#install.packages("shiny")
library(shiny)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("reshape")
library(reshape)
#install.packages("slam")
library(slam)
#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)
#install.packages("wordcloud")
library(wordcloud)


# ==== Data ====

setwd("data")
mty_data <- readRDS("mty_data.RDS")

# Additional Questions Data

mty_data_add <- readRDS("mty_data_add.RDS")
mty_data_bubble <- readRDS("mty_data_bubble.RDS")
improve_Corpus <- readRDS("improve_Corpus.RDS")
science_attitude_Corpus <- readRDS("science_attitude_Corpus.RDS")
CdeC_Corpus <- readRDS("CdeC_Corpus.RDS")

shinyServer(
  function(input, output) {
    output$plot = renderPlot({
      if(input$PrePost == "Histograms"){
        # ==== Overlapping Histograms For Pre and Post Scores ====
        # change data frame to long format (one score per row)
        dat = melt(mty_data, id = "Name")
        dat$value <- as.numeric(dat$value)
        # counts
        theme_set(theme_grey(base_size = 18)) 
        ggplot(dat, aes(x = value, color = variable, fill = variable)) + 
          geom_histogram(alpha = 0.7, position = "identity", binwidth = 7) +
          labs(title = "Histograms of Pre and Post Scores", x = "Score", y = "Counts", color = "", fill = "")
      } else if(input$PrePost == "Violin Plots"){
        # ==== Violin Plots of Pre and Post Scores ====
        # change data frame to long format (one score per row)
        dat = melt(mty_data, id = "Name")
        dat$value <- as.numeric(dat$value)
        theme_set(theme_grey(base_size = 18)) 
        ggplot(dat, aes(x = variable, y = value, fill = variable)) + 
          geom_violin(trim = FALSE, alpha = 0.7, bw = "SJ") + 
          labs(title = "Violin Plots of Pre and Post Scores", x = "", y = "",color="",fill="")
      } else if(input$PrePost == "Box Plots"){
        # ==== Box Plots for Pre and Post Scores ====
        # change data frame to long format (one score per row)
        dat = melt(mty_data, id = "Name")
        dat$value <- as.numeric(dat$value)
        theme_set(theme_grey(base_size = 18)) 
        ggplot(dat, aes(x = variable, y = value, fill = variable)) + 
          geom_boxplot(alpha = 0.7) + 
          labs(title="Box Plots for Pre and Post Scores", x = "", y = "", color = "", fill = "")
      } else if(input$PrePost == "Scatter Plot"){
        # ==== Scatter Plot for Pre and Post Scores ====
        theme_set(theme_grey(base_size = 18)) 
        ggplot(mty_data_bubble, aes(x = Pre, y = Post)) + 
        geom_point(aes(colour = Gender, alpha = 0.7, size = I(Improvement))) + # I() -- "as is"
          xlim(0, 100) + ylim(0, 100 + max(mty_data_bubble$Improvement)/2) +
          geom_abline(slope = 1, intercept = 0, colour = "cyan4") +
          labs(x = "Pre", y = "Post", color = "", fill = "") +
          ggtitle(expression(atop("Pre and Post Scores Scatter Plot", 
                                  atop(italic("Size of bubble proportional to improvement."), ""))))
        
        
      } else if(input$PrePost == "Gender"){
        # ==== Pie Chart for Gender ====
        theme_set(theme_grey(base_size = 18)) 
        ggplot(mty_data_add, aes(x = factor(1), fill = factor(Gender))) + 
          geom_bar(width = 1, alpha = 0.7) + 
          coord_polar(theta = "y") +
          labs(title = "Gender Pie Chart", x = "", y = "", color = "", fill = "")
      } else if(input$PrePost == "Club Rating"){
        # ==== Barplot for Club Rating ====
        YMAX = max(table(mty_data_add[,4])) # scale is 1 to 5
        theme_set(theme_grey(base_size = 18)) 
        ggplot(mty_data_add, aes(x = mty_data_add[,4])) + 
          geom_bar(stat = "Count", fill = "cyan3", alpha = 0.85) + 
          scale_x_continuous(limits = c(0.5, 5.5), breaks = 1:5, labels = 1:5) +
          scale_y_continuous(limits = c(0,YMAX), breaks = 0:YMAX) +
          labs(title="Bar Plot of Ratings", x = "Rating", y = "Counts")
      } else if(input$PrePost == "Wordcloud - How would you improve the club?"){
        # ==== Word Clouds ====
        pal <- brewer.pal(9,"YlGnBu")
        pal <- pal[-(1:4)]
        set.seed(4)
        wordcloud(words = improve_Corpus, scale=c(5,0.1), max.words=100, random.order=FALSE,
                  rot.per=0.35, use.r.layout=FALSE, colors=pal)
      } else if(input$PrePost == "Wordcloud - How did your attitude towards science change?"){
        # ==== Word Clouds ====
        pal <- brewer.pal(9,"YlGnBu")
        pal <- pal[-(1:4)]
        set.seed(6)
        wordcloud(words = science_attitude_Corpus, scale=c(5,0.1), max.words=100, random.order=FALSE,
                  rot.per=0.35, use.r.layout=FALSE, colors=pal)
      } else if(input$PrePost == "Wordcloud - What do you think of Clubes de Ciencia?"){
        pal <- brewer.pal(9,"YlGnBu")
        pal <- pal[-(1:4)]
        set.seed(8)
        wordcloud(words = CdeC_Corpus, scale=c(5,0.1), max.words=100, random.order=FALSE,
                  rot.per=0.35, use.r.layout=FALSE, colors=pal)
      }
    })
  }
)


