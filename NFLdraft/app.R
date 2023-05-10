#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# library(shiny)
# library(markdown)
# library(ggplot2)
# library(bslib)
# library(readr)
# 
# 
# nfl_draft <- read_csv("nfl_draftcopy.csv")
# nfl_draft$rnd = factor(nfl_draft$rnd, levels = 1:7, ordered = TRUE)
# nfl_draft$hof = factor(nfl_draft$hof, levels = c("No", "Yes"))
# nfl_draft$college_univ[nfl_draft$player == "Jason Witten"] = "Tennessee"
# nfl_draft$college_univ[nfl_draft$player == "Shannon Sharpe"] = "Savannah St."
# nfl_draft$year_group = cut(nfl_draft$year, breaks = c(1985, 1995, 2005, Inf), labels = c("1985-1994", "1995-2004", "2005-2015"))
# attach(nfl_draft)
# 
# ui <- navbarPage(
#   theme = bs_theme(version = 4, "simplex"),
#   "NFL Draft Analysis",
#   tabPanel(
#     em("Round Value"),
#     titlePanel("Confidence Interval Accuracy"),
#     sidebarLayout(
#       sidebarPanel(
#         selectInput(
#           "samp_size_1",
#           h4("Position"),
#           choices = list(
#             "QB" = QB,
#             "RB" = RB,
#             "WR" = WR,
#             "TE" = TE,
#             "OT" = OT,
#             "G"  = G,
#             "C"  = C,
#             
#           ),
#           selected = 20
#         ),
#         sliderInput(
#           "cl_1",
#           h4("Confidence Level"),
#           min = 50,
#           max = 99,
#           value = 95
#         ),
#         sliderInput(
#           "num_samps",
#           h4("Number of Samples"),
#           min = 10,
#           max = 500,
#           value = 500
#         ),
#         br(),
#         strong(textOutput("info"))
#       ),
#       mainPanel(
#         br(),
#         plotOutput(
#           "plot1"
#         ),
#         br(),
#         strong("Sampling Distribution"),
#         p(
#           "An approximate sampling distribution is generated from ",
#           span("repeated sampling", style = "color:#E63523"),
#           " of many
#           samples of the same size from a population and shows the distribution of the statistic
#           calculated from each random sample. The sampling distribution shown above
#           was created from a normal population with mean 10 and standard deviation 0.75."),
#         em("Starting at 10, slowly increase the number of samples by dragging the slider to observe how sampling distributions are built."),
#         br(),
#         br(),
#         p("Notice from the sampling distribution
#           how there is variation in the sample means calculated from each independent random sample, but as the number of samples increase, this variation more and more resembles
#           a ", 
#           span("normal distribution", style = "color:#E63523"),
#           ". Since confidence intervals are
#           always centered around the sample mean, different independent random samples yield different confidence intervals of which some
#           will contain the population mean while some will not."),
#         em(
#           "Try changing the
#           sample size while keeping the confidence level constant and notice how the
#           sampling distribution changes."
#         ),
#         br(),
#         br(),
#         p(
#           "Notice that as the sample size increases and the random samples
#           become more representative of the population, the sampling distribution narrows in around the population mean
#           and the margin of error/width of each confidence interval decreases."
#         ),
#         strong("Confidence Level"),
#         p(
#           "A confidence level is essentially a measure of the ",
#           span("success rate ", style = "color:#E63523"),
#           "at which a calculated interval will contain a true population paramater. These intervals, referred to as confidence
#           intervals, attempt to capture the population parameter from repeated sampling. That being said,
#           higher confidence levels will result in a larger percentage of intervals constructed around various
#           sample means that contain the population mean. By contrast, lower confidence levels will result in a
#           smaller percentage of intervals constructed around various sample means that contain the population mean."
#         ),
#         em(
#           "Try adjusting the confidence level and see how the number of successful confidence intervals changes."
#         ),
#         br(),
#         br(),
#         p(
#           "Lower confidence levels decrease the margin of error for a confidence interval because of a smaller critical value, z."
#         )
#       )
#     )
#   ),
#   tabPanel(
#     em("Confidence Interval Width"),
#     titlePanel("Confidence Interval Width"),
#     sidebarLayout(
#       sidebarPanel(
#         h4("Confidence Interval Formula"),
#         withMathJax("$$\\overline{x}  \\pm z  \\frac{\\sigma}{n}$$"),
#         p(
#           "x bar is the sample mean, z is the critical value, sigma is the population standard deviation,
#           n is the sample size, and z*(sigma/n) is the margin of error." 
#         ),
#         selectInput(
#           "n",
#           h4("Sample Size"),
#           choices = list(
#             "10" = 10,
#             "20" = 20,
#             "50" = 50,
#             "100" = 100
#           ),
#           selected = 20
#         ),
#         sliderInput(
#           "cl_2",
#           h4("Confidence Level"),
#           min = 50,
#           max = 99,
#           value = 95
#         ),
#         p("The confidence interval width changes based on the sample size, N, and the level of confidence 
#         that the population mean is in the interval. The corresponding intervals then vary 
#         based on the sample mean associated with that interval."),
#       ),
#       mainPanel(
#         plotOutput("plot2"),
#         br(),
#         br(),
#         strong("Interval Widths"),
#         p("Confidence intervals vary in width depending on the selected confidence level, 
#           with greater confidence leading to wider ranges. Adjusting parameters to the graph above, we can look at how confidence intervals for
#           10 sample means react to changes in confidence levels and sample sizes."),
#         em(
#           "Try changing the confidence level while maintaining a constant n-value and observe the change in the intervals."
#         ),
#         br(),
#         br(),
#         p("You can see the intervals grow wider as confidence levels increase, which makes intuitive sense. 
#           If we encompass a greater range of values, we can be more confident that range holds the true population mean."
#         ),
#         p("The width of a confidence interval is also dependent on the number of observations in your sample."
#         ),
#         em(
#           "Try changing the sample size while maintaining a constant confidence level and observe the change in the intervals."
#         ),
#         br(),
#         br(),
#         p("We can see the trend that exists in the width of the confidence interval from this difference as well,
#           with the interval shrinking as the sample size for each mean increases. With a greater portion of the true population sampled,
#           the more accurately we can predict what the true population mean would be. You can also note that increasing the sample size reduces not only the width of the interval, 
#           but also the variation in the sample means.")
#       )
#     )
#   )
# )
# 
# # Generating a random, normal population
# set.seed(230)
# pop = rnorm(1000, 10, 0.75)
# 
# # Define server logic
# server <- function(input, output) {
#   # Repeated sampling
#   samp.means1 <- reactive({
#     value = NULL
#     for (i in 1:input$num_samps) {
#       value[i] = mean(sample(pop, as.numeric(input$samp_size_1)))
#     }
#     value
#   })
#   
#   
#   # Calculating the 95% confidence intervals for the 500 sample means
#   # Critical Value
#   z = reactive({
#     qnorm((1 - input$cl_1 / 100) / 2, lower.tail = FALSE)
#   })
#   # Lower bounds
#   lower <- reactive({
#     value = NULL
#     temp = samp.means1()
#     for (i in 1:input$num_samps) {
#       value[i] = temp[i] - (z() * 0.75) / sqrt(as.numeric(input$samp_size_1))
#     }
#     value
#   })
#   #Upper bounds
#   upper <- reactive({
#     value = NULL
#     temp = samp.means1()
#     for (i in 1:input$num_samps) {
#       value[i] = temp[i] + (z() * 0.75) / sqrt(as.numeric(input$samp_size_1))
#     }
#     value
#   })
#   
#   
#   # Determining which CIs capture population mean
#   success = reactive({
#     ifelse(lower() < 10 & upper() > 10, "Yes", "No")
#   })
#   df = reactive({
#     data.frame(samp.means1 = samp.means1(),
#                success = success())
#   })
#   
#   # Storing Distribution Values
#   
#   # Adjusting binwidth for sample size
#   value = NULL
#   bin <- reactive({
#     if (as.numeric(input$samp_size_1) == 10) {
#       bin = 0.015
#     }
#     else if (as.numeric(input$samp_size_1) == 20) {
#       bin = 0.011
#     }
#     else if (as.numeric(input$samp_size_1) == 50) {
#       bin = 0.007
#     }
#     else if (as.numeric(input$samp_size_1) == 100) {
#       bin = 0.005
#     }
#   })
#   
#   
#   # Creating the plot
#   output$plot1 <- renderPlot({
#     # Color-coded sampling distribution
#     ggplot(df(),
#            aes(
#              x = samp.means1,
#              y = ..count..,
#              color = success,
#              fill = success
#            )) +
#       geom_dotplot(binwidth = bin(), alpha = 0.75) +
#       labs(x = "Sample Means",
#            fill = "Success") +
#       scale_y_continuous(NULL, breaks = NULL) +
#       scale_fill_manual(values = c("#EB3F1A", "#6EE718")) +
#       scale_color_manual(values = c("#DB4B2B", "#6BD61E")) +
#       guides(color = "none") +
#       ggtitle(
#         paste(
#           "Sample means for n =",
#           input$samp_size_1,
#           "colored by whether or not their \n respective",
#           input$cl_1,
#           "% CI contains the population mean of 10"
#         )
#       ) +
#       theme(text = element_text(size = 10),
#             plot.title = element_text(size = 15))
#   })
#   
#   
#   
#   
#   
#   samp.means2 <- reactive({
#     value = NULL
#     for (i in 1:500) {
#       value[i] = mean(sample(pop, as.numeric(input$n)))
#     }
#     value
#   })
#   
#   z2 = reactive({
#     qnorm((1 - input$cl_2 / 100) / 2, lower.tail = FALSE)
#   })
#   
#   lower2 <- reactive({
#     value = NULL
#     temp = samp.means2()
#     for (i in 1:500) {
#       value[i] = temp[i] - (z2() * 0.75) / sqrt(as.numeric(input$n))
#     }
#     value})
#   
#   upper2 <- reactive({
#     value = NULL
#     temp = samp.means2()
#     for (i in 1:500) {
#       value[i] = temp[i] + (z2() * 0.75) / sqrt(as.numeric(input$n))
#     }
#     value})
#   
#   df2 = reactive({data.frame(samp.means2 = samp.means2(), lower2 = lower2(), upper2 = upper2())})
#   
#   
#   output$plot2 <- renderPlot({
#     ggplot(df2()[1:10,]) +
#       geom_pointrange(aes(x = samp.means2[1:10], y = 1:10, xmin = lower2[1:10],xmax = upper2[1:10]),color = "red") +
#       xlim(8.5,11.5) +
#       labs(
#         x = "Values",
#         y = "",
#       ) +
#       ggtitle(
#         paste(input$cl_2, "% Confidence Intervals of Sample Size ", input$n, "with Mean = 10 and SD = 0.75")
#       )
#   })
#   
#   # Rendering text
#   output$instruction <- renderText({
#     paste0(
#       "Hover over the samples in the sampling distribution to display their ",
#       sep = "",
#       input$cl_1,
#       "% confidence interval."
#     )
#   })
#   
#   # Number of successful CIs
#   x <- reactive({
#     sum(success() == "Yes")
#   })
#   
#   output$info <- renderText({
#     paste0(
#       x(),
#       " out of ", input$num_samps, " samples in the sampling distribution have a ",
#       sep = "",
#       input$cl_1,
#       "% confidence interval that contain the population mean of 10."
#     )
#   })
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)

