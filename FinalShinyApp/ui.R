shinyUI(pageWithSidebar(
    headerPanel("Next Word Prediction Website"),
    sidebarPanel(h3('Please input your phrase here'),
                 textInput(inputId="phrase",label="Your phrase"),
                 submitButton('OK')
                 #actionButton("goButton","OK")
                 ),
    mainPanel(h2('Next Word Prediction result'),
              h4('Your phrase entered:'),
              verbatimTextOutput('phrase'),
              h4('1:'),
              verbatimTextOutput("word1"),
              h4('2:'),
              verbatimTextOutput("word2"),
              h4('3:'),
              verbatimTextOutput("word3"),              
              h2('Welcome to the next word prediction website'),
              p('Nowadays, mobile devices are very popular,
                and even indispensable for some people in everyday living.
                A lot of people are using them to send text messages,
                chat with friends using social network apps, and
                write down notes, contact information, shopping list,
                and all other important texts on their mobile devices.
                But the small screen size of those mobile devices often
                makes it inconvenient to do so. Next word prediction,
                even not always correct, can greatly improve the writing
                speed on such devices, because many times people can just
                select the words predicted for them without the need to
                fully write the words down.'),
              p('This website provides a better next word prediction application than SwiftKey,
                a current well known next word application.
                This application can predict the next word correctly
                for six out of the twenty phrases given in the quizzes of the Johns Hopkins University
                Data Science Capstone Coursera course, while SwiftKey can only get four of them correct.
                If using data from twitter and blogs, it can even get more than half of those twenty right.
                However, the application on this website is geared towards twitter and news.
                In my test on random texts from twitters and news,
                this application has 30% accuracy for all three predicting words in total.'),
              p('The prediction application on this website is also super fast.
                On average, it takes 0.05 seconds to predict for one phrase,
                and it has never exceeded 0.12 seconds in my test.'),
              p('This application is also small. It only takes about 14 MB.
                Comparing to the SwiftKey app on my cellphone,
                which takes about 28 MB, this application is much lighter.'),
              p('Hence this application has a very promising potential and worth investing in.
                Imagining what will happen if you are a sponsor of some company like SwiftKey!'),
              p('Feel free to input some phrases of your choice in the text box (preferably from twitter and news)
                and click the OK button to the left, and have fun predicting.
                The result will be shown in the right main panel above.
                If you are interested in funding this application, please email me at
                wslw77@gmail.com. Thank you very much for your help in advance!')
              )
))