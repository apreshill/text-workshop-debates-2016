---
title       : What's in a word?
subtitle    : Techniques for automating analysis of language
author      : Steven Bedrick & Alison Presmanes Hill
job         : Oregon Health & Science University
framework   : revealjs       # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : default    # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
url         : {lib: "."}
revealjs:
  theme: beige
  transition: convex
  height: 600
---



<!-- My Fonts -->
<link href='https://fonts.googleapis.com/css?family=Raleway:900,800,700,600' rel='stylesheet' type='text/css'>
<link href='https://fonts.googleapis.com/css?family=Cabin:400,700italic,700,400italic,500,500italic' rel='stylesheet' type='text/css'>
<link href='https://fonts.googleapis.com/css?family=Lobster+Two:400,700' rel='stylesheet' type='text/css'>
<link href="https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css" rel="stylesheet">
<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script>


<style>
.reveal h1 {
    color: #DD6540; 
    font-size: 2em;
    font-weight: 700;
    text-shadow: none;
    text-align: left;
    padding-bottom: 30px;
    text-transform: none;
    letter-spacing: normal;
    font-family: Raleway, sans-serif;
}
.reveal h2 {
    color: #6699DD; 
    text-align: left;
    font-size: 1.5em; 
    font-weight: 700;
    text-transform: none;
    padding-bottom: 30px;
    letter-spacing: normal;
    font-family: Cabin, sans-serif;
}
.reveal h3 {
    color: #64354A;
    text-align: left;
    font-size: 1em; 
    text-transform: capitalize;
    padding-bottom: 10px;
    font-weight: 400;
    font-family: Cabin, sans-serif;
}
.reveal h6 {
    color: #92E1FE;
    text-align: left;
    padding-bottom: 10px;
    font-family: Cabin, sans-serif;
}
.reveal body {
    font-family: Cabin, sans-serif;
    text-align: left;
}
.reveal p {
    font-family: Cabin, sans-serif;
}
.reveal ul {
    font-family: Cabin, sans-serif;
}
.reveal ol {
    font-family: Cabin, sans-serif;
}
.reveal li {
    font-family: Cabin, sans-serif;
}
.reveal i.fa { 
    font-family:FontAwesome; 
    font-style: normal; 
    color: #6699DD;
} 
.reveal a:link { 
/* unvisited link */ 
    font-family: Cabin, sans-serif;
    color: #D14D4D; 
} 
.reveal a:visited { 
/* visited link */
    font-family: Cabin, sans-serif;
    color: #D14D4D; 
} 
.reveal a:hover { 
/* hover link */
    color: #92E1FE; 
} 
</style>







# What's in a word?

## Techniques for automating analysis of language

### Steven Bedrick & Alison Presmanes Hill

<div class="list-group" style="font-size: .6em" align = "left">
  <a class="list-group-item" href="mailto:bedricks@ohsu.edu"><i class="fa fa-envelope fa-fw"></i>&nbsp; bedricks@ohsu.edu</a><br>
    <a class="list-group-item" href="mailto:hillali@ohsu.edu"><i class="fa fa-envelope fa-fw"></i>&nbsp; hillali@ohsu.edu</a><br>
  <a class="list-group-item" href="#"><i class="fa fa-map-marker fa-fw"></i>&nbsp; OHSU Gaines Hall 19 & 21 (we're neighbors)</a>
</div>

---
## Why computers and linguistics?

<p> Traditional content analysis techniques are: </p>

* Time-consuming
* Subjective
* Tedious!

---

## Software can help!

---

## A note on "objectivity"

<p> "Computers are inherently objective, right?" </p>

---

## Steps in computational analysis of language:

1. Frame question
2. Identify data
3. Know your data
4. Figure out what to measure
5. Measure it
6. Rinse and repeat!

---

## Today: A <em>whirlwind tour</em> of what this kind of analysis looks like!

---

## Steps in computational analysis of language:

1. Frame question
2. Identify data
3. Know your data
4. Figure out what to measure
5. Measure it
6. Rinse and repeat!

---

## What is Python?

<p> Python is a programming language, that is perfect for: </p>

* Automating repetitive processes
* Working with files
* Working with text
* Free!

---

## We will use Python for:

1. Obtaining our data
2. Cleaning our data
3. Organizing our data

---

## To Python!

---

## What is R?

<p> R is a programming language.</p>

<p class="fragment">There is also a simple software environment for using R, released through the comprehensive R archive network (CRAN).</p>


--- {background: "black"}


## Why learn/use R?

> * Free
> * Open source
> * Available on almost every major platform

<p class="fragment" style="color: #D14D4D; font-family: 'Lobster Two', cursive;">... all of which makes your research <span class="fragment">reproducible,</span> <span class="fragment">reusable,</span> <span class="fragment">and shareable</span></p>

---

## What is RStudio?

<img src="https://www.rstudio.com/wp-content/uploads/2014/03/blue-250.png" style="border: 0;">

RStudio provides an *integrated development environment* or IDE for using R.

There is an edition of RStudio that is free and open source, just like R.

---

## We will use R for:

1. Cleaning our data (even more)
2. Wrangling our data
3. Processing our data (getting summary statistics)
4. Visualizing our data


---

## What do I need to know about R **right now**?

> * R is an interpreter <i class="fa fa-angle-right"></i>
> * Object-oriented `<-`
> * Case matters (`i_like_snake_case`)
> * Anything behind a `#` is a comment
> * Packages! Install once per machine, once per R session
> * Piping/chaining with the `%>%` operator

---


## Object-oriented


```r
fave_number <- 13
fave_number*3 + 5^3
```

```
[1] 164
```

```r
new_thing <- c(1, 2, 8, "Willie Nelson")
```



--- 

## Case matters


```r
new_thing <- c(1, 2, 8, "Willie Nelson")
New_thing
```

```
Error in eval(expr, envir, enclos): object 'New_thing' not found
```



---

## Anything behind a `#` is a comment


```r
new_thing <- c(1, 2, 8, "Willie Nelson")
# I can say anything I want here
and # here
```

```
Error in eval(expr, envir, enclos): object 'and' not found
```

---

## Packages! 


<span style="color: #D14D4D">Install once</span> per machine


```r
install.packages("ggplot2")
```

<span style="color: #D14D4D">Load once</span> per R work session

```r
library(ggplot)
```

<p class="fragment" style="color: #D14D4D; font-family: 'Lobster Two', cursive;">also: quotes matter, sorry </p>


---

# <center>%>%</center>

## <center>this is the magrittr operator</center>

---

## %>%

<span style="color: #D14D4D">Nesting</span> your dataframe in commands is hard to read


```r
head(iris)
```

```
  Sepal.Length Sepal.Width Petal.Length Petal.Width Species
1          5.1         3.5          1.4         0.2  setosa
2          4.9         3.0          1.4         0.2  setosa
3          4.7         3.2          1.3         0.2  setosa
4          4.6         3.1          1.5         0.2  setosa
5          5.0         3.6          1.4         0.2  setosa
6          5.4         3.9          1.7         0.4  setosa
```

<span style="color: #D14D4D">Piping</span> your dataframe into a command lets you read L to R

```r
iris %>% head(.)
```

```
  Sepal.Length Sepal.Width Petal.Length Petal.Width Species
1          5.1         3.5          1.4         0.2  setosa
2          4.9         3.0          1.4         0.2  setosa
3          4.7         3.2          1.3         0.2  setosa
4          4.6         3.1          1.5         0.2  setosa
5          5.0         3.6          1.4         0.2  setosa
6          5.4         3.9          1.7         0.4  setosa
```



---

## %>%

Sequences of commands are read <span style="color: #D14D4D">inside out</span>


```r
head(iris[iris$Species == "virginica", ])
```

```
    Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
101          6.3         3.3          6.0         2.5 virginica
102          5.8         2.7          5.1         1.9 virginica
103          7.1         3.0          5.9         2.1 virginica
104          6.3         2.9          5.6         1.8 virginica
105          6.5         3.0          5.8         2.2 virginica
106          7.6         3.0          6.6         2.1 virginica
```

Chaining your commands lets you read <span style="color: #D14D4D">L to R</span>

```r
iris %>% filter(Species == "virginica") %>% head(.) 
```

```
  Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
1          6.3         3.3          6.0         2.5 virginica
2          5.8         2.7          5.1         1.9 virginica
3          7.1         3.0          5.9         2.1 virginica
4          6.3         2.9          5.6         1.8 virginica
5          6.5         3.0          5.8         2.2 virginica
6          7.6         3.0          6.6         2.1 virginica
```


---

## Now on to the debates! 

<img src="https://espnfivethirtyeight.files.wordpress.com/2016/02/bialik-debateliveblog0211ratings-1.png" height = 300, width = 600, style="border: 0;">

---

# <center>Wrap-Up</center>

---

# <center>Did you know...</center>
<center>OHSU has a Computer Science education program?</center>

---

## CS @ OHSU

> * Courses 
> * A masters degree program
> * A Ph.D. program
> * Opportunities for applied work related to health

---

## OHSU's CSLU

The Center for Spoken Language Understanding focuses on basic and applied research on algorithms for speech and language technology, imaging and biology, with applications comprising a wide range of biomedical and human-machine interface challenges.

---
## Courses

> * Machine Learning
> * Speech Signal Processing
> * Automatic Speech Recognition
> * Image Processing
> * Natural Language Processing
> * Information Retrieval
> * Cluster Computing
> * Probability & Statistics for Scientists and Engineers

---

## Faculty

* Steven Bedrick (PhD, OHSU): information retrieval, brain computer interface
* Alison Presmanes Hill (PhD, Vanderbilt): computational behavioral science
* Peter Heeman (PhD, U. Rochester): HCI, spoken dialogue systems
* Alexander Kain (PhD, OGI): speech synthesis, voice modification
* Kemal Sönmez (PhD, U. Maryland): computational biology, speech recognition
* Jan van Santen (PhD, U. Michigan): speech analysis, computational behavioral modeling
* Xubo Song (PhD, CalTech): medical imaging, machine learning

---
## Jobs

> * There is an enormous and exponentially growing unmet demand for the skills we teach!

> * Our graduates work at Amazon, AT&T Research, Apple (Siri), the Broad Institute, Google, Intel, Kaiser Permanente, Microsoft Research, Nuance, Sensory, and many other high-tech companies that depend on the speech, language, and image processing we teach

---

## Applying

* Rolling deadline for admissions: talk to us!

* Prerequisite: a bachelors degree in computer science, electrical engineering, mathematics, statistics, linguistics, neuroscience, or related fields

---

<p style="text-align: center; font-family: 'Cabin'; color: #6699DD; font-size: 3em;"><a href="http://www.ohsu.edu/csee">www.ohsu.edu/csee</a></br></p>
<p class="fragment" style="color: #D14D4D; font-family: 'Lobster Two', cursive;">Thank you!</p>

---

# What's in a word?

## Techniques for automating analysis of language

### Steven Bedrick & Alison Presmanes Hill

<div class="list-group" style="font-size: .6em" align = "left">
  <a class="list-group-item" href="mailto:bedricks@ohsu.edu"><i class="fa fa-envelope fa-fw"></i>&nbsp; bedricks@ohsu.edu</a><br>
    <a class="list-group-item" href="mailto:hillali@ohsu.edu"><i class="fa fa-envelope fa-fw"></i>&nbsp; hillali@ohsu.edu</a><br>
  <a class="list-group-item" href="#"><i class="fa fa-map-marker fa-fw"></i>&nbsp; OHSU Gaines Hall 19 & 21 (we're neighbors)</a>
</div>
  
---



<script>
$('ul.incremental li').addClass('fragment')
$('ol.incremental li').addClass('fragment')
</script>
