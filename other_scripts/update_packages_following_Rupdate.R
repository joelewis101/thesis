# get installed packages ready for install new R version

allmypackages <- as.data.frame(installed.packages())

allmypackages <- allmypackages %>%
  filter(Priority != "base" | is.na(Priority)) %>%
  select(-c(Enhances:MD5sum, LinkingTo:Suggests)) %>%
  droplevels()
str(allmypackages)

WELCOME!
  Here you will find daily news and tutorials about R, contributed by over 750 bloggers. 
There are many ways to follow us - 
  By e-mail:
  Your e-mail here
On Facebook: 
  If you are an R blogger yourself you are invited to add your own R content feed to this site (Non-English R bloggers should add themselves- here)
RSS JOBS FOR R-USERS
Customer Success Representative
Movement Building Analyst
Business Intelligence Analyst
Innovation Fellow
Postdoctoral position Stats, Comp. Biol. @ Madrid, Spain.
RECENT POSTS
Create Animation in R : Learn by Examples
Predicting Car Battery Failure With R And H2O – Study
Practical Data Science with R, half off sale!
  Rstudio & ThinkR roadshow – June 6 – Paris
[R]eady for Production: a Joint Event with RStudio and EODA
Royal Society of Biology: Introduction to Reproducible Analyses in R
Spotlight on: Julia Silge, Stack Overflow
Comparing Frequentist, Bayesian and Simulation methods and conclusions
Analysing the HIV pandemic, Part 4: Classification of lab samples
MRAN snapshots, and you
Deep (learning) like Jacques Cousteau – Part 5 – Vector addition
New Color Palette for R
Easy quick PCA analysis in R
Create a CLI for R with npm
Bug when Creating Reference Maps with Choroplethr
OTHER SITES
Jobs for R-users
SAS blogs
Upgrading to R 3.6.0 on a Mac – May 14, 2019
May 13, 2019
By Chuck Powell
Share
(This article was first published on Chuck Powell, and kindly contributed to R-bloggers)
Share
Tweet
Every time there is a new major update from The R
Foundation (like the recent 3.6.0
            release in April). I’m always happy to see
the continuing progress and the combination of new features and bug
fixes, but I also dread the upgrade because it means I have to address
the issue of what to do about the burgeoning number of packages
(libraries) I have installed.

Up until now I confess I simply have sort of “winged it”, done the
upgrade and either manually thought about what packages I “really”
needed or just grabbed a few essentials and then let my needs dictate
whatever else I reloaded. This time I decided to get serious about the
process and pay attention to not only what I was doing but documenting
it and keeping a record via some amount of coding (and this post).

I’m aware that there are full-fledged package
managers like
packrat and checkpoint and even a package designed to manage the
upgrade for you on windows, but I’m a Mac user and wanted to do things
my own way and I don’t need that level of sophistication.

So I set out to do the following:
  
  Capture a list of everything I had installed under R 3.5.3 and,
very importantly, as much as I could about where I got the package
e.g.  CRAN or GitHub or ???
  Keep a copy for my own edification and potential future use.
Do a clean R 3.6.0 install and not copy any library directories
manually.
Take a look at the list I produced in #1 above but mainly to just
download and install the exact same packages if I can find them.
Make the process mainly scripted and automatic and available again
for the future.
Helpful background
As I was searching the web I found a few helpful posts that saved me
time in building my own solution. The primary was this
post
on Stack Overflow. I wanted to extend the function listed there to do
a little more of my work for me. Instead of just being able to generate
a listing of what I had installed from GitHub I wanted to be able to
determine most of the places I get packages from, which are CRAN,
GitHub and R-Forge.

So let’s load tidyverse to have access to all it’s various functions
and features and then build a dataframe called allmypackages with the
basic information about the packages I currently have installed in R
3.5.3.

Note – I’m writing this after already upgrading so there will be a few
inconsistencies in the output

This could just as easily be a tibble but I chose as.data.frame
I am deliberately removing base packages from the dataframe by
filter
I am eliminating columns I really don’t care about with select
require(tidyverse)
allmypackages <- as.data.frame(installed.packages())
allmypackages <- allmypackages %>%
  filter(Priority != "base" | is.na(Priority)) %>%
  select(-c(Enhances:MD5sum, LinkingTo:Suggests)) %>%
  droplevels()
str(allmypackages)


package_source <- function(pkg){
  x <- as.character(packageDescription(pkg)$Repository)
  if (length(x)==0) {
    y <- as.character(packageDescription(pkg)$GithubRepo)
    z <- as.character(packageDescription(pkg)$GithubUsername)
    if (length(y)==0) {
      return("Other")
    } else {
      return(str_c("GitHub repo = ", z, "/", y))
    }
  } else {
    return(x)
  }
}

allmypackages$whereat <- sapply(allmypackages$Package, package_source)
str(allmypackages)



write.csv(allmypackages, "misc/mypackagelistMay2019.csv" )
