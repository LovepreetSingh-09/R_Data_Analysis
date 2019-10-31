library(tidyverse)
library(nycflights13)
library(htmlwidgets)
library(microbenchmark)

str_length(c("a", "R for data science", NA))

# combine strings
str_c("x","y")
str_c("x","y","z",sep=',')
x=c("abc",NA)
str_c('|-',x,'-|')
str_c("|-",str_replace_na(x),"-|")
str_c("prefix-", c("a", "b", "c"), "-suffix")
str_c(c("x", "y", "z"), collapse = ", ")

# sunbsetting strings
x <- c("Apple", "Banana", "Pear")
str_sub(x,1,4)
str_sub(x,-3,-1)
# we can also modify string by str_sub 
str_sub(x,1,1)=str_to_lower(str_sub(x,1,1))
x

# Locales 
str_to_upper(c("i", "ı"))
# using the language code to modify string
str_to_upper(c("i", "ı"),locale='tr')

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")
str_sort(x,locale='haw')

# RegExp
x <- c("apple", "banana", "pear")
str_view(x,'.a.')

# To create the regular expression, we need \\
dot <- "\\."
# But the expression itself only contains one becoz strings explicits \
writeLines(dot)
# We need to use \\ to find a special character. 
# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "a\\.c")
x='a\\b'
# for single \ we need to use 4 \\\\
str_view(x,'\\\\')
x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x,'a$')
x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple$")
str_view(c("grey", "gray"),'gr(e|a)y')
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
# preference is given to the preceeding elemment
str_view(x, "CC?")
str_view(x,'CC+')
str_view(x,'C[LX]+')
str_view(x,'C{2}')
str_view(x,'C{2,3}')
# RegExp shows greedy behaviour means takes the longest streak of match
# To find the short match we can use ? after the expression
str_view(x,'C[LX]+?')

# backrefrences
# finds all fruits that have a repeated pair of letters
fruit
str_view(fruit, "(..)\\1", match = TRUE)
str_match(fruit,'(..)\\1')

# str_detect gives the output as TRUE/FALSE for each string where expression matches
x<- c("apple", "banana", "pear")
str_detect(x, "e")
# 1000 common words
words
sum(str_detect(words,'^t'))
mean(str_detect(words,'[aeiou]$'))
no_vowels_1 <- !str_detect(words, "[aeiou]")
no_vowels_2 <- str_detect(words,'^[^aeiou]+$')
identical(no_vowels_1,no_vowels_2) # True

words[str_detect(words,'x$')]
# This is same as
str_subset(words,'x$')
df <- tibble(
  word = words,
  i = seq_along(word))
df
df %>% filter(str_detect(word,'x$'))

# str_count for counting the matching of expression in each string and returns a list
x <- c("apple", "banana", "pear")
str_count(x,'a')
mean(str_count(words, "[aeiou]"))
df %>% mutate(vowels=str_count(word,'[aeiou]'),
              consonants=str_count(word,'[^aeiou]'))
str_count("abababa", "aba")
str_view('abababa','aba')
# _all behind every string function matches every pattern in a string while others matches just the first one.
# also the matches do not overlap like the following one
str_view_all('abababa','aba')

length(sentences) # 720
head(sentences)
colors <- c(
  "red", "orange", "yellow", "green", "blue", "purple")
color_match <- str_c(colors, collapse = "|")
# making a regular expression
color_match
subset <- str_subset(sentences,color_match)
str_extract(subset,color_match)

str_count(sentences, color_match) 
more <- sentences[str_count(sentences, color_match) > 1]
more
str_extract(more,color_match)
str_extract_all(more,color_match)
# simplify=TRUE makes a matrix 
str_extract_all(more,color_match,simplify = TRUE)
# the matrix's no of columns are equal to the maximum no. of matches in any string
# for less matches in other documents there is blank
x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)

noun='(the|a) ([^ ]+)'
has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun
has_noun %>% str_extract(noun)

# str_match() makes matrix where 1st column is the match and the later are the split or parts of the match
has_noun %>% str_match(noun)

# remove = FALSE includes the string in the tibble
tibble(sentence=sentences) %>%
  tidyr::extract(sentence,c('article','noun'),noun,remove=FALSE)

x <- c("apple", "pear", "banana")
str_replace(x,'[aeiou]','-')
# _all replaces all
str_replace_all(x,'[aeiou]','-')
x <- c("1 house", "2 cars", "3 people")
str_replace_all(x,c('1'='one','2'='two','3'='three'))
sentences %>% head(5)
# flipping the 3rd and 2nd word of string 
sentences %>%
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>%
  head(5)
sentences %>%
  head(5) %>%
  str_split(" ")

"a|b|c|d" %>%
  str_split('\\|') %>% .[[1]] # .[[1]] makes a single vector instead of list of list

sentences %>%
  head(5) %>%
  str_split(" ", simplify = TRUE)

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE) # n specify the no. of columns 

x <- "This is a sentence. This is another sentence."
str_view_all(x, boundary("word"))
str_split(x, boundary("word"))[[1]]
str_view(fruit, regex("nana"))

# multiline = TRUE allows ^ and $ to match the start and end of
# each line rather than the start and end of the complete string
x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]

phone <- regex("
\\(? # optional opening parens
(\\d{3}) # area code
[)- ]? # optional closing parens, dash, or space
(\\d{3}) # another three numbers
[ -]? # optional space or dash
(\\d{3}) # three more numbers
", comments = TRUE)

str_match("514-791-8141", phone)

# fixed() matches exactly the specified sequence of bytes. It ignores all special regular expressions
# and operates at a very low level. This allows you to avoid complex escaping and can be
# much faster than regular expressions.
microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20)

a1 <- "\u00e1"
a2 <- "a\u0301"
c(a1, a2)
a1 == a2

str_detect(a1, fixed(a2)) # FALSE

# coll() compares strings using standard collation rules.
# coll() takes a locale parameter that controls which rules are used for comparing characters.
str_detect(a1, coll(a2)) # TRUE
i <- c("I", "İ", "i", "ı")
i
str_subset(i, coll("i", ignore_case = TRUE))
str_subset( i, coll("i", ignore_case = TRUE, locale = "tr") )

stringi::stri_locale_info()

# apropos() searches all objects available from the global environment.
apropos("count")

# dir() lists all the files in a directory.
head(dir(pattern = "\\.Rmd$"))


