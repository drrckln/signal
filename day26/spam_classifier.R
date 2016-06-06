library("hash")

# Create dictionary to check email label - spam (0) or ham (1)
setwd("/home/derrick/signal/day26")
classifiction = hash()
email_labels = readLines(file("SPAMTrain.label", open="r"))

for (l in email_labels) {
  l = unlist(strsplit(l, "[ ]"))
  lbl = l[1]
  name = l[2]
  classifiction[[name]] = as.numeric(lbl)
}


# Grab and calculate frequencies from training corpus
setwd("~/signal/day26/TRAINING")
training_paths = list.files()

spam = hash()
ham = hash()

for (f in training_paths) {
  words = scan(file=f, what=character())
  if (classifiction[[f]] == 1) {
    to_update = ham
  } else {
    to_update = spam
  }
  # could use table! also should unique
  for (word in unique(tolower(words))) {
    if (word == "") {
      print("augh, no word")
    } else if (has.key(word, to_update)) {
      to_update[[word]] = to_update[[word]] + 1
    } else {
      to_update[[word]] = 1
    }
  }
}

# Calculate p_i = P(S | W_i), for all W
prob_i = hash()

words = union(keys(ham), keys(spam))
for (word in words) {
  s = if (is.null(spam[[word]])) 1 else spam[[word]] 
  h = if (is.null(ham[[word]])) 1 else spam[[word]]
  prob_i[[word]] = s / (s + h)
}

# String -> Spam? P(S) > 1/2
detect_spam = function(s) {
  pieces = unique(tolower(unlist(strsplit(s, "[ ]"))))
  rhs = sum(unlist(sapply(pieces, function(w) { if (w == "" || is.null(prob_i[[w]])) 0.5 else log(1-prob_i[[w]]) - log(prob_i[[w]]) })))
  p_i = 1 / (exp(rhs) + 1)
  return(p_i)
}

# test if it works
# setwd("~/signal/day26/TESTING/")
# testing_paths = list.files()

test_s = scan(file=training_paths[340], what=character())
detect_spam(test_s)


# Which words have the highest and lowest p_i?
sorted_words = words[order(unlist(sapply(words, function(x) { prob_i[[x]] })), decreasing = TRUE)]
# highest (actual?) words (likely indicates ham)
# "CTO", "one-key", "one-liners", "simple", "simplifies", "meeting"
# lowest words
# "spans", "spanked", "Spanish", variations on "spam"

# Tried it on a spam email, got back 0.1141781, so classified as spam :)
# Tried it on a real email, got back 0.1824255, so classified as spam :( (tried 3. shit...)
# Oh, I'd misinterpreted prob_i..

t_p = 0
t_n = 0
f_p = 0
f_n = 0
total = 0
for (eml in training_paths) {
  eml_class = if (detect_spam(scan(file=eml, what=character())) > 0.5) 0 else 1
  total = total + 1
  if (eml_class == 1 && classifiction[[eml]] == 1) {
    t_p = t_p + 1
  } else if (eml_class == 1 && classifiction[[eml]] == 0) {
    f_p = f_p + 1
  } else if (eml_class == 0 && classifiction[[eml]] == 1) {
    f_n = f_n + 1
  } else {
    t_n = t_n + 1
  }
}

results = data.frame(t_p, t_n, f_p, f_n) / total
#         t_p       t_n         f_p        f_n
# 1 0.6702103 0.3182343 0.000231107 0.01132424

# what if you convert all to lowercase?
#         t_p t_n       f_p f_n
# 1 0.6815346   0 0.3184654   0
# weird everything got classed as positive..