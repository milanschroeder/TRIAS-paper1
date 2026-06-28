### Prepare paragraphs in context (up to 300 tokens) ###

# get para in full context ####
  # detect p in full_text
    # while not unique:
      # p <- p-1:p+1 (allow for other content in between)
    # def p-, p+ 
      # if was not unique: p <- remove p-1 & p+1, p- <- c(p-, p-1), p+ <- c(p+1, p+)) 

# tokenize paragraphs ####
  # if tokens_p > 300:
    # ...

  # else:
    # if tokens_p-1:tokens_p+1 > 300:
      #...
    # else:
      # context <- tokens_p-1:tokens_p+1