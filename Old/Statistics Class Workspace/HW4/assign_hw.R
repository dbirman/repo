# Question Assignment Function
#
# Use:
# assign(q_list = c("A","B"))
#
# Dan Birman 10/30/14
#
# http://danbirman.com/assign_hw.R

assign = function(q_list) {
  if (length(q_list) != length(unique(q_list))) {
    stop("All questions must have a unique identifier")
  }
  names = c("Yuan","Daniel","Robert","Arianna","Sophie","Natalia","Dan","Zeynep","Ian","Anna","Cayce","Eric")
  # Mix things up
  names = sample(names)
  q_list = sample(q_list)
  # Add names to have more names than questions (to make things fair)
  reps = length(q_list) / length(names)
  names = rep(names,ceiling(reps))
  # Init variables
  sols = c()
  qi = 1
  # Loop across names
  for (i in 1:length(names)) {
    # For each name give that person a question
    sols = c(sols,q_list[qi])
    qi = qi+1
    # If we run out of questions, copy the question list
    if (qi > length(q_list)) {q_list = sample(q_list); qi = 1}
  }
  # Re-order everything into a data frame
  d = list()
  for (i in 1:length(q_list)) { d[[i]] = names[sols==q_list[i]] }
  ret = data.frame(cbind(q_list,d))
  names(ret) = c("Question","Assigned To")
  ret
}