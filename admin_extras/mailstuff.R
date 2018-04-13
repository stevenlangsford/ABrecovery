library(mailR)
getpwd <- readline(prompt="mailR password:") #warning, persists in session until mail sent, easy to save an image or something and expose it. Don't do that.

sendMail <- function(amessage){
sender <- "<langsford.steven@gmail.com>"
recipients <- c("<langsford.steven@gmail.com>")
send.mail(from = sender,
          to = recipients,
          subject = "R script",
          body = amessage,
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "langsford.steven",            
                      passwd = getpwd, ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)
rm(getpwd)
}
