context("ggteho")

test_that("ggetho works well with a variety of expressions", {
  metadata <- data.table(id= sprintf("toy_experiment|%02d", 1:20),
                    condition=c("A","B"))
  dt <- toy_activity_data(metadata, 3)
  pl <- ggetho(dt, aes(y=moving))
  pl
  pl <- ggetho(dt, aes(y=1-moving ))
  pl
  pl <- ggetho(dt, aes(y=!moving ))
  pl


  pl <- ggetho(dt, aes(z=moving))
  pl
  pl <- ggetho(dt, aes(z=1-moving ))
  pl
  pl <- ggetho(dt, aes(z=!moving ))
  pl

  # sorting with paste
  pl <- ggetho(dt, aes(z=moving,y=paste(condition, id)))
  pl

  pl <- ggetho(dt, aes(z=moving, y=paste(condition, id)))
  pl
})
