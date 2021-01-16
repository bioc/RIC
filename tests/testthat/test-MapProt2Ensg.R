smalllist<-list(c("A0A183"),c("A0A5B9", "A0AUZ9"),"NA")
smallarraylist<-array(c("ENSG00000235942",NA,"ENSG00000144445" ), c(1,3))
colnames(smallarraylist)<-c("A0A183","A0A5B9","A0AUZ9")
smallarraylist<-list(smallarraylist)
names(smallarraylist)<-"GeneName"

test_that("MapProt2Ensg throws error without valid input", {
  expect_error( MapProt2Ensg(smalllist<-list(list("ENSG00000100836"),list("ENSG00000146574", "ENSG00000122674"),"NA")))
})


test_that("MapProt2Ensg returns a list with ENSG names", {
  expect_true(is.list( MapProt2Ensg(smalllist,smallarraylist )))
  expect_true(is.numeric(grep("ENSG",unlist(MapProt2Ensg(smalllist,smallarraylist)[3]))))
})
