handle_contacts_rdb <- function(config, source){
  
  require(geoflow)
  
  nfi_url <- "http://www.fao.org/fishery"
  nfi_name <- "FAO Fisheries and Aquaculture Division (NFI)"
  nfis_name <- "FAO Fisheries and Aquaculture Division (NFI). Statistics and Information Branch (NFIS)"
  
  contact1 <- geoflow_contact$new()
  contact1$setId("nfi")
  contact1$setOrganizationName(nfi_name)
  contact1$setPostalAddress("Viale delle Terme di Caracalla")
  contact1$setPostalCode("00153")
  contact1$setCity("Rome")
  contact1$setCountry("Italy")
  contact1$setWebsiteUrl(nfi_url)
  contact1$setWebsiteName(nfi_name)
  
  contact2 <- geoflow_contact$new()
  contact2$setId("Aureliano.Gentile@fao.org")
  contact2$setEmail("Aureliano.Gentile@fao.org")
  contact2$setFirstName("Aureliano")
  contact2$setLastName("Gentile")
  contact2$setPositionName("Information Management Officer")
  contact2$setOrganizationName(nfis_name)
  contact2$setWebsiteUrl(nfi_url)
  contact2$setWebsiteName(nfi_name)
  
  contact3 <- geoflow_contact$new()
  contact3$setId("Emmanuel.Blondel@fao.org")
  contact3$setEmail("Emmanuel.Blondel@fao.org")
  contact3$setFirstName("Emmanuel")
  contact3$setLastName("Blondel")
  contact3$setPositionName("Geographic Information Systems and R Expert")
  contact3$setOrganizationName(nfis_name)
  contact3$setWebsiteUrl(nfi_url)
  contact3$setWebsiteName(nfi_name)
  
  contact4 <- geoflow_contact$new()
  contact4$setId("Yann.Laurent@fao.org")
  contact3$setEmail("Yann.Laurent@fao.org")
  contact4$setFirstName("Yann")
  contact4$setLastName("Laurent")
  contact4$setPositionName("Senior Fisheries Information and Statistics System Expert")
  contact4$setOrganizationName(nfis_name)
  contact4$setWebsiteUrl(nfi_url)
  contact4$setWebsiteName(nfi_name)
  
  contact5 <- geoflow_contact$new()
  contact5$setId("James.Geehan@fao.org")
  contact5$setEmail("James.Geehan@fao.org")
  contact5$setFirstName("James")
  contact5$setLastName("Geehan")
  contact5$setPositionName("Senior Fishery statistician")
  contact5$setOrganizationName(nfis_name)
  contact5$setWebsiteUrl(nfi_url)
  contact5$setWebsiteName(nfi_name)
  
  contacts <- list(contact1,contact2,contact3,contact4,contact5)
  return(contacts)
}