library(RCurl)
library(XML)
library(xml2)
library(tidyverse)

## NEXT ISSUES: MULTIPLE REQUESTS, HOW TO USE STRING IDS INSTEAD OF JUST INTEGER (arghhh)
testids <- read_csv(here::here('data','raw','vessel registration','vessel registration 2018-2019 121819.csv')) %>% 
  pull('VESSEL_NUM') %>% head()

##
# headerFields =
#   c(Accept = "text/xml",
#     "Host" = "cgmix.uscg.mil",
#     "Content-Type" = "text/xml; charset=utf-8",
#     SOAPAction = "http://cgmix.uscg.mil/getVesselDimensionsXMLString")
# body = 
#   '<?xml version="1.0" encoding="utf-8"?>
# <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
#   <soap:Body>
#     <getVesselDimensionsXMLString xmlns="http://cgmix.uscg.mil">
#       <VesselID>OR351TN</VesselID>
#     </getVesselDimensionsXMLString>
#   </soap:Body>
# </soap:Envelope>
# '


body = 
  '<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <getVesselParticularsXMLString xmlns="http://cgmix.uscg.mil">
      <VesselID>int</VesselID>
    </getVesselParticularsXMLString>
  </soap:Body>
</soap:Envelope>'

#header value for http request
headerFields =
  c(Accept = "text/xml",
    "Host" = "cgmix.uscg.mil",
    "Content-Type" = "text/xml; charset=utf-8",
    SOAPAction = "http://cgmix.uscg.mil/getVesselParticularsXMLString")

reader = basicTextGatherer()

#curl with HTTP request
getURLContent(url = "https://cgmix.uscg.mil/xml/PSIXData.asmx",
              httpheader = headerFields,
              postfields = body,
              ssl.verifyhost=FALSE,
              timeout=4,
              useragent= "R",
              writefunction = reader$update,
              verbose=TRUE
)
#Convert and save XML file
xmlval <- reader$value()
# (xml <- xmlParse(xmlval))
# saveXML(xml, file = "boatdata.xml")

res <- read_xml(xmlval) %>% as_list()

# this works (holy shit)
res$Envelope$Body$getVesselSummaryXMLStringResponse$getVesselSummaryXMLStringResult[[1]] %>% xmlToDataFrame()
