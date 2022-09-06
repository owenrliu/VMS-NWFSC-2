library(RCurl)
library(XML)


#body of http request, contains parameters to search
body = 
'<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <getVesselSummaryXMLString xmlns="http://cgmix.uscg.mil">
      <VesselID></VesselID>
      <VesselName>CITY OF CHICAGO</VesselName>
      <CallSign></CallSign>
      <VIN></VIN>
      <HIN></HIN>
      <Flag></Flag>
      <Service></Service>
      <BuildYear></BuildYear>
    </getVesselSummaryXMLString>
  </soap:Body>
</soap:Envelope>'

#header value for http request
headerFields =
  c(Accept = "text/xml",
    "Host" = "cgmix.uscg.mil",
    "Content-Type" = "text/xml; charset=utf-8",
    SOAPAction = "http://cgmix.uscg.mil/getVesselSummaryXMLString")

##
headerFields =
  c(Accept = "text/xml",
    "Host" = "cgmix.uscg.mil",
    "Content-Type" = "text/xml; charset=utf-8",
    SOAPAction = "http://cgmix.uscg.mil/getVesselDimensionsXMLString")
body = 
  '<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <getVesselDimensionsXMLString xmlns="http://cgmix.uscg.mil">
      <VesselID>650617</VesselID>
    </getVesselDimensionsXMLString>
  </soap:Body>
</soap:Envelope>
'

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
res$Envelope$Body$getVesselDimensionsXMLStringResponse$getVesselDimensionsXMLStringResult[[1]] %>% xmlToDataFrame()

## RANDOM web searching
# https://www.ssllabs.com/ssltest/analyze.html?d=cgmix.uscg.mil
# https://curl.se/docs/ssl-compared.html
# 

# below works
# dat <- read_xml("https://www.w3schools.com/xml/simple.xml")
# doc <- xmlParse(dat)
# xmlToDataFrame(nodes = getNodeSet(doc, "//food"))
