@XmlSchema(
   namespace = "http://www.w3.org/2005/Atom",
   elementFormDefault = XmlNsForm.QUALIFIED,
   xmlns = {@XmlNs(
   prefix = "d",
   namespaceURI = "http://schemas.microsoft.com/ado/2007/08/dataservices"
), @XmlNs(
   prefix = "m",
   namespaceURI = "http://schemas.microsoft.com/ado/2007/08/dataservices/metadata"
), @XmlNs(
   prefix = "georss",
   namespaceURI = "http://www.georss.org/georss"
), @XmlNs(
   prefix = "gml",
   namespaceURI = "http://www.opengis.net/gml"
), @XmlNs(
   prefix = "",
   namespaceURI = "http://www.w3.org/2005/Atom"
)}
)
package com.folib.nuget.odata.feed;

import jakarta.xml.bind.annotation.XmlNs;
import jakarta.xml.bind.annotation.XmlNsForm;
import jakarta.xml.bind.annotation.XmlSchema;