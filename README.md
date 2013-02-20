# SOAP

The `soap` package provides basic tools to make SOAP queries.
The specs are very complex and abstract, so it's only a thin layer
over `http-conduit` and `xml-conduit` to create a request,
send it and get a response.

Web.SOAP.Service
----------------

`invokeWS` is the method to run queries. It takes following arguments:

  * `SOAPSettings` - service options (HTTP URL, namespace and iconv codepage).
  * `Text` - SOAPAction HTTP header.
  * Request headers of a typeclass `ToNodes` (see below).
  * Request body of a typeclass `ToNodes`.

A result of an operation will be a needed type extracted from an XML response using `FromCursor` class.

    reply <- invokeWS
                 (SOAPSettings "http://localhost/ws" "urn:test" "utf-8")
                 "urn:test:EchoQuery"
                 () -- empty SOAP header
                 (EchoQuery "hellow, orld!")

Web.SOAP.Types
--------------

This module has the type classes used with `invokeWS` and some helpers to deal with XML data.

**ToNodes**

This class provides a main `toNodes` function that can use two levels of helpers to construct XML nodes from a `Element` list (`toElements`) which, in turn, can use a single-`Element` helper `toElement`.

Additionally there is a couple of infix constructors that, with a help from `OverloadedStrings` can build `ToNodes` instances with ease.

Use `.=` to construct a text node and wrap a list of nodes into element with `.=:`, like this:

    newtype EchoQuery = EchoQuery Text
    
    instance ToNodes EchoQuery where
        toNodes (EchoQuery t) = "{urn:test}EchoQuery" .=: [ "Echo" .= t ]

**FromCursor**

The class has only one function `fromCursor` that receives a `Cursor` to the entire response document. To facilitate data extraction, there are some helpers available to be used with `Text.XML.Cursor` from `xml-conduit`.

To extract an child element content at cursor as `Text` use *readT*. Any data that has a `Read` typeclass implemented can be extracted with *readC*.

    newtype EchoResponse = EchoResponse Text
    
    instance FromCursor EchoResponse where
        fromCursor c = EchoResponse (readT "Echo" $ c $/ laxElement "EchoQueryResponse")

# SOAP-WSDL

TBD
