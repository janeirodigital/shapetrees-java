# shapetrees-java-core

shapetrees-java-core provides the core classes to perform shape tree validation in
either client or server-side applications.

Below is a high-level description of the packages in this module to guide
exploration of specific classes:

 * [`contentloaders`](src/main/java/com/janeirodigital/shapetrees/core/contentloaders) - this package contains an 
   interface [`DocumentContentLoader`](src/main/java/com/janeirodigital/shapetrees/core/contentloaders/DocumentContentsLoader.java)
   which is used when shape tree or shape schema resources are required for processing.  The default implementation
   [`HttpDocumentContentsLoader`](src/main/java/com/janeirodigital/shapetrees/core/contentloaders/HttpDocumentContentsLoader.java)
   provides a loader which retrieves these resources from the public web but first checks the requested URI against
   a configured white and/or black list of hosts.
   
 * [`methodhandlers`](src/main/java/com/janeirodigital/shapetrees/core/methodhandlers) - this package contains classes
   which validation standard HTTP methods (GET, POST, PUT, PATCH, DELETE) according to the shape tree specification.
   To see an example of how these method handlers can be invoked, see the [sample client implementation](../shapetrees-java-client-com.janeirodigital.shapetrees.okhttp/src/main/java/com/janeirodigital/shapetrees/client/com.janeirodigital.shapetrees.okhttp/ValidatingShapeTreeInterceptor.java).
