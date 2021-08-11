# shapetrees-java-client-http

shapetrees-java-client-http is an (abstract) implementation of `ShapeTreeClient` for HTTP client. For a binding to a concrete HTTP library, see e.g. `shapetrees-java-okhttp`.

The HTTP driver provides two modes of operation:
 1. Client-side validation - (_default_) in this mode all requests from the client are intercepted
    by an OkHttp Interceptor which apply validation through method handlers in [shapetrees-java-core](../shapetrees-java-client-core/README.md)
    
 1. Pass-through - in this mode no client-side validation is performed, assuming that the server will perform that
    validation.
    
## Usage

**Instantiation**

`HttpClientManager.setFactory(new OkHttpClientFactory());`

**Disable client-side validation**

`client.skipValidation(true);`

## Client-side Validation
Client-side validation is accomplished by intercepting requests from `HttpShapeTreeClient`.

Several classes in this module implement interfaces provided by [shapetrees-java-core](../shapetrees-java-client-core/README.md):

 * [`HttpClient`](src/main/java/com/janeirodigital/shapetrees/client/http/HttpClient.java)
   implements [`ShapeTreeRequest`](../shapetrees-java-core/src/main/java/com/janeirodigital/shapetrees/core/ShapeTreeRequest.java)
   which abstracts the notion of an HTTP request, required by the shape tree validation API.  This interface defines the key
   aspects of an HTTP requests used by shape tree processing an allows different HTTP client implementations
   to be adapted to this common interface.


 * [`HttpRemoteResourceAccessor`](src/main/java/com/janeirodigital/shapetrees/client/http/HttpRemoteResourceAccessor.java)
   implements [`ResourceAccessor`](../shapetrees-java-core/src/main/java/com/janeirodigital/shapetrees/core/ResourceAccessor.java)
   which defines a set of operations used by shape tree validation logic to interact with resources that are 
   used in the validation process.  In this implementation an OkHttp-based client is used to retrieve, create, and update
   resources.
