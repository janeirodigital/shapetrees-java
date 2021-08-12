# shapetrees-java-client-http

shapetrees-java-client-http is an (abstract) implementation of `ShapeTreeClient` for HTTP client. For a binding to a concrete HTTP library, see e.g. `shapetrees-java-okhttp`.

The HTTP driver provides two modes of operation:
 1. Client-side validation - (_default_) in this mode all requests from the client are intercepted
    by an OkHttp Interceptor which apply validation through method handlers in [shapetrees-java-core](../shapetrees-java-client-core/README.md)
    
 1. Pass-through - in this mode no client-side validation is performed, assuming that the server will perform that
    validation.
    
## Usage

This module requires a binding to a specific HTTP library (e.g. OkHttp). The binding requires implementation of

 * [`HttpClient`](src/main/java/com/janeirodigital/shapetrees/client/http/HttpClient.java) - HTTP operations to fetch one of ShapeTreeResource, ShapeTreeResponse, or to update a passed RemoteResource.

* [`HttpClientFactory`](src/main/java/com/janeirodigital/shapetrees/client/http/HttpClientFactory.java) - construct or reuse and `HttpClient` tailored to a `HttpShapeTreeClientConfiguration`, which controls SSL and Shape validation.


**User Library Instantiation**

A user library will invoke a variant of

`HttpClientManager.setFactory(new OkHttpClientFactory());`

to set the global HttpClient factory.

**Disable client-side validation**

`client.skipValidation(true);`

## Client-side Validation
Client-side validation is accomplished by intercepting requests from `HttpShapeTreeClient`. For example the <a href="{@docRoot}/shapetrees-java-okhttp/README">shapetrees-java-okhttp</a> uses a native <a href="https://square.github.io/okhttp/interceptors/">OkHttp interceptor</a>. 

## implemententations of [shapetrees-java-core](../shapetrees-java-client-core/README.md) interfaces:

 * [`HttpShapeTreeClient`](src/main/java/com/janeirodigital/shapetrees/client/http/HttpShapeTreeClient.java)
   implements [`ShapeTreeRequest`](../shapetrees-java-core/src/main/java/com/janeirodigital/shapetrees/core/ShapeTreeRequest.java)
   which abstracts the notion of an HTTP request, required by the shape tree validation API.  This interface defines the key
   aspects of an HTTP requests used by shape tree processing an allows different HTTP client implementations
   to be adapted to this common interface.


 * [`HttpRemoteResourceAccessor`](src/main/java/com/janeirodigital/shapetrees/client/http/HttpRemoteResourceAccessor.java)
   implements [`ResourceAccessor`](../shapetrees-java-core/src/main/java/com/janeirodigital/shapetrees/core/ResourceAccessor.java)
   which defines a set of operations used by shape tree validation logic to interact with resources that are 
   used in the validation process.  In this implementation an OkHttp-based client is used to retrieve, create, and update
   resources.
