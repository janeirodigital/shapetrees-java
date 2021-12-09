# shapetrees-java-okhttp

shapetrees-java-client-com.janeirodigital.shapetrees.okhttp is an implementation of `ShapeTreeClient` using OkHttp as the
HTTP client.

The implementation, `OkHttpShapeTreeClient`, provides two modes of operation:
1. Client-side validation - (_default_) in this mode all requests from the client are intercepted
   by an OkHttp Interceptor which apply validation through method handlers in [shapetrees-java-core](../shapetrees-java-client-core/README.md)

1. Pass-through - in this mode no client-side validation is performed, assuming that the server will perform that
   validation.

## Usage

**Instantiation**

``` java
HttpClientManager.setFactory(new OkHttpClientFactory());
this.context = new ShapeTreeContext();
this.shapeTreeClient = new HttpShapeTreeClient();
```

**Disable client-side validation**

`client.skipValidation(true);`

## Client-side Validation
Client-side validation is accomplished by intercepting requests from `OkHttpShapeTreeClient` via
[`ValidatingShapeTreeInterceptor`](src/main/java/com/janeirodigital/shapetrees/client/com.janeirodigital.shapetrees.okhttp/ValidatingShapeTreeInterceptor.java).

One class in this module implements interfaces provided by [shapetrees-java-core](../shapetrees-java-client-core/README.md):

* [`OkHttpClient`](src/main/java/com/janeirodigital/shapetrees/okhttp/OkHttpClient.java)
  implements [`HttpClient`](../shapetrees-java-client-http/src/main/java/com/janeirodigital/shapetrees/client/http/HttpClient.java)
  which abstracts a few functions required by the `shapetrees-java-client-http`.  This interface defines the key
  aspects of an HTTP requests used by shape tree processing an allows different HTTP client implementations
  to be adapted to this common interface.
