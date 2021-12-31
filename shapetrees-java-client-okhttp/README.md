# shapetrees-java-okhttp

Client-side shape-tree validation for the [OkHttp](https://square.github.io/okhttp/) client-library. 

The implementation provides two modes of operation:
1. Client-side validation - (_default_) in this mode all requests from the client are intercepted
   by an OkHttp Interceptor which apply validation through method handlers in [shapetrees-java-core](../shapetrees-java-core/README.md)

1. Pass-through - in this mode no client-side validation is performed, assuming that the server will perform that
   validation.

Client-side validation is accomplished by intercepting requests from `OkHttpShapeTreeClient` via
[`ValidatingShapeTreeInterceptor`](src/main/java/com/janeirodigital/shapetrees/client/com.janeirodigital.shapetrees.okhttp/ValidatingShapeTreeInterceptor.java).
