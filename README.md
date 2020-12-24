# shapetrees-java

[![codecov](https://codecov.io/gh/janeirodigital/shapetrees-java/branch/master/graph/badge.svg?token=GU1O02A6A5)](https://codecov.io/gh/janeirodigital/shapetrees-java)

shapetrees-java is a Java (JDK11+) implementation of the [shape tree specification](https://shapetrees.org/TR/specification/index.html).

As detailed in the specification shape tree validation can be performed on either client or server sides.

Each module within the project has its own README.md to further explain its contents:
 * [shapetrees-java-core](shapetrees-java-core/README.md) - Core validation functionality and any classes required
   to interact with the API are present here
 * [shapetrees-java-client-core](shapetrees-java-client-core/README.md) - Defines a proposed interface for any shape
   tree client implementations
 * [shapetrees-java-client-okhttp](shapetrees-java-client-okhttp/README.md) - Provides a shape tree client implemented
   with [OkHttp](https://github.com/square/okhttp/).  This client can perform client-side
   validation by using an interceptor or interact directly with a server that performs server-side validation
   
