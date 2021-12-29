# shapetrees-java

[![Coverage](https://sonarcloud.io/api/project_badges/measure?project=janeirodigital_shapetrees-java&metric=coverage)](https://sonarcloud.io/summary/new_code?id=janeirodigital_shapetrees-java)
[![Maintainability Rating](https://sonarcloud.io/api/project_badges/measure?project=janeirodigital_shapetrees-java&metric=sqale_rating)](https://sonarcloud.io/summary/new_code?id=janeirodigital_shapetrees-java)
[![Reliability Rating](https://sonarcloud.io/api/project_badges/measure?project=janeirodigital_shapetrees-java&metric=reliability_rating)](https://sonarcloud.io/summary/new_code?id=janeirodigital_shapetrees-java)
[![Security Rating](https://sonarcloud.io/api/project_badges/measure?project=janeirodigital_shapetrees-java&metric=security_rating)](https://sonarcloud.io/summary/new_code?id=janeirodigital_shapetrees-java)

A Java (JDK11+) implementation of the [shape tree specification](https://shapetrees.org/TR/specification/index.html).

As detailed in the specification shape tree validation can be performed on either client or server sides.

Each module within the project has its own README.md to further explain its contents:
 * [shapetrees-java-core](shapetrees-java-core/README.md) - Core validation functionality and any classes required
   to interact with the API are present here
 * [shapetrees-java-client-okhttp](shapetrees-java-client-okhttp/README.md) - Provides a shape tree client implemented
   with [OkHttp](https://github.com/square/okhttp/).  This client can perform client-side
   validation by using an interceptor or interact directly with a server that performs server-side validation

