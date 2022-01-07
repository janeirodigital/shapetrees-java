# Testing client-side validation with OkHttp

We test end-to-end client-side validation with OkHttp using 
[MockWebServer](https://github.com/square/okhttp/tree/master/mockwebserver) to simulate
responses from the resource server (i.e. Solid server). Performing client-side
validation requires additional requests and responses to the resource over the 
course of one validation request, to check whether or not the resource is managed
by one or more shape trees, and what they are.

Consequently, we must create fixtures for these additional responses in many of
the tests herein. Different requests require slightly different fixture sets.


### Create (PUT, PATCH, POST) managed resource

```java
// Must add fixtures to GET the root of the shape tree hierarchy: both the managed resource and shape tree manager
mockOnGet(dispatcher, "/data/projects/", "project/projects-container");
mockOnGet(dispatcher, "/data/projects/.shapetree", "project/projects-container-manager-two-assignments");
// Add fixtures to GET parent container that the resource is being created in
mockOnGet(dispatcher, "/data/projects/project-1/", "project/project-1-container-no-contains");
mockOnGet(dispatcher, "/data/projects/project-1/.shapetree", "project/project-1-container-manager");
// Add fixtures for the various states of the target resource being created
// Target resource - The first GET will return 404, and the second will return the created resource (post-creation) 
mockOnGet(dispatcher, "/data/projects/project-1/milestone-3/", List.of("http/404", "project/milestone-3-container-no-contains"));
// Target resource - Fixture for the successful PUT response when the resource is created
mockOnPut(dispatcher, "/data/projects/project-1/milestone-3/", "http/201");
// Manager resource - The first GET will return 404, and the second will return the assigned shape tree manager (post assignment) 
mockOnGet(dispatcher, "/data/projects/project-1/milestone-3/.shapetree", List.of("http/404", "project/milestone-3-container-manager"));
// Manager resource - Fixture for the successful PUT response when the manager resource is created during assignment
mockOnPut(dispatcher, "/data/projects/project-1/milestone-3/.shapetree", "http/201");
```

### Update (PUT, PATCH) managed resource

```java
    // Add fixture for the target resource itself so that it will be returned on GET and evaluated for update
    mockOnGet(dispatcher, "/data/projects/project-1/milestone-3/", "project/milestone-3-container-no-contains");
    // Add fixture to GET a shape tree manager for the target resource (so it is recognized as managed)
    mockOnGet(dispatcher, "/data/projects/project-1/milestone-3/.shapetree", "project/milestone-3-container-manager");
    // Add fixture for the target resource to return a successful HTTP 204 on PATCH
    mockOnPatch(dispatcher, "/data/projects/project-1/milestone-3/", "http/204");
```

### Plant Shape Tree


### Unplant Shape Tree