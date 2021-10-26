package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import lombok.extern.slf4j.Slf4j;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

@Slf4j
public class ShapeTreeResource {
    public static final String TEXT_TURTLE = "text/turtle";

    // access parameters
    final protected ResourceAccessor _resourceAccessor;
    final protected ShapeTreeContext _shapeTreeContext;
    final protected boolean _wasCreateFromMetadata;

    // components
    private Optional<Primary> userOwnedResource = Optional.empty();
    private Optional<Metadata> metadataResource = Optional.empty();

    // simple getters
    public boolean wasCreatedFromMetadata() { return this._wasCreateFromMetadata; }
    public ShapeTreeContext getShapeTreeContext() { return this._shapeTreeContext; }

    // constructors
    private ShapeTreeResource(URL url, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext, Fork str) {
        this._resourceAccessor = resourceAccessor;
        this._shapeTreeContext = shapeTreeContext;
        if (str instanceof Metadata) {
            this._wasCreateFromMetadata = true;
            this.metadataResource = Optional.of((Metadata) str);
        } else {
            this._wasCreateFromMetadata = false;
            this.userOwnedResource = Optional.of((Primary) str);
        }
    }
    public ShapeTreeResource(URL url, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext) throws ShapeTreeException {
        this(url, resourceAccessor, shapeTreeContext, resourceAccessor.getResource(shapeTreeContext, url));
    }
    public ShapeTreeResource(URL url, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
        this(url, resourceAccessor, shapeTreeContext, resourceAccessor.createResource(shapeTreeContext, shapeTreeRequest.getMethod(), url, shapeTreeRequest.getHeaders(), shapeTreeRequest.getBody(), shapeTreeRequest.getContentType()));
    }

    // Get resource forks
    public Primary getUserOwnedResourceFork() throws ShapeTreeException {
        Primary uor;
        if (this.userOwnedResource.isEmpty()) {
            Metadata mr = this.metadataResource.orElseThrow(unintialized_resourceFork);
            /* TODO: #86 @see https://github.com/xformativ/shapetrees-java/issues/86
MedicalRecordTests
  plantConditionShapeTree()
  plantMedicalRecord()
ProjectTests
  unplantData()
  failPlantOnMissingShapeTree()
  plantDataRepositoryWithPatch()
  plantSecondShapeTreeOnProjects()
  updateProjectsLocatorWithPatch()
  unplantProjects()
  plantDataRepository()
ProjectRecursiveTests
  plantDataRecursively()
  plantProjectsRecursively()
            */
//            if (... no userOwnedResourceUri ...) {
//                throw new ShapeTreeException(500, "No link headers in metadata resource <" + mr.url + ">");
//            }
            URL url = mr.getUserOwnedResourceUrl();
            Fork str = this._resourceAccessor.getResource(this._shapeTreeContext, url);
            if (str instanceof Primary) {
                this.userOwnedResource = Optional.of(uor = (Primary) str);
            } else {
                throw new IllegalStateException("Dereferencing <" + url + "> did not yield a Primary");
            }
        } else {
            uor = this.userOwnedResource.get();
        }
        return uor;
    }

    public Metadata getMetadataResourceFork() throws ShapeTreeException {
        Metadata mr;
        if (this.metadataResource.isEmpty()) {
//            Primary uor = this.userOwnedResource.orElseThrow(unintialized_resourceFork);
//            if (... no shapeTreeMetadataUrlForResource ...) {
//                throw new ShapeTreeException(500, "No link headers in user-owned resource <" + uor.url + ">");
//            }
            final URL url = this.getShapeTreeMetadataUrlForResource();
            Fork str = this._resourceAccessor.getResource(this._shapeTreeContext, url);
            if (str instanceof Metadata) {
                this.metadataResource = Optional.of(mr = (Metadata) str);
            } else {
                throw new IllegalStateException("Dereferencing <" + url + "> did not yield a Primary");
            }
        } else {
            mr = this.metadataResource.get();
        }
        return mr;
    }

    protected URL getShapeTreeMetadataUrlForResource() throws ShapeTreeException {
        Primary uor = this.userOwnedResource.orElseThrow(unintialized_resourceFork);
        final List<String> linkHeaderValues = uor.attributes.allValues(HttpHeaders.LINK.getValue());
        ResourceAttributes linkHeaders = ResourceAttributes.parseLinkHeaders(linkHeaderValues);

        final URL base = uor.url;
        if (linkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).isEmpty()) {
            log.error("The resource {} does not contain a link header of {}", base, LinkRelations.SHAPETREE_LOCATOR.getValue());
            throw new ShapeTreeException(500, "The resource <" + base + "> has no Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
        }
        String metaDataUrlString = linkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).orElseThrow(
                () -> new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found")
        );
        try {
            return new URL(base, metaDataUrlString);
        } catch (MalformedURLException e) { // TODO: ACTION: ericP to migrate everything to URLs
            // throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
            // If we can't do relative URL resolution, assume that the locator is a URL and we have some other means of resolving it.
            try {
                return new URL(metaDataUrlString);
            } catch (MalformedURLException ex) {
                throw new IllegalStateException("Malformed relative URL <" + metaDataUrlString + "> (resolved from <" + base + ">)");
            }
        }
    }

    public void createOrUpdateMetadataResource(ShapeTreeLocator primaryResourceLocator) throws ShapeTreeException {
        Metadata primaryMetadataResource = this.getMetadataResourceFork();
        if (!primaryMetadataResource.isExists()) {
            // create primary metadata resource if it doesn't exist
            ResourceAttributes headers = new ResourceAttributes();
            headers.setAll(HttpHeaders.CONTENT_TYPE.getValue(), Collections.singletonList(TEXT_TURTLE));
            this._resourceAccessor.createResource(this._shapeTreeContext,"POST", primaryMetadataResource.url, headers, primaryResourceLocator.getGraph().toString(), TEXT_TURTLE);
        } else {
            // Update the existing metadata resource for the primary resource
            this._resourceAccessor.updateResource(this._shapeTreeContext, "PUT", primaryMetadataResource, primaryResourceLocator.getGraph().toString());
        }
    }

    static final Supplier<IllegalStateException> unintialized_resourceFork = () -> new IllegalStateException("unintialized Fork");

    static public class Fork {
        final protected URL url;
        final protected ShapeTreeResourceType resourceType;
        final protected ResourceAttributes attributes;
        final protected String body;
        final protected String name;
        final protected boolean exists;

        Fork(URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists) {
            this.url = url;
            this.resourceType = resourceType;
            this.attributes = attributes;
            this.body = body;
            this.name = name;
            this.exists = exists;
        }

        public URL getUrl() {
            return this.url;
        }
        public ShapeTreeResourceType getResourceType() {
            return this.resourceType;
        }
        public ResourceAttributes getAttributes() {
            return this.attributes;
        }
        public String getBody() {
            return this.body;
        }
        public String getName() {
            return this.name;
        }
        public boolean isExists() {
            return this.exists;
        }
    }

    static public class Primary extends Fork {
        final protected Optional<URL> metadataResourceUrl;
        final protected boolean _container;

        public Primary(URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists, Optional<URL> metadataResourceUrl, boolean isContainer) {
            super(url, resourceType, attributes, body, name, exists);
            this.metadataResourceUrl = metadataResourceUrl;
            this._container = isContainer;
        }

        public Optional<URL> getMetadataResourceUrl() {
            return this.metadataResourceUrl;
        }
        public boolean isContainer() {
            return this._container;
        }
    }

    static public class Metadata extends Fork {
        final protected URL userOwnedResourceUrl;

        public Metadata(URL url, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists, URL userOwnedResourceUrl) {
            super(url, resourceType, attributes, body, name, exists);
            this.userOwnedResourceUrl = userOwnedResourceUrl;
        }

        public URL getUserOwnedResourceUrl() {
            return this.userOwnedResourceUrl;
        }
    }
}
