package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import com.janeirodigital.shapetrees.core.models.ShapeTreeContext;
import com.janeirodigital.shapetrees.core.models.ShapeTreeLocator;
import lombok.extern.slf4j.Slf4j;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
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
    final protected boolean _createFromMetadata;

    // components
    private Optional<UserOwned> userOwnedResource = Optional.empty();
    private Optional<Metadata> metadataResource = Optional.empty();

    // simple getters
    public boolean createdFromMetadata() { return this._createFromMetadata; }
    public ShapeTreeContext getShapeTreeContext() { return this._shapeTreeContext; }

    // constructors
    private ShapeTreeResource(URI uri, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext, Fork str) {
        this._resourceAccessor = resourceAccessor;
        this._shapeTreeContext = shapeTreeContext;
        if (str instanceof Metadata) {
            this._createFromMetadata = true;
            this.metadataResource = Optional.of((Metadata) str);
        } else {
            this._createFromMetadata = false;
            this.userOwnedResource = Optional.of((UserOwned) str);
        }
    }
    public ShapeTreeResource(URI uri, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext) throws ShapeTreeException {
        this(uri, resourceAccessor, shapeTreeContext, resourceAccessor.getResource(shapeTreeContext, uri));
    }
    public ShapeTreeResource(URI uri, ResourceAccessor resourceAccessor, ShapeTreeContext shapeTreeContext, ShapeTreeRequest shapeTreeRequest) throws ShapeTreeException {
        this(uri, resourceAccessor, shapeTreeContext, resourceAccessor.createResource(shapeTreeContext, shapeTreeRequest.getMethod(), uri, shapeTreeRequest.getHeaders(), shapeTreeRequest.getBody(), shapeTreeRequest.getContentType()));
    }

    // Get resource forks
    public UserOwned getUserOwnedResourceFork() throws ShapeTreeException {
        UserOwned uor;
        if (this.userOwnedResource.isEmpty()) {
            Metadata mr = this.metadataResource.orElseThrow(unintialized_resourceFork);
            /* TODO: @see https://github.com/xformativ/shapetrees-java/issues/86
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
//                throw new ShapeTreeException(500, "No link headers in metadata resource <" + mr.uri + ">");
//            }
            URI uri = mr.getUserOwnedResourceUri();
            Fork str = this._resourceAccessor.getResource(this._shapeTreeContext, uri);
            if (str instanceof UserOwned) {
                this.userOwnedResource = Optional.of(uor = (UserOwned) str);
            } else {
                throw new IllegalStateException("Dereferencing <" + uri + "> did not yield a UserOwned");
            }
        } else {
            uor = this.userOwnedResource.get();
        }
        return uor;
    }

    public Metadata getMetadataResourceFork() throws ShapeTreeException {
        Metadata mr;
        if (this.metadataResource.isEmpty()) {
//            UserOwned uor = this.userOwnedResource.orElseThrow(unintialized_resourceFork);
//            if (... no shapeTreeMetadataURIForResource ...) {
//                throw new ShapeTreeException(500, "No link headers in user-owned resource <" + uor.uri + ">");
//            }
            final URI uri = this.getShapeTreeMetadataURIForResource();
            Fork str = this._resourceAccessor.getResource(this._shapeTreeContext, uri);
            if (str instanceof Metadata) {
                this.metadataResource = Optional.of(mr = (Metadata) str);
            } else {
                throw new IllegalStateException("Dereferencing <" + uri + "> did not yield a UserOwned");
            }
        } else {
            mr = this.metadataResource.get();
        }
        return mr;
    }

    protected URI getShapeTreeMetadataURIForResource() throws ShapeTreeException {
        UserOwned uor = this.userOwnedResource.orElseThrow(unintialized_resourceFork);
        final List<String> linkHeaderValues = uor.attributes.allValues(HttpHeaders.LINK.getValue());
        ResourceAttributes linkHeaders = ResourceAttributes.parseLinkHeaders(linkHeaderValues);

        if (linkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).isEmpty()) {
            log.error("The resource {} does not contain a link header of {}", uor.uri, LinkRelations.SHAPETREE_LOCATOR.getValue());
            throw new ShapeTreeException(500, "The resource <" + uor.uri + "> has no Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
        }
        String metaDataURIString = linkHeaders.firstValue(LinkRelations.SHAPETREE_LOCATOR.getValue()).orElseThrow(
                () -> new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found")
        );
        try {
            final URL base = new URL(uor.uri.toString());
            final URL resolved = new URL(base, metaDataURIString);
            return URI.create(resolved.toString());
        } catch (MalformedURLException e) { // TODO: vet this
            // throw new ShapeTreeException(500, "No Link header with relation of " + LinkRelations.SHAPETREE_LOCATOR.getValue() + " found");
            // If we can't do relative URL resolution, assume that the locator is a URI and we have some other means of resolving it.
            return URI.create(metaDataURIString);
        }
    }

    public void createOrUpdateMetadataResource(ShapeTreeLocator primaryResourceLocator) throws ShapeTreeException, URISyntaxException {
        Metadata primaryMetadataResource = this.getMetadataResourceFork();
        if (!primaryMetadataResource.isExists()) {
            // create primary metadata resource if it doesn't exist
            ResourceAttributes headers = new ResourceAttributes();
            headers.setAll(HttpHeaders.CONTENT_TYPE.getValue(), Collections.singletonList(TEXT_TURTLE));
            this._resourceAccessor.createResource(this._shapeTreeContext,"POST", primaryMetadataResource.uri, headers, primaryResourceLocator.getGraph().toString(), TEXT_TURTLE);
        } else {
            // Update the existing metadata resource for the primary resource
            this._resourceAccessor.updateResource(this._shapeTreeContext, "PUT", primaryMetadataResource, primaryResourceLocator.getGraph().toString());
        }
        // If we decide to make this mutable, we could parse the DocumentResponse (or a subsequent GET in case of non-200 success)
        // this._init(primaryMetadataResource.uri, new ShapeTreeResource999(primaryMetadataResource.uri, res));
    }

    static final Supplier<IllegalStateException> unintialized_resourceFork = () -> new IllegalStateException("unintialized Fork");

    static public class Fork { // TODO: abstract with helpful toString() for error messages
        final protected URI uri;
        final protected ShapeTreeResourceType resourceType;
        final protected ResourceAttributes attributes;
        final protected String body;
        final protected String name;
        final protected boolean exists;

        Fork(URI uri, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists) {
            this.uri = uri;
            this.resourceType = resourceType;
            this.attributes = attributes;
            this.body = body;
            this.name = name;
            this.exists = exists;
        }

        public URI getUri() {
            return this.uri;
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

    static public class UserOwned extends Fork {
        final protected Optional<URI> metadataResourceUri;
        final protected boolean _container;

        public UserOwned(URI uri, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists, Optional<URI> metadataResourceUri, boolean isContainer) {
            super(uri, resourceType, attributes, body, name, exists);
            this.metadataResourceUri = metadataResourceUri;
            this._container = isContainer;
        }

        public Optional<URI> getMetadataResourceUri() {
            return this.metadataResourceUri;
        }
        public boolean isContainer() {
            return this._container;
        }
    }

    static public class Metadata extends Fork {
        final protected URI userOwnedResourceUri;
        // TODO: move graph to Fork for getContainedResources?
        // Or keep here, make this one non-Optional, and fix test harness to have RDF Content-Type. (what about 404?)

        public Metadata(URI uri, ShapeTreeResourceType resourceType, ResourceAttributes attributes, String body, String name, boolean exists, URI userOwnedResourceUri) {
            super(uri, resourceType, attributes, body, name, exists);
            this.userOwnedResourceUri = userOwnedResourceUri;
        }

        public URI getUserOwnedResourceUri() {
            return this.userOwnedResourceUri;
        }
    }
}
