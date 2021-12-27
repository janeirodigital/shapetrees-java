package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.client.http.HttpClientFactory;
import com.janeirodigital.shapetrees.client.http.HttpRequest;
import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.net.URL;

/**
 * The ShapeTree library uses a generic interface (`HttpClient`) to execute HTTP queries on the POD and for external documents.
 * The OkHttpShapeTreeClient uses the okhttp library to implement `HttpClient`.
 * This factory generates variations of okhttp those clients depending on the need for SSL validation and ShapeTree validation.
 *
 * okhttp documentation (https://square.github.io/okhttp/4.x/okhttp/okhttp3/-ok-http-client/#okhttpclients-should-be-shared)
 * recommends that instance of the client be shared/reused.  OkHttpShapeTreeClientFactory's get() provides an
 * instance of the OkHttpShapeTreeClient which can be re-used for multiple configurations (validation on/off, https verification on/off).
 */
public class OkHttpShapeTreeClientFactory implements HttpClientFactory, ExternalDocumentLoader {
    boolean useSslValidation;
    static final int NON_VALIDATING = 0;
    static final int VALIDATING = 1;
    private static final OkHttpShapeTreeClient[][] okHttpClients = {{null, null}, {null, null}};
    private final BlackWhiteList blackWhiteList;

    /**
     * Construct a factory for OkHttpClients
     *
     * @param useSslValidation
     * @param blackWhiteList
     */
    OkHttpShapeTreeClientFactory(boolean useSslValidation, BlackWhiteList blackWhiteList) {
        this.useSslValidation = useSslValidation;
        this.blackWhiteList = blackWhiteList;
    }

    /**
     * Create or re-use okhttp HttpClient.
     * This fulfils the HttpClientFactory interface, so this factory can be use in
     *   HttpClientFactoryManager.setFactory(new OkHttpShapeTreeClientFactory(...));
     *
     * @param useClientShapeTreeValidation
     * @return a new or existing okhttp HttpClient
     * @throws ShapeTreeException if the OkHttpShapeTreeClient constructor threw one
     */
    @Override
    public OkHttpShapeTreeClient get(boolean useClientShapeTreeValidation) throws ShapeTreeException {
        return getForOptions(this.useSslValidation, useClientShapeTreeValidation);
    }

    /**
     * synchronize setting and getting configured OkHttpClients
     *
     * @param useSslValidation
     * @param useClientShapeTreeValidation
     * @return a new or existing okhttp HttpClient
     * @throws ShapeTreeException if the OkHttpShapeTreeClient constructor threw one
     */
    private static synchronized OkHttpShapeTreeClient getForOptions(boolean useSslValidation, boolean useClientShapeTreeValidation) throws ShapeTreeException {

        int ssl = useSslValidation ? VALIDATING : NON_VALIDATING;
        int shapeTrees = useClientShapeTreeValidation ? VALIDATING : NON_VALIDATING;

        if (okHttpClients[ssl][shapeTrees] != null) {
            return okHttpClients[ssl][shapeTrees];
        }
        try {
            OkHttpShapeTreeClient client = new OkHttpShapeTreeClient(useSslValidation, useClientShapeTreeValidation);
            okHttpClients[ssl][shapeTrees] = client;
            return client;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }

    }

    /**
     * Load a non-POD document
     * This fulfils the ExternalDocumentLoader interface, so this factory can be use in
     *   DocumentLoaderManager.setLoader(new OkHttpShapeTreeClientFactory(...));
     *
     * @param resourceUrl URL of resource to be retrieved
     * @return a DocumentResponse with the results of a successful GET
     * @throws ShapeTreeException if the GET was not successful
     */
    @Override
    public DocumentResponse loadExternalDocument(URL resourceUrl) throws ShapeTreeException {
        if (this.blackWhiteList != null) {
            this.blackWhiteList.check(resourceUrl); }

        DocumentResponse response = this.get(false).fetchShapeTreeResponse(new HttpRequest("GET", resourceUrl, null, null, null));
        if (response.getStatusCode() != 200) { throw new ShapeTreeException(500, "Failed to load contents of document: " + resourceUrl); }
        return response;
    }

}
