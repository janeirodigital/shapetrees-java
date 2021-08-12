package com.janeirodigital.shapetrees.client.http;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

/**
 * Constructs HttpClients based on the passed configuration.
 *
 * <p>See the <a href="{@docRoot}/shapetrees-java-client-http/README#Usage">Usage section</a> in README.
 */
public interface HttpClientFactory {
    /**
     * Reuses or constructs a new HttpClient tailored to the passed configuration
     * @param useShapeTreeValidation whether or not the returned HttpClient must do ShapeTree validation (and Shape validation) on dereferenced resources
     * @return an implementation of HttpClient that can be used to map HTTP library (e.g. OkHttp)
     * requests and responses to `shapetrees-java-client-http` classes.
     */
    public HttpClient get(boolean useShapeTreeValidation) throws ShapeTreeException;
}
