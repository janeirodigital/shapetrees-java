package com.janeirodigital.shapetrees.client.fetch;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;

import java.util.concurrent.ConcurrentHashMap;

// !! DELME - no need for this to be outside of OkHttpFetcher

/**
 * OkHttp documentation (https://square.github.io/okhttp/4.x/okhttp/okhttp3/-ok-http-client/#okhttpclients-should-be-shared)
 * recommends that instance of the client be shared/reused.  This class acts as a point of abstraction where a single
 * instance of the OkHttpClient can be re-used for multiple configurations (validation on/off, https verification on/off).
 *
 * A static map of client references are managed per configuration which can be easily retrieved
 */
public class ShapeTreeHttpClientHolder {
    private ShapeTreeHttpClientHolder() {
    }

    private static final ConcurrentHashMap<ShapeTreeClientConfiguration, OkHttpFetcher> clientMap = new ConcurrentHashMap<>();

    /**
     * Gets an OkHttpFetcher for a given configuration.  Looks up an instance from the private static clientMap.
     * If a client hasn't yet been initialized for a given configuration it is built and added to the cache.
     * @param configuration ShapeTreeClientConfiguration to retrieve a client for
     * @return OkHttpFetcher instance for use
     * @throws ShapeTreeException ShapeTreeException
     */
    public static synchronized OkHttpFetcher getForConfig(ShapeTreeClientConfiguration configuration) throws ShapeTreeException {
        if (clientMap.containsKey(configuration)) {
            return clientMap.get(configuration);
        }
        try {
            OkHttpFetcher client = new OkHttpFetcher(configuration);
            clientMap.put(configuration, client);
            return client;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }
}
