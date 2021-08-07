package com.janeirodigital.shapetrees.client.fetch;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import okhttp3.OkHttpClient;

import javax.net.ssl.*;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.concurrent.ConcurrentHashMap;

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

    private static final OkHttpClient baseClient = new OkHttpClient();
    private static final ConcurrentHashMap<ShapeTreeClientConfiguration, OkHttpClient> clientMap = new ConcurrentHashMap<>();

    /**
     * Gets an OkHttpClient for a given configuration.  Looks up an instance from the private static clientMap.
     * If a client hasn't yet been initialized for a given configuration it is built and added to the cache.
     * @param configuration ShapeTreeClientConfiguration to retrieve a client for
     * @return OkHttpClient instance for use
     * @throws ShapeTreeException ShapeTreeException
     */
    public static synchronized OkHttpClient getForConfig(ShapeTreeClientConfiguration configuration) throws ShapeTreeException {
        if (clientMap.containsKey(configuration)) {
            return clientMap.get(configuration);
        }
        try {
            OkHttpClient client = buildClientFromConfiguration(configuration);
            clientMap.put(configuration, client);
            return client;
        } catch (Exception ex) {
            throw new ShapeTreeException(500, ex.getMessage());
        }
    }

    private static OkHttpClient buildClientFromConfiguration(ShapeTreeClientConfiguration configuration) throws NoSuchAlgorithmException, KeyManagementException {
        OkHttpClient.Builder clientBuilder = baseClient.newBuilder();
        if (Boolean.TRUE.equals(configuration.getUseValidation())) {
            clientBuilder.interceptors().add(new ValidatingShapeTreeInterceptor());
        }
        if (Boolean.TRUE.equals(configuration.getSkipSslValidation())) {
            // Install the all-trusting trust manager
            final SSLContext sslContext = SSLContext.getInstance("TLSv1.2");
            TrustManager[] trustAllCerts = getTrustAllCertsManager();
            sslContext.init(null, trustAllCerts, new java.security.SecureRandom());
            // Create an ssl socket factory with our all-trusting manager
            final SSLSocketFactory sslSocketFactory = sslContext.getSocketFactory();

            clientBuilder.sslSocketFactory(sslSocketFactory, (X509TrustManager)trustAllCerts[0])
                    .hostnameVerifier(getTrustAllHostnameVerifier());
        }
        return clientBuilder.build();
    }

    private static TrustManager[] getTrustAllCertsManager() {
        // Create a trust manager that does not validate certificate chains
        return new TrustManager[] {
                new X509TrustManager() {
                    @Override
                    public void checkClientTrusted(java.security.cert.X509Certificate[] chain, String authType) {
                        // All clients are trusted when SSL validation is skipped
                    }

                    @Override
                    public void checkServerTrusted(java.security.cert.X509Certificate[] chain, String authType) {
                        // All servers are trusted when SSL validation is skipped
                    }

                    @Override
                    public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                        return new java.security.cert.X509Certificate[]{};
                    }
                }
        };
    }

    private static HostnameVerifier getTrustAllHostnameVerifier() {
        return (hostname, session) -> true;
    }

}
