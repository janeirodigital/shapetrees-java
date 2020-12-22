package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.client.core.ShapeTreeClientConfiguration;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import okhttp3.OkHttpClient;

import javax.net.ssl.*;
import java.util.concurrent.ConcurrentHashMap;

public class ShapeTreeHttpClientHolder {

    private static final OkHttpClient baseClient = new OkHttpClient();
    private static final ConcurrentHashMap<ShapeTreeClientConfiguration, OkHttpClient> clientMap = new ConcurrentHashMap<>();

    public static synchronized OkHttpClient getForConfig(ShapeTreeClientConfiguration configuration) throws ShapeTreeException {
        if (clientMap.containsKey(configuration)) {
            return clientMap.get(configuration);
        }

        OkHttpClient client = buildClientFromConfiguration(configuration);
        clientMap.put(configuration, client);
        return client;
    }

    private static OkHttpClient buildClientFromConfiguration(ShapeTreeClientConfiguration configuration) {
        OkHttpClient.Builder clientBuilder = baseClient.newBuilder();
        if (configuration.getUseValidation()) {
            clientBuilder.interceptors().add(new ValidatingShapeTreeInterceptor());
        }
        if (configuration.getSkipSslValidation()) {
            try {
                // Install the all-trusting trust manager
                final SSLContext sslContext = SSLContext.getInstance("SSL");
                TrustManager[] trustAllCerts = getTrustAllCertsManager();
                sslContext.init(null, trustAllCerts, new java.security.SecureRandom());
                // Create an ssl socket factory with our all-trusting manager
                final SSLSocketFactory sslSocketFactory = sslContext.getSocketFactory();

                clientBuilder.sslSocketFactory(sslSocketFactory, (X509TrustManager)trustAllCerts[0])
                        .hostnameVerifier(getTrustAllHostnameVerifier());
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        return clientBuilder.build();
    }

    private static TrustManager[] getTrustAllCertsManager() {
        // Create a trust manager that does not validate certificate chains
        return new TrustManager[] {
                new X509TrustManager() {
                    @Override
                    public void checkClientTrusted(java.security.cert.X509Certificate[] chain, String authType) {
                    }

                    @Override
                    public void checkServerTrusted(java.security.cert.X509Certificate[] chain, String authType) {
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
