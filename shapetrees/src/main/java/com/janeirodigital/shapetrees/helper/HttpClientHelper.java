package com.janeirodigital.shapetrees.helper;

import okhttp3.Interceptor;
import okhttp3.OkHttpClient;

import javax.net.ssl.*;
import java.security.cert.CertificateException;


public class HttpClientHelper {


    public static OkHttpClient getClient() {

        if (shouldIgnoreSSL()) {
            return getUnsafeOkHttpClient(null);
        } else {
            return new OkHttpClient();
        }
    }

    public static OkHttpClient getClient(Interceptor interceptor) {
        if (shouldIgnoreSSL()) {
            return getUnsafeOkHttpClient(interceptor);
        } else {
            OkHttpClient client = new OkHttpClient.Builder()
                    .addInterceptor(interceptor)
                    .build();
            return client;
        }
    }

    private static OkHttpClient getUnsafeOkHttpClient(Interceptor interceptor) {
        try {
            // Create a trust manager that does not validate certificate chains
            final TrustManager[] trustAllCerts = new TrustManager[] {
                    new X509TrustManager() {
                        @Override
                        public void checkClientTrusted(java.security.cert.X509Certificate[] chain, String authType) throws CertificateException {
                        }

                        @Override
                        public void checkServerTrusted(java.security.cert.X509Certificate[] chain, String authType) throws CertificateException {
                        }

                        @Override
                        public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                            return new java.security.cert.X509Certificate[]{};
                        }
                    }
            };

            // Install the all-trusting trust manager
            final SSLContext sslContext = SSLContext.getInstance("SSL");
            sslContext.init(null, trustAllCerts, new java.security.SecureRandom());
            // Create an ssl socket factory with our all-trusting manager
            final SSLSocketFactory sslSocketFactory = sslContext.getSocketFactory();

            HostnameVerifier trustingHostNameVerifier = new HostnameVerifier() {
                @Override
                public boolean verify(String hostname, SSLSession session) {
                    return true;
                }
            };

            OkHttpClient okHttpClient;

            if (interceptor != null) {
                okHttpClient = new OkHttpClient.Builder()
                        .sslSocketFactory(sslSocketFactory, (X509TrustManager)trustAllCerts[0])
                        .hostnameVerifier(trustingHostNameVerifier)
                        .addInterceptor(interceptor)
                        .build();
            } else {
                okHttpClient = new OkHttpClient.Builder()
                        .sslSocketFactory(sslSocketFactory, (X509TrustManager)trustAllCerts[0])
                        .hostnameVerifier(trustingHostNameVerifier)
                        .build();
            }

            return okHttpClient;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static boolean shouldIgnoreSSL() {
        boolean ignoreSSL = false;
        if (System.getenv("SHAPETREE_IGNORE_SSL") != null) {
            ignoreSSL = Boolean.parseBoolean(System.getenv("SHAPETREE_IGNORE_SSL"));
        }
        return ignoreSSL;
    }
}
