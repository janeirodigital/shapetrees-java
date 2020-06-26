package com.janeirodigital.shapetrees.helper;

import okhttp3.Interceptor;
import okhttp3.OkHttpClient;

import javax.net.ssl.*;
import java.security.cert.CertificateException;
import java.util.List;


public class HttpClientHelper {

    public static OkHttpClient getClient() {
        return new OkHttpClient();
    }

    public static OkHttpClient getClient(Boolean ignoreSSL) {
        if (ignoreSSL) {
            return getUnsafeOkHttpClient(null);
        } else {
            return new OkHttpClient();
        }
    }

    public static OkHttpClient getClient(Boolean ignoreSSL, List<Interceptor> interceptorList) {
        if (ignoreSSL) {
            return getUnsafeOkHttpClient(interceptorList);
        } else {
            OkHttpClient client = new OkHttpClient();
            client.interceptors().addAll(interceptorList);
            return client;
        }
    }

    private static OkHttpClient getUnsafeOkHttpClient(List<Interceptor> interceptorList) {
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
                            return null;
                        }
                    }
            };

            // Install the all-trusting trust manager
            final SSLContext sslContext = SSLContext.getInstance("SSL");
            sslContext.init(null, trustAllCerts, new java.security.SecureRandom());
            // Create an ssl socket factory with our all-trusting manager
            final SSLSocketFactory sslSocketFactory = sslContext.getSocketFactory();

            OkHttpClient okHttpClient = new OkHttpClient.Builder()
                    .sslSocketFactory(sslSocketFactory)
                    .hostnameVerifier(new HostnameVerifier() {
                        @Override
                        public boolean verify(String hostname, SSLSession session) {
                            return true;
                        }
                    }).build();
            if (interceptorList != null) {
                okHttpClient.interceptors().addAll(interceptorList);
            }

            return okHttpClient;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
