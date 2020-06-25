package com.janeirodigital.shapetrees.helper;

import lombok.SneakyThrows;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.TrustStrategy;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.BasicHttpClientConnectionManager;

import javax.net.ssl.SSLContext;
import java.security.cert.X509Certificate;

public class HttpClientHelper {
    @SneakyThrows
    public static SSLConnectionSocketFactory getIgnoreSSLSocketFactory() {
        TrustStrategy acceptingTrustStrategy = (X509Certificate[] chain, String authType) -> true;
        SSLContext sslContext = org.apache.http.ssl.SSLContexts.custom()
                .loadTrustMaterial(null, acceptingTrustStrategy)
                .build();

        SSLConnectionSocketFactory sslsf = new SSLConnectionSocketFactory(sslContext,
                NoopHostnameVerifier.INSTANCE);

        return sslsf;
    }

    public static BasicHttpClientConnectionManager getIgnoreSSLConnectionManager() {
        Registry<ConnectionSocketFactory> socketFactoryRegistry =
                RegistryBuilder.<ConnectionSocketFactory> create()
                        .register("https", getIgnoreSSLSocketFactory())
                        .register("http", new PlainConnectionSocketFactory())
                        .build();
        BasicHttpClientConnectionManager connectionManager =
                new BasicHttpClientConnectionManager(socketFactoryRegistry);
        return connectionManager;
    }

    public static CloseableHttpClient getClient(Boolean ignoreSSL) {
        if (ignoreSSL) {
            return HttpClients.custom().setSSLSocketFactory(getIgnoreSSLSocketFactory())
                    .setConnectionManager(getIgnoreSSLConnectionManager()).build();
        } else {
            return HttpClients.createDefault();
        }
    }
}
