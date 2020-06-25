package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import org.apache.http.client.HttpClient;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;

public class ShapeTreeValidatingClientBuilder {

    private final ShapeTreeEcosystem ecosystem;

    public ShapeTreeValidatingClientBuilder(ShapeTreeEcosystem ecosystem) {
        this.ecosystem = ecosystem;
    }

    public HttpClient get() {
        ValidatingShapeTreeInterceptor requestInterceptor = new ValidatingShapeTreeInterceptor(ecosystem);
        CloseableHttpClient httpClient = HttpClients
                .custom().
                .addInterceptorFirst(requestInterceptor)
                .add
                .build();

        return httpClient;
    }
}
