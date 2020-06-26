package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.helper.HttpClientHelper;
import okhttp3.Interceptor;
import okhttp3.OkHttpClient;
import org.apache.http.client.HttpClient;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;

import java.util.ArrayList;
import java.util.List;

public class ShapeTreeValidatingClientBuilder {

    private final ShapeTreeEcosystem ecosystem;

    public ShapeTreeValidatingClientBuilder(ShapeTreeEcosystem ecosystem) {
        this.ecosystem = ecosystem;
    }

    public OkHttpClient get() {
        ValidatingShapeTreeInterceptor requestInterceptor = new ValidatingShapeTreeInterceptor(ecosystem);
        return HttpClientHelper.getClient(true, requestInterceptor);
    }
}
