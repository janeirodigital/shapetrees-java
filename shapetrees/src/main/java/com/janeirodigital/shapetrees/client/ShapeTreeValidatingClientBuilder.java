package com.janeirodigital.shapetrees.client;

import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.helper.HttpClientHelper;
import okhttp3.OkHttpClient;

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
