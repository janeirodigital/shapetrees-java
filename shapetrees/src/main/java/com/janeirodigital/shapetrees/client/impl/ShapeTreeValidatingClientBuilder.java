package com.janeirodigital.shapetrees.client.impl;

import com.janeirodigital.shapetrees.ShapeTreeEcosystem;
import com.janeirodigital.shapetrees.helper.HttpClientHelper;
import okhttp3.OkHttpClient;

public class ShapeTreeValidatingClientBuilder {

    private final ShapeTreeEcosystem ecosystem;
    private final boolean skipValidation;

    public ShapeTreeValidatingClientBuilder(ShapeTreeEcosystem ecosystem) {
        this(ecosystem, false);
    }

    public ShapeTreeValidatingClientBuilder(ShapeTreeEcosystem ecosystem, boolean skipValidation) {
        this.ecosystem = ecosystem;
        this.skipValidation = skipValidation;
    }

    public OkHttpClient get() {
        ValidatingShapeTreeInterceptor requestInterceptor = new ValidatingShapeTreeInterceptor(ecosystem);
        if (skipValidation) {
            return HttpClientHelper.getClient();
        } else {
            return HttpClientHelper.getClient(requestInterceptor);
        }
    }
}
