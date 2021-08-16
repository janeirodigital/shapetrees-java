package com.janeirodigital.shapetrees.client.http;

import lombok.Getter;
import lombok.Setter;
import lombok.Synchronized;

public abstract class AbstractHttpClientFactory {
    @Setter(onMethod_={@Synchronized}) @Getter(onMethod_={@Synchronized})
    private static HttpClientFactory factory;
}
