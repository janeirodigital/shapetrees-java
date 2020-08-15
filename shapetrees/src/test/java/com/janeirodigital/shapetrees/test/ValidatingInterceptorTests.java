package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.helper.HttpClientHelper;
import lombok.SneakyThrows;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ValidatingInterceptorTests {

    @Test
    @DisplayName("Secure Client Test")
    @SneakyThrows
    void secureClient() {

        OkHttpClient client = HttpClientHelper.getClient();
        Request get = new Request.Builder()
                .url("https://solidproject.org/")
                .build();

        Response response = client.newCall(get).execute();
        assertEquals(200, response.code());
    }
}
