package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.ShapeTreeFactory;
import com.janeirodigital.shapetrees.client.ShapeTreeValidatingClientBuilder;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Slf4j
public class ShapeTreeParsingTests {

    private static final String rootPath = "https://ldp.local-ess.inrupt.com/aHR0cDovL2RldnNlcnZlcjozMDA4Mi9hdXRoL3JlYWxtcy9tYXN0ZXJiMzg4YmJlMV85ZjYzXzRlYmNfYmEzMF80MWY4ZmJjZmM0NTc/shapetree-testing/";
    private static final String token = "eyJraWQiOiJyc2ExIiwiYWxnIjoiUlMyNTYifQ.eyJzdWIiOiJodHRwczpcL1wvbGRwLmxvY2FsLWVzcy5pbnJ1cHQuY29tXC9hSFIwY0RvdkwyUmxkbk5sY25abGNqb3pNREE0TWk5aGRYUm9MM0psWVd4dGN5OXRZWE4wWlhKaU16ZzRZbUpsTVY4NVpqWXpYelJsWW1OZlltRXpNRjgwTVdZNFptSmpabU0wTlRjXC9wcm9maWxlXC9jYXJkI21lIiwiYXpwIjoiaHR0cDpcL1wvbG9jYWxob3N0OjQyMDAiLCJpc3MiOiJodHRwczpcL1wvb2lkYy5sb2NhbC1lc3MuaW5ydXB0LmNvbVwvIiwiY25mIjp7ImprdCI6Ik01RUlYX0hMbGxvazhOR3U2b0FURGtFaTJNNVFsMEt2TVV4c0pVOTEtSzgifSwiZXhwIjoxNTkzNjg2NTEzLCJpYXQiOjE1OTMwODE3MTMsImp0aSI6ImM3ZTM3NWIyLWViYzEtNDllYi05YjM4LTczNDg1NzdlM2JjZCJ9.3foE2Irffgd3H3KOHPpNSh5kk_egUL4fZ-TnIk5sR4Jh4SL5a8s9VniqDtMPmvLpqU2gOUBy-kPTNStuKqPzfhrkXXr3BOnvu-b5sP_bcObOGI8aafXNkfK2gSyvth2IsmoyhgHRU-6yIZVLO5KEk8sDqo0ot1BxUffu-2udoV4wcZx62je5PuSCRmja4HuvM_jH-mbEx-5dIh1acFSm7uFq34rTKeWFkDKQ8RCm22Pn890-XSQMwXYoVgKYWULdcXolFBa-75D8WqsKxIsDHs96nEeghLy7DZvkSunpKWQ7_Py-TWkFYtPj3mUcD0Zl1cVjhiHzw6GEzzwjEomGOQ";

    @SneakyThrows
    @Test
    @DisplayName("Parse Tree")
    void parseShapeTree() {
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/gh-flat-shapetree.jsonld#orgs"));
        ShapeTreeFactory.getShapeTreeStep(new URI("http://localhost:9999/static/gh-flat-shapetree.jsonld#issue"));
    }

    @SneakyThrows
    @Test
    void plantTest() {
        OkHttpClient client = new ShapeTreeValidatingClientBuilder(new MockEcosystem()).get();

        Request plantPost = new Request.Builder()
                .url(rootPath)
                .addHeader("Authorization", "Bearer " + token)
                .addHeader("Link", "<http://localhost:9999/static/gh-flat-shapetree.jsonld#orgs>; rel=\"ShapeTree\"")
                .addHeader("Link", "<http://www.w3.org/ns/ldp#Container>; rel=\"type\"")
                .addHeader("Slug", "Git-Orgs")
                .post(RequestBody.create(new byte[]{}))
                .build();

        Response response = client.newCall(plantPost).execute();

        assertEquals(201, response.code());
    }

    @AfterAll
    static void tearDownAll() {
        List<String> urls = new ArrayList<>();
        urls = SolidTestHelper.getRecursiveContainerContents("Bearer " + token, rootPath, urls);

        Collections.reverse(urls);
        for (String url : urls) {
            log.info("Deleting " + url);
            SolidTestHelper.deleteResource("Bearer " + token, url);
        }

    }
}
