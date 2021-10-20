package com.janeirodigital.shapetrees.tests;

import com.janeirodigital.shapetrees.core.DocumentResponse;
import com.janeirodigital.shapetrees.core.contentloaders.DocumentLoaderManager;
import com.janeirodigital.shapetrees.core.contentloaders.ExternalDocumentLoader;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.SneakyThrows;
import org.junit.jupiter.api.*;

import java.net.URI;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class DocumentLoaderTests {

    @BeforeAll @AfterAll
    static void clearDocumentManager() {
        DocumentLoaderManager.setLoader(null);
    }

    @Test
    @Order(1)
    @DisplayName("Fail to get missing document loader")
    @SneakyThrows
    void failToGetMissingDocumentLoader() {
        Assertions.assertThrows(ShapeTreeException.class, () -> {
            DocumentLoaderManager.getLoader();
        });
    }

    @Test
    @Order(2)
    @DisplayName("Get document loader")
    @SneakyThrows
    void getDocumentLoader() {
        DocumentLoaderManager.setLoader(new TestDocumentLoader());
        Assertions.assertNotNull(DocumentLoaderManager.getLoader());
    }

}

class TestDocumentLoader implements ExternalDocumentLoader {
    public DocumentResponse loadExternalDocument(URI resourceURI) throws ShapeTreeException {
        return new DocumentResponse(null, null, 200);
    }
}
