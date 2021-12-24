// Corresponding shapetrees-java package: com.janeirodigital.shapetrees.tests
import { DocumentResponse } from '@shapetrees/core/src/DocumentResponse';
import { DocumentLoaderManager } from '@shapetrees/core/src/contentloaders/DocumentLoaderManager';
import { ExternalDocumentLoader } from '@shapetrees/core/src/contentloaders/ExternalDocumentLoader';
import { ShapeTreeException } from '@shapetrees/core/src/exceptions/ShapeTreeException';

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class DocumentLoaderTests {

  // @BeforeAll, @AfterAll
  static clearDocumentManager(): void {
    DocumentLoaderManager.setLoader(null);
  }

  // @Test, @Order(1), @DisplayName("Fail to get missing document loader"), @SneakyThrows
  failToGetMissingDocumentLoader(): void {
    Assertions.assertThrows(ShapeTreeException.class, () -> {
      DocumentLoaderManager.getLoader();
    });
  }

  // @Test, @Order(2), @DisplayName("Get document loader"), @SneakyThrows
  getDocumentLoader(): void {
    DocumentLoaderManager.setLoader(new TestDocumentLoader());
    Assertions.assertNotNull(DocumentLoaderManager.getLoader());
  }
}

class TestDocumentLoader implements ExternalDocumentLoader {

  public loadExternalDocument(resourceURL: URL): DocumentResponse /* throws ShapeTreeException */ {
    return new DocumentResponse(null, null, 200);
  }
}
