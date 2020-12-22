package com.janeirodigital.shapetrees.client.okhttp;

import com.janeirodigital.shapetrees.core.ShapeTreeResponse;
import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.client.okhttp.fixtures.DispatcherEntry;
import com.janeirodigital.shapetrees.client.okhttp.fixtures.RequestMatchingFixtureDispatcher;
import com.janeirodigital.shapetrees.core.vocabularies.ShapeTreeVocabulary;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.*;

import java.net.URI;
import java.util.List;
import java.util.Map;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class GitHubDeepTests extends BaseShapeTreeTest {

    public GitHubDeepTests() {
        super();
    }

    private static RequestMatchingFixtureDispatcher dispatcher = null;

    @BeforeAll
    static void beforeAll() {
        dispatcher = new RequestMatchingFixtureDispatcher(List.of(
                new DispatcherEntry(List.of("shapetrees/github-deep-shapetree-ttl"), "GET", "/static/shapetrees/github-deep/shapetree", null),
                new DispatcherEntry(List.of("schemas/github-shex"), "GET", "/static/shex/github/shex", null),
                new DispatcherEntry(List.of("githubDeep/data-container"), "GET", "/ldp/data/", null),
                new DispatcherEntry(List.of("errors/404"), "GET", "/ldp/data/?ext=shapetree", null),
                new DispatcherEntry(List.of("errors/404"), "GET", "/ldp/data/Git", null),
                new DispatcherEntry(List.of("githubDeep/git-container"), "GET", "/ldp/data/Git/", null),
                new DispatcherEntry(List.of("errors/404", "githubDeep/git-container-metadata"), "GET", "/ldp/data/Git/?ext=shapetree", null),
                new DispatcherEntry(List.of("githubDeep/create-git-container-response"), "POST", "/ldp/data/", Map.of(HttpHeaders.SLUG.getValue(), List.of("Git"))),
                new DispatcherEntry(List.of("githubDeep/git-metadata-update-response"), "PUT", "/ldp/data/Git/?ext=shapetree",null),

                new DispatcherEntry(List.of("errors/404"), "GET", "/ldp/data/Git/users", null),
                new DispatcherEntry(List.of("githubDeep/create-git_users-container-response"), "POST", "/ldp/data/Git/", Map.of(HttpHeaders.SLUG.getValue(), List.of("users"))),
                new DispatcherEntry(List.of("githubDeep/git-users-container"), "GET", "/ldp/data/Git/users/", null),
                new DispatcherEntry(List.of("githubDeep/git-users-metadata-update-response"), "PUT", "/ldp/data/Git/users/?ext=shapetree",null),
                new DispatcherEntry(List.of("errors/404", "githubDeep/git-users-container-metadata"), "GET", "/ldp/data/Git/users/?ext=shapetree", null),

                new DispatcherEntry(List.of("errors/404"), "GET", "/ldp/data/Git/repos", null),
                new DispatcherEntry(List.of("githubDeep/create-git_repos-container-response"), "POST", "/ldp/data/Git/", Map.of(HttpHeaders.SLUG.getValue(), List.of("repos"))),
                new DispatcherEntry(List.of("githubDeep/git-repos-container"), "GET", "/ldp/data/Git/repos/", null),
                new DispatcherEntry(List.of("githubDeep/git-repos-metadata-update-response"), "PUT", "/ldp/data/Git/repos/?ext=shapetree",null),
                new DispatcherEntry(List.of("errors/404", "githubDeep/git-repos-container-metadata"), "GET", "/ldp/data/Git/repos/?ext=shapetree", null),

                new DispatcherEntry(List.of("githubDeep/git-repos-jd-create-response"), "PUT", "/ldp/data/Git/repos/janeirodigtal/", null),
                new DispatcherEntry(List.of("errors/404"), "GET", "/ldp/data/Git/repos/janeirodigital", null),
                new DispatcherEntry(List.of("githubDeep/create-git_repos_jd-container-response"), "POST", "/ldp/data/Git/repos/", Map.of(HttpHeaders.SLUG.getValue(), List.of("janeirodigital"))),
                new DispatcherEntry(List.of("githubDeep/git-repos-jd-container"), "GET", "/ldp/data/Git/repos/janeirodigital/", null),
                new DispatcherEntry(List.of("githubDeep/git-repos-jd-metadata-update-response"), "PUT", "/ldp/data/Git/repos/janeirodigital/?ext=shapetree",null),
                new DispatcherEntry(List.of("errors/404", "githubDeep/git-repos-janeirodigital-container-metadata"), "GET", "/ldp/data/Git/repos/janeirodigital/?ext=shapetree", null)

        ));
    }

    @Test
    @Order(1)
    @DisplayName("Create /Git/")
    @SneakyThrows
    void plantGitRootCreatesStatics() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        this.shapeTreeClient.plantShapeTree(this.context,
                getURI(server,"/ldp/data/"),
                List.of(getURI(server,"/static/shapetrees/github-deep/shapetree#root")),
                null,
                null,
                "Git",
                null,
                TEXT_TURTLE);

        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/Git/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), getURI(server, "/ldp/data/Git/"));
        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/Git/"), new URI(ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE), getURI(server, "/static/shapetrees/github-deep/shapetree#root"));
        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/Git/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE), getURI(server, "/static/shapetrees/github-deep/shapetree#root"));

        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/Git/repos/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), getURI(server, "/ldp/data/Git/"));
        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/Git/repos/"), new URI(ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE), getURI(server, "/static/shapetrees/github-deep/shapetree#root"));
        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/Git/repos/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE), getURI(server, "/static/shapetrees/github-deep/shapetree#repos"));

        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/Git/users/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), getURI(server, "/ldp/data/Git/"));
        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/Git/users/"), new URI(ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE), getURI(server, "/static/shapetrees/github-deep/shapetree#root"));
        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/Git/users/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE), getURI(server, "/static/shapetrees/github-deep/shapetree#users"));
    }

    @Test
    @Order(2)
    @SneakyThrows
    void createRepo() {
        MockWebServer server = new MockWebServer();
        server.setDispatcher(dispatcher);

        ShapeTreeResponse response = this.shapeTreeClient.createDataInstance(this.context,
                getURI(server, "/ldp/data/Git/repos/"),
                "#janeirodigital",
                getURI(server, "/static/shapetrees/github-deep/shapetree#org"),
                "janeirodigital",
                true,
                getOrgTtl("janeirodigital"),
                "text/turtle");
        Assertions.assertEquals(201, response.getStatusCode());

        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/Git/repos/janeirodigital/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE_INSTANCE_ROOT), getURI(server, "/ldp/data/Git/"));
        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/Git/repos/janeirodigital/"), new URI(ShapeTreeVocabulary.HAS_ROOT_SHAPE_TREE), getURI(server, "/static/shapetrees/github-deep/shapetree#root"));
        ensureExistsHasMetadataWithPredicateValue(getURI(server, "/ldp/data/Git/repos/janeirodigital/"), new URI(ShapeTreeVocabulary.HAS_SHAPE_TREE), getURI(server, "/static/shapetrees/github-deep/shapetree#org"));
    }

    private String getOrgTtl(String orgName) {
        return "PREFIX gdt: <http://github.example/dt#>\n" +
                "PREFIX gh: <http://github.example/ns#>\n" +
                "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" +
                "\n" +
                "<#janeirodigital>\n" +
                "  gh:url <http://github.example/user/" + orgName + "> ;\n" +
                "  gh:id 573478 ;\n" +
                "  gh:node_id \"MDQ6VXNlcjU3MzQ3OA==\" ;\n" +
                "\n" +
                "  gh:avatar_url <https://avatars3.githubusercontent.com/u/573478?v=4> ;\n" +
                "  gh:events_url \"events{/privacy}\"^^gdt:urlTemplate ;\n" +
                "  gh:followers_url <followers> ;\n" +
                "  gh:following_url \"following{/other_user}\"^^gdt:urlTemplate ;\n" +
                "  gh:gists_url \"gists{/gist_id}\"^^gdt:urlTemplate ;\n" +
                "  gh:gravatar_id \"\" ;\n" +
                "  gh:html_url \"https://github.com/" + orgName + "\"^^gdt:urlTemplate ;\n" +
                "  gh:login \"" + orgName + "\" ;\n" +
                "  gh:organizations_url <orgs> ;\n" +
                "  gh:received_events_url <received_events> ;\n" +
                "  gh:repos_url <repos> ;\n" +
                "  gh:site_admin false ;\n" +
                "  gh:starred_url \"starred{/owner}{/repo}\"^^gdt:urlTemplate ;\n" +
                "  gh:subscriptions_url <subscriptions> ;\n" +
                "  gh:type \"User\" .\n" +
                "\n";
    }
}
