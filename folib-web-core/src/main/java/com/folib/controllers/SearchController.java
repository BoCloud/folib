package com.folib.controllers;

import com.folib.providers.search.SearchException;
import com.folib.services.ArtifactSearchService;
import com.folib.storage.search.SearchRequest;
import com.folib.storage.search.SearchResults;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.net.URLDecoder;

import io.swagger.annotations.*;
import org.apache.lucene.queryparser.classic.ParseException;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * @author 
 */
@Controller
@RequestMapping("/api/search")
@Api(description = "搜索组件控制器",tags = "搜索组件控制器")
public class SearchController
        extends BaseController
{

    @Inject
    ArtifactSearchService artifactSearchService;

    /**
     * Performs a search against the Lucene index of a specified repository,
     * or the Lucene indexes of all repositories.
     *
     * @param storageId
     * @param repositoryId
     * @param query
     * @return
     * @throws IOException
     * @throws ParseException
     */
    @ApiOperation(value = "Used to search for artifacts.", response = SearchResults.class)
    @ApiResponses(value = { @ApiResponse(code = 200, message = "") })
    @PreAuthorize("hasAuthority('SEARCH_ARTIFACTS')")
    @GetMapping(consumes = { MediaType.APPLICATION_OCTET_STREAM_VALUE,
                             MediaType.TEXT_PLAIN_VALUE },
                produces = { MediaType.APPLICATION_JSON_VALUE,
                             MediaType.TEXT_PLAIN_VALUE })
    public ResponseEntity search(@ApiParam(value = "The storageId", required = false)
                                 @RequestParam(name = "storageId", required = false) final String storageId,
                                 @ApiParam(value = "The repositoryId", required = false)
                                 @RequestParam(name = "repositoryId", required = false) final String repositoryId,
                                 @ApiParam(value = "The search query", required = true)
                                 @RequestParam(name = "q") final String query,
                                 HttpServletRequest request)
            throws IOException, SearchException
    {
        String accept = request.getHeader("accept");
        String q = URLDecoder.decode(query, "UTF-8");

        logger.info("[search] {}\n\taccept {}\n\tstorageId = {}\n\trepositoryId = {}",
                     q, accept, storageId, repositoryId);

        if (accept.equalsIgnoreCase(MediaType.TEXT_PLAIN_VALUE))
        {
            final SearchResults artifacts = getSearchResults(storageId, repositoryId, q);

            return ResponseEntity.ok(artifacts.toString());
        }
        else
        {
            // Apparently, the JSON root tag's name is based on the name of the object
            // which the Jersey method returns, hence this is "artifacts".
            @SuppressWarnings("UnnecessaryLocalVariable")
            final SearchResults artifacts = getSearchResults(storageId, repositoryId, q);

            return ResponseEntity.ok(artifacts);
        }
    }

    private SearchResults getSearchResults(String storageId,
                                           String repositoryId,
                                           String query)
            throws SearchException
    {
        return artifactSearchService.search(new SearchRequest(storageId,
                                                              repositoryId,
                                                              query));
    }

}
