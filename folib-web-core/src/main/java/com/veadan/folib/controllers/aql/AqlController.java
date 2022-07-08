package com.veadan.folib.controllers.aql;

import com.veadan.folib.services.impl.FqlSearchService;
import com.veadan.folib.storage.search.SearchResults;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.domain.ArtifactEntity;

import javax.inject.Inject;
import java.io.IOException;

import io.swagger.annotations.*;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * @author xuxinping
 *
 */
@Controller
@RequestMapping("/api/fql")
@Api(value = "/api/fql")
public class AqlController extends BaseController
{

//    @Inject
//    private AqlSearchService aqlSearchService;

    @Inject
    private FqlSearchService fqlSearchService;

    @ApiOperation(value = "Used to search for artifacts.", response = SearchResults.class)
    @ApiResponses(value = { @ApiResponse(code = 200, message = "OK") })
    @PreAuthorize("hasAuthority('SEARCH_ARTIFACTS')")
    @GetMapping(produces = { MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity search(@RequestParam(name = "artifactName", required = true) String artifactName,
                                 @RequestParam(name = "storageId", required = false) String storageId,
                                 @RequestParam(name = "repositoryId", required = false) String repositoryId,
                                 @RequestParam(name = "limit", required = true) int limit,
                                 @RequestParam(name = "page", required = true) int page) throws IOException {
        SearchResults result = fqlSearchService.artfactQuery(artifactName, storageId, repositoryId, limit, page);

        return ResponseEntity.ok(result);
    }

}
