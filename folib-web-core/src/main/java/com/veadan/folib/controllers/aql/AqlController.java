package com.veadan.folib.controllers.aql;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.services.impl.FqlSearchService;
import com.veadan.folib.storage.search.SearchResults;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.inject.Inject;
import java.io.IOException;

/**
 * @author xuxinping
 */
@Controller
@RequestMapping("/api/fql")
@Api(value = "/api/fql")
public class AqlController extends BaseController {

//    @Inject
//    private AqlSearchService aqlSearchService;

    @Inject
    private FqlSearchService fqlSearchService;

    @ApiOperation(value = "Used to search for artifacts.", response = SearchResults.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('SEARCH_ARTIFACTS')")
    @GetMapping(produces = {MediaType.APPLICATION_JSON_VALUE})
    public ResponseEntity search(@RequestParam(name = "artifactName", required = true) String artifactName,
                                 @RequestParam(name = "regex", required = false) Boolean regex,
                                 @RequestParam(name = "storageId", required = false) String storageId,
                                 @RequestParam(name = "repositoryId", required = false) String repositoryId,
                                 @RequestParam(name = "beginDate", required = false) String beginDate,
                                 @RequestParam(name = "endDate", required = false) String endDate,
                                 @RequestParam(name = "sortField", required = false) String sortField,
                                 @RequestParam(name = "sortOrder", required = false) String sortOrder,
                                 @RequestParam(name = "limit", required = false) Integer limit,
                                 @RequestParam(name = "page", required = false) Integer page) throws IOException {
        SearchResults result = fqlSearchService.artifactQuery(regex, artifactName, storageId, repositoryId, beginDate, endDate, sortField, sortOrder, limit, page);
        return ResponseEntity.ok(result);
    }

}
