package com.veadan.folib.controllers.configuration;

import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.controllers.support.NumberOfConnectionsEntityBody;
import com.veadan.folib.controllers.support.PoolStatsEntityBody;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.RepositoryMapping;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

import java.io.IOException;

import org.apache.http.pool.PoolStats;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

/**
 * @author korest
 * @author Pablo Tirado
 */
@Controller
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/configuration/proxy/connection-pool")
@Api(value = "/api/configuration/proxy/connection-pool")
public class HttpConnectionPoolConfigurationManagementController
        extends BaseConfigurationController
{
    private final ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    public HttpConnectionPoolConfigurationManagementController(ConfigurationManagementService configurationManagementService,
                                                               ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService)
    {
        super(configurationManagementService);
        this.proxyRepositoryConnectionPoolConfigurationService = proxyRepositoryConnectionPoolConfigurationService;
    }

    @ApiOperation(value = "Update number of pool connections pool for proxy repository")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Number of pool connections for proxy repository was updated successfully."),
                            @ApiResponse(code = 400, message = "The proxy repository has no associated remote repository."),
                            @ApiResponse(code = 404, message = "The (storage/repository) does not exist!") })
    @PutMapping(value = "{storageId}/{repositoryId}/{numberOfConnections}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity setNumberOfConnectionsForProxyRepository(@RepositoryMapping Repository repository,
                                                                   @PathVariable(value = "numberOfConnections") int numberOfConnections,
                                                                   @RequestHeader(HttpHeaders.ACCEPT) String accept) throws IOException
    { 
        final RepositoryData immutableRepository = (RepositoryData) repository;
        final String storageId = immutableRepository.getStorage().getId();
        final String repositoryId = immutableRepository.getId();

        if (immutableRepository.getRemoteRepository() == null)
        {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                                 .body(getResponseEntityBody("The proxy repository has no associated remote repository.", accept));
        }

        configurationManagementService.setProxyRepositoryMaxConnections(storageId, repositoryId, numberOfConnections);
        proxyRepositoryConnectionPoolConfigurationService.setMaxPerRepository(
                immutableRepository.getRemoteRepository().getUrl(),
                numberOfConnections);

        String message = "Number of pool connections for repository was updated successfully.";

        return ResponseEntity.ok(getResponseEntityBody(message, accept));
    }

    @ApiOperation(value = "Get proxy repository pool stats")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "Proxy repository pool stats where retrieved."),
                            @ApiResponse(code = 400,
                                         message = "Repository doesn't have remote repository!"),
                            @ApiResponse(code = 404,
                                    message = "The (storage/repository) does not exist!") })
    @GetMapping(value = "{storageId}/{repositoryId}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getPoolStatsForProxyRepository(@RepositoryMapping Repository repository,
                                                         @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        final RepositoryData immutableRepository = (RepositoryData) repository;
        if (immutableRepository.getRemoteRepository() == null)
        {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                                 .body(getResponseEntityBody("Repository doesn't have remote repository!", accept));
        }

        PoolStats poolStats = proxyRepositoryConnectionPoolConfigurationService
                                      .getPoolStats(immutableRepository.getRemoteRepository()
                                                                       .getUrl());

        return ResponseEntity.ok(getPoolStatsEntityBody(poolStats, accept));
    }

    @ApiOperation(value = "Update default number of connections for proxy repository")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "Default number of connections for proxy repository was updated successfully."),
                            @ApiResponse(code = 400,
                                         message = "Could not update default number of connections for proxy repository.") })
    @PutMapping(value = "default/{numberOfConnections}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity setDefaultNumberOfConnectionsForProxyRepository(@PathVariable(value = "numberOfConnections")
                                                                                  int numberOfConnections,
                                                                          @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        proxyRepositoryConnectionPoolConfigurationService.setDefaultMaxPerRepository(numberOfConnections);
        String message = "Default number of connections for proxy repository was updated successfully.";
        return ResponseEntity.ok(getResponseEntityBody(message, accept));
    }

    @ApiOperation(value = "Get default number of connections for proxy repository")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "Default number of connections was retrieved."),
                            @ApiResponse(code = 400,
                                         message = "Could not get default number of connections for proxy repository.") })
    @GetMapping(value = "default-number",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getDefaultNumberOfConnectionsForProxyRepository(@RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        int defaultNumber = proxyRepositoryConnectionPoolConfigurationService.getDefaultMaxPerRepository();
        return ResponseEntity.ok(getNumberOfConnectionsEntityBody(defaultNumber, accept));
    }

    @ApiOperation(value = "Update max number of connections for proxy repository")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "Max number of connections for proxy repository was updated successfully."),
                            @ApiResponse(code = 400,
                                         message = "Could not update max number of connections for proxy repository.") })
    @PutMapping(value = "max/{numberOfConnections}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity setMaxNumberOfConnectionsForProxyRepository(@PathVariable(value = "numberOfConnections")
                                                                              int numberOfConnections,
                                                                      @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        proxyRepositoryConnectionPoolConfigurationService.setMaxTotal(numberOfConnections);
        String message = "Max number of connections for proxy repository was updated successfully.";
        return ResponseEntity.ok(getResponseEntityBody(message, accept));
    }

    @ApiOperation(value = "Get max number of connections for proxy repository")
    @ApiResponses(value = { @ApiResponse(code = 200,
                                         message = "Max number of connections for proxy repository was retrieved."),
                            @ApiResponse(code = 400,
                                         message = "Could not get max number of connections for proxy repository.") })
    @GetMapping(produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity getMaxNumberOfConnectionsForProxyRepository(@RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        int maxNumberOfConnections = proxyRepositoryConnectionPoolConfigurationService.getTotalStats()
                                                                                      .getMax();
        return ResponseEntity.ok(getNumberOfConnectionsEntityBody(maxNumberOfConnections, accept));
    }

    private Object getNumberOfConnectionsEntityBody(int numberOfConnections, String accept)
    {
        if (MediaType.APPLICATION_JSON_VALUE.equals(accept))
        {
            return new NumberOfConnectionsEntityBody(numberOfConnections);
        }
        else
        {
            return String.valueOf(numberOfConnections);
        }
    }

    private Object getPoolStatsEntityBody(PoolStats poolStats, String accept)
    {
        if (MediaType.APPLICATION_JSON_VALUE.equals(accept))
        {
            return new PoolStatsEntityBody(poolStats);
        }
        else
        {
            return String.valueOf(poolStats);
        }
    }

}
